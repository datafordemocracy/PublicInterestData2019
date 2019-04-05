# Public Interest Data Lab
# Example of visualization and modeling of an outcome
# March 2019


# ..........................................................................................

# load libraries ----
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(car) # for Anova function
library(stargazer) # for table of results
library(MASS) # for ordered logit, negative binomial models
library(survival) # for duration/survival model


# ..........................................................................................

# read in files ----
# read data from encrypted volume and data dictionary
setwd("/Volumes/PIDL19")
dss <- readRDS("dss.rds")
# created in data_clean.R


# Example analysis: removals
# ..........................................................................................

# Create a data frame to generate a graph ----
# subset data 
# children in foster care; only whte, black, multirace; only removal variables and race
dss_remove <- dss %>% 
  filter(fc_enter == "Yes") %>% 
  filter(race %in% c("White", "Black", "Multi-Race")) %>% 
  dplyr::select(race, remove_physabuse:remove_house)

# drop unused race levels, and recode reasons
dss_remove$race <- droplevels(dss_remove$race)
var <- c(names(dss)[100:114])
dss_remove <- dss_remove %>% 
  mutate_at(var, as.numeric) %>% 
  mutate_at(var, list(~ dplyr::recode(., `1` = 0L, `2` = 1L)))

dss_remove$gender <- droplevels(dss_remove$gender)

# reshape to long
remove <- dss_remove %>% 
  gather(reason, count, -race) %>% 
  group_by(race, reason) %>% 
  summarize(tot = sum(count))

# add total number of children in each racial category to data frame
remove_race <- dss_remove %>% count(race)
remove <- left_join(remove, remove_race, by = "race")
remove <- remove %>% 
  mutate(prop = tot/n)

# make reason a factor and order by prop
remove <- remove %>% 
  mutate(reason = factor(reason),
         reason = fct_reorder(reason, prop))


# ..........................................................................................

# Generate a figure of reasons ---
ggplot(remove, aes(x = reason, y = prop, fill = race)) + 
  geom_col(width=0.9, position=position_dodge(.75)) +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(8,6,4)]) +
  coord_flip() +
  labs(title = "Reasons for Removal from Home", subtite = "By Race", 
       y = "Proportion of Children Removed for Reason", x = "Reason for Removal")
# needs some work, but it's a start
#   should get rid of unused reasons, 
#   should rename reasons to remove "remove",
#   should rename legend (e.g., Race of Child)
 
# get p-values for differences to include in graph
chisq.test(dss_remove$remove_neglect, dss_remove$race, simulate.p.value = TRUE)
# because many of the cell counts are quite small, the distributional approximations can be poor; 
# simulating the p-values is one way of accounting for this (these p-values will generally be larger);
# another approach is to use a Fisher's exact test
fisher.test(dss_remove$remove_neglect, dss_remove$race)
# I don't do it here, but once I have the p-values, I could add them as text to the figure


# ..........................................................................................

# Generate a model for a reason ----
# re-create subsetted data frame for analysis (but keep all vars), recode age
dss_remove <- dss %>% 
  filter(fc_enter == "Yes") %>% 
  filter(race %in% c("White", "Black", "Multi-Race")) %>% 
  mutate(age_rem = interval(start = dob, end = remove_currdate) /
           duration(num = 1, units = "years"))

# 1. most basic model: race, age, gender
house1 <- glm(remove_house ~ race + age_rem + gender, 
              data = dss_remove, family = "binomial")
# model coefficients and summary stats
summary(house1)
# anova/F-test for variables -- useful to see joint effect of factor
# with Anova (from car), I can make an explict call to the type 3 test (which is what I want)
Anova(house1, type = 3) 

# 2. adding polynomial for age
house2 <- glm(remove_house ~ race + poly(age_rem, 2) + gender, 
              data = dss_remove, family = "binomial")
summary(house2)
Anova(house2, type = 3)

# is model with age-squared "better"?
# AIC comparison
AIC(house1)
AIC(house2)
# lower values are better -- so, house 1 (without age-squared) fits better

# anova test of nested models: does fuller model improve fit over reduced model?
anova(house2, house1, test = "Chisq")
# no, the null is that the fit equally well, and we have no evidence against the null
# so reduced model is preferred

# 3. adding family structure (reduced to single/two parents)
dss_remove <- dss_remove %>% 
  mutate(care_structure2 = fct_recode(care_structure,
                                      "Single" = "Single mom",
                                      "Single" = "Single dad",
                                      "Dual" = "Married couple",
                                      "Dual" = "Unmarried couple"))
house3 <- glm(remove_house ~ race + age_rem + gender + care_structure2, 
              data = dss_remove, family = "binomial")
summary(house3)
Anova(house3, type = 3)

# 4. adding numref
house4 <- glm(remove_house ~ race + age_rem + gender + care_structure2 + numref, 
              data = dss_remove, family = "binomial")
summary(house4)
Anova(house4, type = 3)

# 5. adding ever_find
house5 <- glm(remove_house ~ race + age_rem + gender + care_structure2 + numref + ever_find, 
              data = dss_remove, family = "binomial")
summary(house5)
Anova(house5, type = 3)

# All together
stargazer(house1, house2, house3, house4, house5, 
          type = "text", star.cutoffs = c(0.25, 0.1, 0.05))
# stargazer allows a great deal of customization and formatting
# any tables we add to the report we will generate with stargazer as type = "latex"; 
# type = "text" is great for seeing a quick combination of results in the console


# Generate interactive models  ----
# race by care_structure2
house6 <- glm(remove_house ~ race*care_structure2 + age_rem + gender + numref + ever_find, 
              data = dss_remove, family = "binomial")
summary(house6)
Anova(house6, type = 3)

# race by ever_find
house7 <- glm(remove_house ~ race*ever_find + age_rem + gender + numref + care_structure2, 
              data = dss_remove, family = "binomial")
summary(house7)
Anova(house7, type = 3)

# race by numref
house8 <- glm(remove_house ~ race*numref + age_rem + gender + ever_find + care_structure2, 
              data = dss_remove, family = "binomial")
summary(house8)
Anova(house8, type = 3)


# ..........................................................................................

# Other model syntax, briefly  ----
# 1. ordinal logit
find1 <- polr(ever_find ~ race_ethn + age_ref1 + gender + numref, 
              data = dss, Hess = TRUE)
summary(find1)
Anova(find1, type = 3)
# polr doesn't generate p-values for coefficients; 
# but you can create them in if you really want to
find1.coef <- data.frame(coef(summary(find1)))
find1.coef$pval = round((pnorm(abs(find1.coef$t.value), lower.tail = FALSE) * 2),2)
find1.coef


# 2. count/negative binomial
# see https://data.library.virginia.edu/getting-started-with-negative-binomial-regression-modeling/
# create number of allegations
var <- c(names(dss)[9:13])
dss <- dss %>% 
  mutate_at(var, as.numeric) %>% 
  mutate_at(var, list(~ dplyr::recode(., `1` = 0L, `2` = 1L)))
dss <- dss %>% 
  mutate(allege_sum = reduce(dplyr::select(., c(var)), `+`))
table(dss$race, dss$allege_sum)


allege1 <- glm.nb(allege_sum ~ race_ethn + age_ref1 + gender + numref,
                  data = dss)
summary(allege1)
Anova(allege1, type = 3)


# 3. duration/survival
# binary indicator for whether the child is still in the system
dss_remove <- dss_remove %>% 
  mutate(fc_exit = if_else(is.na(discharge_date), 0, 1)) # 1 if exit
query_date <- "2018-12-31 12:00:00 UTC" # data retrieval data for clients still in care

dss_remove$duration <- ifelse((dss_remove$fc_exit==0), 
                      difftime(query_date, dss_remove$remove_currdate, units="weeks"), 
                      difftime(dss_remove$discharge_date, dss_remove$remove_currdate, units="weeks"))

# Estimated survival function: race only
S <- Surv(dss_remove$duration, dss_remove$fc_exit)
dur1 <- coxph(S ~ race, data = dss_remove)
summary(dur1)

# race, gender, age
dur2 <- coxph(S ~ race + gender + age_rem, data=dss_remove)  
summary(dur2)  

# Plot of "survival" function
plot(survfit(dur1), xlab="Weeks", ylab="Cases Active")
race_treat <- with(dss_remove,
                    data.frame(
                      race = c("White", "Black", "Multi-Race"),
                      gender = c("Male", "Male", "Male"),
                      age_rem = c(7,7,7))
                   )

plot(survfit(dur1, newdata = race_treat), 
     xlab="Weeks", ylab="Cases Active",
     col = c("lightblue", "blue", "darkblue")) 


# ..........................................................................................

# Generate visual of model results ----

# example using the house1 model, removal due to inadequate housing 

# predicted probability
racelev <- levels(factor(dss_remove$race))
logit.prob <- sapply(racelev, FUN=function(x){
  mean(predict(house1, type = "response", 
               newdata = mutate(dss_remove, race = x)), na.rm=TRUE)})

QI <- data.frame(race = racelev,
                 logit.prob = logit.prob)
kable(QI)

# standard errors
B <- coef(house1)
V <- vcov(house1)
sim.coefs <- mvrnorm(1000, mu=B, Sigma=V)

# simulated coefficients
logit2 <- house1
sim.qi <- apply(sim.coefs, 1, FUN=function(x){
  logit2$coefficients <- x
  logit.prob <- sapply(racelev, FUN=function(y){
    mean(predict(logit2, type = "response", 
                 newdata = mutate(dss_remove, race = y)), na.rm=TRUE)
  })
})
sim.qi <- t(sim.qi)

logit.race.prob.se <- apply(sim.qi, 2, sd)
logit.race.prob.lb <- apply(sim.qi, 2, FUN=function(x){
  quantile(x, .025)
})
logit.race.prob.ub <- apply(sim.qi, 2, FUN=function(x){
  quantile(x, .975)
})
QI <- mutate(QI, 
             logit.prob.se = logit.race.prob.se,
             logit.prob.lb = logit.race.prob.lb,
             logit.prob.ub = logit.race.prob.ub)
kable(QI)

# visualize the model results
g <- ggplot(QI, aes(x = race, y = logit.prob, colour=race)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin = logit.prob.lb, ymax=logit.prob.ub)) +
  ylim(0,1) +
  labs(title="Probability of Removal due to Inadequate Housing by Race",
       x="Race",
       y="Probability of Removal",
       caption = "Note: error bars are 95% credible intervals generated 
       through \n 1000 draws of new coefficients from the posterior.") + guides(color=F)
g



