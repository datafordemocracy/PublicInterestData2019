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
# children in foster care; only white, black, multirace; only removal variables and race
dss_remove <- dss %>% 
  filter(fc_enter == "Yes") %>% 
  filter(race %in% c("White", "Black", "MultiRace")) %>% 
  dplyr::select(race, remove_physabuse:remove_house)

# drop unused race levels, and recode reasons
dss_remove$race <- droplevels(dss_remove$race)
var <- c(names(dss)[100:114])
dss_remove <- dss_remove %>% 
  mutate_at(var, as.numeric) %>% 
  mutate_at(var, list(~ dplyr::recode(., `1` = 0L, `2` = 1L)))


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
  scale_fill_manual(values = brewer.pal(9, "Oranges")[c(8,6,4)]) +
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
  filter(race %in% c("White", "Black", "MultiRace")) %>% 
  mutate(age_rem = interval(start = dob, end = remove_currdate) /
           duration(num = 1, units = "years"))
dss_remove$race <- droplevels(dss_remove$race)
dss_remove$gender <- droplevels(dss_remove$gender)


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
                      race = c("White", "Black", "MultiRace"),
                      gender = c("Male", "Male", "Male"),
                      age_rem = c(7,7,7))
                   )

plot(survfit(dur1, newdata = race_treat), 
     xlab="Weeks", ylab="Cases Active",
     col = c("lightblue", "blue", "darkblue")) 


# competing risk: cause-specific
dss_remove2 <- dss_remove %>% filter(discharge_reason != "5232")

# Estimated survival function: race only
dss_remove2 <- dss_remove2 %>% 
  mutate(dis1560 = if_else(discharge_reason == "1560", 1, 0),
         dis1565 = if_else(discharge_reason == "1565", 1, 0),
         dis1571 = if_else(discharge_reason == "1571", 1, 0),
         dis1572 = if_else(discharge_reason == "1572", 1, 0))
S_1560 <- Surv(dss_remove2$duration, dss_remove2$dis1560)
S_1565 <- Surv(dss_remove2$duration, dss_remove2$dis1565)
S_1571 <- Surv(dss_remove2$duration, dss_remove2$dis1571)
S_1572 <- Surv(dss_remove2$duration, dss_remove2$dis1572)

dur_1560 <- coxph(S_1560 ~ race + gender + age_rem, data = dss_remove2)
dur_1565 <- coxph(S_1565 ~ race + gender + age_rem, data = dss_remove2)
dur_1571 <- coxph(S_1571 ~ race + gender + age_rem, data = dss_remove2)
dur_1572 <- coxph(S_1572 ~ race + gender + age_rem, data = dss_remove2)

summary(dur_1560)
summary(dur_1565)
summary(dur_1571)
summary(dur_1572)

stargazer(dur2, dur_1560, dur_1565, dur_1571, dur_1572, 
          type = "text", star.cutoffs = c(0.25, 0.1, 0.05))


install.packages("cmprsk")
library(cmprsk)

crr_data <- finegray(Surv(duration, discharge_reason) ~ race + gender + age_rem, data = dss_remove2)
dur_crr <- coxph(Surv(fgstart, fgstop, fgstatus) ~ race + gender + age_rem, 
                 weight = fgwt, data = crr_data)
summary(dur_crr)

# cuminc
dur_cuminc <- cuminc(dss_remove2$duration, dss_remove2$discharge_reason)
dur_cuminc
plot(dur_cuminc)

# crr
cov1 <- model.matrix(~ race + gender + age_rem,
                     data = dss_remove2)[, -1]
dur_crr2 <- crr(dss_remove2$duration, dss_remove2$discharge_reason, cov1, failcode = 1572, cencode = 0)
summary(dur_crr2)

dur_crr_pred <- predict(dur_crr2, rbind(c(0,0,0,10), c(1,0,0,10), c(0,1,0,10)))
plot(dur_crr_pred)


# ..........................................................................................
# consider age
dss <- dss %>% 
  mutate(age_cat5 = cut(age_ref1, breaks = c(0, 1, 4, 8, 12, Inf), include.lowest = TRUE),
         age_cat4 = cut(age_ref1, breaks = c(0, 2, 5, 8, Inf), include.lowest = TRUE),
         age_cat3 = cut(age_ref1, breaks = c(0, 2, 8, Inf), include.lowest = TRUE))


numref_age <- glm.nb(numref ~ race_ethn + age_ref1 + gender,
                     data = dss)
numref_age5 <- glm.nb(numref ~ race_ethn + age_cat5 + gender,
                      data = dss)
numref_age4 <- glm.nb(numref ~ race_ethn + age_cat4 + gender,
                      data = dss)
numref_age3 <- glm.nb(numref ~ race_ethn + age_cat3 + gender,
                      data = dss)

stargazer(numref_age, numref_age5, numref_age4, numref_age3, 
          type = "text", star.cutoffs = c(0.25, 0.1, 0.05))
# age5, age3, age4, age

screen_age <- glm(ever_screened ~ race_ethn + gender + age_ref1, data = dss, family = "binomial")
screen_age5 <- glm(ever_screened ~ race_ethn + gender + age_cat5, data = dss, family = "binomial")
screen_age4 <- glm(ever_screened ~ race_ethn + gender + age_cat4, data = dss, family = "binomial")
screen_age3 <- glm(ever_screened ~ race_ethn + gender + age_cat3, data = dss, family = "binomial")

stargazer(screen_age, screen_age5, screen_age4, screen_age3, 
          type = "text", star.cutoffs = c(0.25, 0.1, 0.05))
# age, age5, age4, age3


inv_age <- glm(as.factor(ever_inv2) ~ race_ethn + gender + age_ref1, data = dss, family = "binomial")
inv_age5 <- glm(as.factor(ever_inv2) ~ race_ethn + gender + age_cat5, data = dss, family = "binomial")
inv_age4 <- glm(as.factor(ever_inv2) ~ race_ethn + gender + age_cat4, data = dss, family = "binomial")
inv_age3 <- glm(as.factor(ever_inv2) ~ race_ethn + gender + age_cat3, data = dss, family = "binomial")

stargazer(inv_age, inv_age5, inv_age4, inv_age3, 
          type = "text", star.cutoffs = c(0.25, 0.1, 0.05))
# age5, age, age4, age3

find_age <- polr(ever_find ~ race_ethn + gender + age_ref1, data = dss, Hess = TRUE)
find_age5 <- polr(ever_find ~ race_ethn + gender + age_cat5, data = dss, Hess = TRUE)
find_age4 <- polr(ever_find ~ race_ethn + gender + age_cat4, data = dss, Hess = TRUE)
find_age3 <- polr(ever_find ~ race_ethn + gender + age_cat3, data = dss, Hess = TRUE)

stargazer(find_age, find_age5, find_age4, find_age2, 
          type = "text", star.cutoffs = c(0.25, 0.1, 0.05))
AIC(find_age)
AIC(find_age5)
AIC(find_age4)
AIC(find_age3)
# age, age3, age4, age5 

safe_age <- glm(ever_unsafe ~ race_ethn + gender + age_ref1, data = dss, family = "binomial")
safe_age5 <- glm(ever_unsafe ~ race_ethn + gender + age_cat5, data = dss, family = "binomial")
safe_age4 <- glm(ever_unsafe ~ race_ethn + gender + age_cat4, data = dss, family = "binomial")
safe_age3 <- glm(ever_unsafe ~ race_ethn + gender + age_cat3, data = dss, family = "binomial")

stargazer(safe_age, safe_age5, safe_age4, safe_age3, 
          type = "text", star.cutoffs = c(0.25, 0.1, 0.05))
# age, age3, age4, age5

foster_age <- glm(fc_enter ~ race_ethn + gender + age_ref1, data = dss, family = "binomial")
foster_age5 <- glm(fc_enter ~ race_ethn + gender + age_cat5, data = dss, family = "binomial")
foster_age4 <- glm(fc_enter ~ race_ethn + gender + age_cat4, data = dss, family = "binomial")
foster_age3 <- glm(fc_enter ~ race_ethn + gender + age_cat3, data = dss, family = "binomial")

stargazer(foster_age, foster_age5, foster_age4, foster_age3, 
          type = "text", star.cutoffs = c(0.25, 0.1, 0.05))
# age, age3, age4, age5


# ..........................................................................................

# Generate visual of model results ----
# example using the house5 model, removal due to inadequate housing 
dss_remove <- dss_remove %>% mutate(ever_find = factor(ever_find, ordered = FALSE))
house5 <- glm(remove_house ~ race + age_rem + gender + care_structure2 + numref + ever_find, 
              data = dss_remove, family = "binomial")
summary(house5)
Anova(house5, type = 3)

# from Charlotte/From Kropko
# Generate visual of model results ----

# predicted probability for logit model
racelev <- levels(factor(dss_remove$race))      # vector of levels for race
logit.prob <- sapply(racelev, FUN=function(x){  # get mean predicted probability for each level
  mean(predict(house5, type = "response", 
               newdata = mutate(dss_remove, race = x)), na.rm=TRUE)})

qoi <- data.frame(race = racelev,               # combine into data frame
                  logit.prob = logit.prob)
qoi


# generate bootstrapped credibility intervals 
B <- coef(house5) # extract coefficients from model
V <- vcov(house5) # extract covariance matrix from model
# for reproducibility, set random seed
set.seed(1017)
sim.coefs <- mvrnorm(1000, mu=B, Sigma=V) # simulate 1000 coefficients (mvrnorm from MASS)
                                          # (drawn from ~ N(beta, var(beta)))

# use simulated coefficients to generate a distribution of predicted probabilities
logit.sim <- house5
sim.qi <- apply(sim.coefs, 1, FUN=function(x){
  logit.sim$coefficients <- x
  logit.prob <- sapply(racelev, FUN=function(y){
    mean(predict(logit.sim, type = "response", 
                 newdata = mutate(dss_remove, race = y)), na.rm=TRUE)
  })
})
sim.qi <- t(sim.qi) # transpose the reuslting probability matrix
# this contains 1000 predicted probabilities of the outcome for each group/level

logit.race.prob.se <- apply(sim.qi, 2, sd) # estimate the standard error via the standard deviation of the simulated probabilities
logit.race.prob.lb <- apply(sim.qi, 2, FUN=function(x){ # calculate the lower bound of each interval
  quantile(x, .05) # takes the values with 5% of observations below (for 90% interval)
})
logit.race.prob.ub <- apply(sim.qi, 2, FUN=function(x){
  quantile(x, .95) # takes the values with 95% of observations above (for 90% interval)
})

qoi <- mutate(qoi, # add the estimated standard error, lower and upper bound for each predicted probability
             logit.prob.se = logit.race.prob.se,
             logit.prob.lb = logit.race.prob.lb,
             logit.prob.ub = logit.race.prob.ub)
qoi


#######
# let's put it in a function:
# input group variable (grp), data frame (df), model object (mod)
# output qoi (named whatever you want)
gen_qoi <- function(df, grp, mod){
  grplev <- levels(factor(df[[grp]]))
  grp <-enquo(grp)

  logit.prob <- sapply(grplev, FUN=function(x){
    mean(predict(mod, type = "response",
                 newdata = mutate(df, !! grp := !! x)), na.rm = TRUE)
  })
  
  qoi <- data.frame(group = grplev,
                    logit.prob = logit.prob)

  B <- coef(mod)
  V <- vcov(mod)
  set.seed(1017)
  sim.coefs <- mvrnorm(1000, mu = B, Sigma = V)

  logit.sim <- mod
  sim.qi <- apply(sim.coefs, 1, FUN=function(x){
    logit.sim$coefficients <- x
    logit.prob <- sapply(grplev, FUN=function(y){
      mean(predict(logit.sim, type = "response", 
                   newdata = mutate(df, !! grp := !! y)), na.rm=TRUE)
    })
  })
  sim.qi <- t(sim.qi)

  logit.grp.prob.se <- apply(sim.qi, 2, sd)
  logit.grp.prob.lb <- apply(sim.qi, 2, FUN=function(x){
    quantile(x, .05)
  })
  logit.grp.prob.ub <- apply(sim.qi, 2, FUN=function(x){
    quantile(x, .95)
  })
  
  qoi <- mutate(qoi, 
                logit.prob.se = logit.grp.prob.se,
                logit.prob.lb = logit.grp.prob.lb,
                logit.prob.ub = logit.grp.prob.ub)
}


# try it: gen_qoi(grp, df, mod)
prob <- gen_qoi(dss_remove, "race", house5)
prob


# plot the predicted probabilities by race
p <- ggplot(prob, aes(x = group, y = logit.prob)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = logit.prob.lb, ymax=logit.prob.ub)) +
  ylim(0,1) + coord_flip() +
  labs(title="Probability of Removal due to Inadequate Housing by Race",
       x="Race",
       y="Probability of Removal",
       caption = "Note: error bars are 90% credible intervals") 
p



# try negative binomial
var <- c(names(dss)[9:13])
dss <- dss %>% 
  mutate_at(var, as.numeric) %>% 
  mutate_at(var, list(~ dplyr::recode(., `1` = 0L, `2` = 1L)))
dss <- dss %>% 
  mutate(allege_sum = reduce(dplyr::select(., c(var)), `+`))

# difference in distributions
table(dss$race, dss$allege_sum)
prop.table(table(dss$race, dss$allege_sum), 1)

# difference in means
dss %>% 
  group_by(race_ethn) %>% summarize(mean(allege_sum))
aov(allege_sum ~ race_ethn, data = dss)


allege1 <- glm.nb(allege_sum ~ race_ethn + age_ref1 + gender + numref,
                  data = dss)
summary(allege1)

# try it: gen_qoi(grp, df, mod)
count <- gen_qoi(dss, "race_ethn", allege1)
count

# plot the predicted counts by race
p <- ggplot(count, aes(x = group, y = logit.prob)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = logit.prob.lb, ymax=logit.prob.ub)) +
  coord_flip() +
  labs(title="Expected Number of Allegations by Race",
       x="Race",
       y="Number of Allegations",
       caption = "Note: error bars are 90% credible intervals") 
p

# compare to glm.predicts
countpred <- predicts(allege1, "F;7;F(2);2", sim.count = 1000, conf.int = 0.90, set.seed = 1017)

p <- ggplot(countpred, aes(x = race_ethn, y = mean)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Expected Number of Allegations by Race",
       x="Race",
       y="Number of Allegations",
       caption = "Note: error bars are 90% credible intervals") 
p

# polr
dss_find <- dss %>% filter(ever_inv2 == "Yes")
find1 <- polr(ever_find ~ race_ethn + gender + age_ref1, data = dss_find, Hess = TRUE)
summary(find1)

# using glm.predicts
polpred <- predicts(find1, "F;F(2);7", sim.count = 1000, conf.int = 0.90, set.seed = 1017)
p2 <- ggplot(polpred, aes(x = race_ethn, y = mean)) +
  geom_point(size=4) + geom_pointrange(aes(ymin = lower, ymax = upper)) +
  coord_flip() + facet_wrap(~ level)
p2

table(dss$ever_find)
