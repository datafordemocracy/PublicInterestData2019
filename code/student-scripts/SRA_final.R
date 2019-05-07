##Final Script for Stuart, Ana, and Rishabh

library(tidyverse)
library(RColorBrewer)
library(scales)
library(MASS)

setwd("/Volumes/PIDL19")
load("cville_acs.Rdata")

##Updated Population Graph

colorviz5b <- c("#fe785b","#e35d7c", "#b25590", "#755391", "#384c7d")

acs_ref %>% filter(race != "Other") %>%  
  ggplot(aes(y = number, x = source)) +
  geom_bar(stat = "identity", aes(fill=race), position = "fill") +
  labs(title = "Population Proportions and Referral Proportions by Race",
       subtitle = "Population proportions from 2013-2017 American Community Survey",
       y = "", x = "", fill = "race") + 
  scale_fill_manual(values=colorviz5b) +
  scale_x_discrete(labels=c("acs17" = "2013-2017 Population", "ref" = "2015-2017 Referrals")) +
  scale_y_continuous(labels = scales::percent) +
  annotate("text", x = 1, y = c(.02, .10, .20, .46, .95), 
           label = c("4.7 (±- 1.4)", "8.2 (± 2.5)", "10.2 (± 1.3)", "26.4 (± 3.2)", "50.8 (± 0.9)"), color = "white") + 
  annotate("text", x = 2, y = c(.02, .09, .16, .72, .95), 
           label = c("0.8", "10.9", "6.1", "56.0", "24.0"), color = "white")


##Updated Disproportionality Index Graph
ref_acs <- ref_acs %>% 
  mutate(rd_lo = refprop/(prop - pmoe),
         rd_mi = refprop/prop,
         rd_hi = refprop/(prop + pmoe))

colorviz5 <- c("#384c7d","#755391","#b25590","#e35d7c","#fe785b")

ref_acs %>% filter(race != "Total" & race != "Other") %>% 
  ggplot(aes(x = race, y = rd_mi, fill = race, color=race)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = rd_hi, ymax = rd_lo), width = .2) +
  geom_hline(yintercept = 1, color = "black") +
  scale_y_continuous(name = "Disproportionality Index (90% Confidence Intervals)", 
                     trans = "log",
                     breaks = c(0.125, 0.25, 0.33, 0.5, 0.67, 1, 1.5, 2, 3), 
                     labels = c("0.125", "0.25", "0.33", "0.5", "0.67", "1", "1.5", "2", "3")) +
  scale_color_manual(values=colorviz5) +
  scale_fill_manual(values=colorviz5) +
  expand_limits(y = 3) +
  labs(title = "Racial Disproportionality Index in Referrals",
       subtitle = "Based on population proportions from 2013-2017\n American Community Survey",
       y = "", x = "", fill = "race") +
  annotate("text", x = c(1,2,3,4,5), y = c(.85, 1.15, .85, 1.15, .85), label = c("0.18", "2.12", "0.60", "1.34", "0.47"), color = "white") +
  coord_flip()


##Average Refs Graph
dss <- readRDS("dss.rds")
refdata <- readRDS("referral.rds")

group_by (refdata, race_ethn) %>% tally()

dss %>% filter(race_ethn != "Unknown") %>% 
  ggplot(aes(x=race_ethn, y=numref, fill = race_ethn)) + stat_summary(fun.y="mean", geom="bar") +
  scale_fill_manual(values=colorviz5b) +
  labs(title = "Average Number of Referrals by Race",
       y = "", x = "") +
  annotate("text", x=2, y=2.1, color='black', label=paste0(" * "), size=8) +
  annotate("text", x=3, y=2.8, color='black', label=paste0(" * "), size=8) +
  theme(legend.position="none") +
  geom_label(x=4.6, y=2.3, label="Note: * represents a \n significant difference \n from White children \n at p < .01", label.size=.5,
             inherit.aes=FALSE)

##Models
library(stargazer)

##Regressions for Foster Care Entrance
reg1<-glm(fc_enter~age_ref1+race_ethn, family="binomial", data = dss)
summary(reg1)

reg2<-glm(fc_enter~age_ref1+race_ethn+numref+ment_ab+phys_ab+phys_neg+substance_ex, family="binomial", data = dss)
summary(reg2)

reg3<-glm(fc_enter~age_ref1+race_ethn+ever_unsafe+ever_find+numref+ment_ab+phys_ab+phys_neg+substance_ex, family="binomial", data = dss)
summary(reg3)

stargazer(reg1, reg2, reg3, type = "latex", star.cutoffs = c(0.25, 0.1, 0.05))

#********#visualization of model
colorviz5 <- c("#384c7d","#755391","#b25590","#e35d7c","#fe785b")
pred_prob <- gen_qoi(dss, "race_ethn", reg3)
pred_prob
pred_prob <- mutate(pred_prob, 
                    group=fct_relevel(group, "White", "Black", 
                                      "MultiRace", "Hispanic", "Asian"),
                    group=fct_rev(group)) # because we're using coord_flip()

p <- ggplot(subset(pred_prob, !group=="Unknown"), aes(x = group, y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Entering Foster Care by Race",
       x="Race",
       y="Probability of Entering Foster Care",
       caption = "Note: error bars are 90% credible intervals")+
  scale_color_manual(values=colorviz5)
p

##Regressions for Number of Referrals
negbin3 <- glm.nb(numref ~ race_ethn + gender + age_ref1+I(age_ref1^2), data=dss)
summary(negbin3)

negbin4 <- glm.nb(numref ~ race_ethn + gender + age_ref1+I(age_ref1^2)+phys_ab+phys_neg+sex_ab, data=dss)
summary(negbin4)

stargazer(negbin3, negbin4, type = "latex", star.cutoffs = c(0.25, 0.1, 0.05))
#Do we add this into the report?

#Here is the code for all things related to Race Ethnicity and Age: 

#Density Graph to model the age at referral for each of the races all on the same plot
dss %>% ggplot(aes(x= age_ref1, fill= race)) + geom_density(alpha= 0.5) +
  labs(x= 'Age at Referral', fill= 'Race', title= 'Age Density by Race')

#A general summary of all the ages in foster care 
summary(dss$age_ref1)

#A summary for each race with the age referred variable 
summary(black$age_ref1)
summary(white$age_ref1)
summary(multi$age_ref1)
summary(unknown$age_ref1)
summary(asian$age_ref1)

# Age of referral by race: use race_ethn, use aov(); e.g.,

age_aov <- aov(age_ref1 ~ race_ethn, data = dss) 
age_aov
summary(age_aov)

# The p-value is significantly low, so this is telling us that race ethnicity is a strong predictor of age 
# at referral and that this is a significant test. 

#############################

# Age of referral by race: look at histogram/density with facets -- highlights the 0 spike, 
# present for al groups (except Hispanic), but most pronounced for multiracial children;

ggplot(dss, aes(age_ref1)) + geom_histogram(aes(y=..density..), binwidth=1, fill = "blue") +
  geom_density(col = "orange") + facet_wrap(~ race_ethn)

############################  

#difference of means for races vs other races as opposed to vs all of the races 


#Subsetting all of the races 
white<- subset(dss, race_ethn=="White")
black<- subset(dss, race_ethn=="Black")
multi<- subset(dss, race_ethn=="MultiRace")
unknown<- subset(dss, race_ethn=="Unknown")
asian<- subset(dss, race_ethn=="Asian")
hisp<- subset(dss, race_ethn=="Hispanic")

#P-values for the ages of all of the races compared against the average of other races 

#White vs. Black 
t.test(white$age_ref1, y=black$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Significant 

#White vs. MultiRace 
t.test(multi$age_ref1, y=white$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Significant 

#White vs. Asian 
t.test(white$age_ref1, y=asian$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Not Significant - why is this? 

#White vs. Hispanic 
t.test(white$age_ref1, y=black$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Significant 

#White vs. Unknown 
t.test(white$age_ref1, y=unknown$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Not significant 

#Black vs. MultiRace 
t.test(black$age_ref1, y=multi$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Not significant (but very close) 

#Black vs. Asian 
t.test(black$age_ref1, y=asian$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Significant 

#Black vs. Hispanic 
t.test(black$age_ref1, y=hisp$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Significant 

#Black vs. Unknown 
t.test(black$age_ref1, y=unknown$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Not significant 

#Multirace vs. Asian 
t.test(multi$age_ref1, y=asian$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Significant 

#MultiRace vs Hispanic 
t.test(multi$age_ref1, y=hisp$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Significant 

#MultiRace vs. Unkown
t.test(multi$age_ref1, y=unknown$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Not significant 

#Asian vs. Hispanic 
t.test(asian$age_ref1, y=hisp$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Not significant 

#Asian vs. Unknown 
t.test(asian$age_ref1, y=unknown$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Significant 

#Hispanic vs. Unknown 
t.test(hisp$age_ref1, y=unknown$age_ref1, mu=0, alternative="two.sided", conf.level=0.95) 
#Not significant 


