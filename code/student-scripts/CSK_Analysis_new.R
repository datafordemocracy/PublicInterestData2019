### Kathryn Bernard, Carolyn Ours, Savannah Quick
### Analysis Script - PIDL '19
### April 26, 2019

###### Kathryn ###### 

### set working directory to encrypted volume
#setwd("/Volumes/PIDL19")

### load packages
library(tidyverse)
library(lubridate)
library(car) # for Anova function
library(psych) # for aov function
library(nnet) # for multinomial logit
library(stargazer) # for display table
library(glm.predict) # for predictive probabilities

### read in data created in CSK_Cleaning.R
foster <- readRDS("foster_clean.rds")

### create color palettes
colorviz5 <- c("#384c7d","#755391","#b25590","#e35d7c","#fe785b")
colorviz3 <- c("#b25590","#e35d7c","#fe785b")
## reorder color palette
color_swap <- c("#fe785b", "#e35d7c", "#b25590")

###### discharge reason ###### 

### subset data to only children who have been discharged
discharged <- foster %>% 
  filter(discharge_reason != 'Still in Care') %>% # remove children who haven't been discharged
  filter(discharge_reason != 'Custody Transfer (Agency)') %>% # remove child who was transferred to another agency
  mutate_at("discharge_reason", droplevels) %>% 
  mutate(discharge_reason = fct_relevel(discharge_reason, "Emancipation", "Reunification",
                                        "Adoption", "Custody Transfer (Relative)"))

### multinomial logit models - full set of discharge reasons
# 1. race only
# 2. demographics (race, age, gender)
# 3. + child_disabled
# 4. + top reasons for removal
# 5. final model 

## race only
dis1 <- multinom(discharge_reason ~ race, 
                 data=discharged)
summary(dis1)
Anova(dis1, type=3)

## race, gender, age
dis2 <- multinom(discharge_reason ~ race +
                   gender +
                   age_rem,
                 data=discharged)
summary(dis2)
Anova(dis2, type=3)


## race, gender, age, disabled
dis3 <- multinom(discharge_reason ~ race +
                   gender +
                   age_rem +
                   child_disabled,
                 data=discharged)

summary(dis3)
Anova(dis3, type=3)

## race, gender, age, disabled, parental substance abuse (alc/drug), remove house, neglect
## using remove_parent_alc and remove_parent_drug -- most salient for current case as opposed to the ever_find variables
dis4 <- multinom(discharge_reason ~ race +
                   gender +
                   age_rem +
                   child_disabled +
                   remove_parent_alc +
                   remove_parent_drug +
                   remove_house +
                   remove_neglect,
                 data=discharged)

summary(dis4)
Anova(dis4, type=3)

## race, gender, age, disabled, remove neglect, remove parent drug 
## AIC = 111; lowest of all models
dis5 <- multinom(discharge_reason ~ race +
                   gender +
                   age_rem +
                   child_disabled +
                   remove_neglect +
                   remove_house,
                 data=discharged)

summary(dis5)
Anova(dis5, type=3)

## race, gender, age, disabled, remove neglect
dis6 <- multinom(discharge_reason ~ race +
                   gender +
                   age_rem +
                   child_disabled +
                   remove_neglect,
                 data=discharged)

summary(dis6)
Anova(dis6, type=3)

## race, gender, age, disabled, remove parent drug
dis7 <- multinom(discharge_reason ~ race +
                   gender +
                   age_rem +
                   child_disabled +
                   remove_parent_drug,
                 data=discharged)

summary(dis7)
Anova(dis7, type=3)

## race, gender, age, disabled, remove house
dis8 <- multinom(discharge_reason ~ race +
                   gender +
                   age_rem +
                   child_disabled +
                   remove_house,
                 data=discharged)

summary(dis8)
Anova(dis8, type=3)


## generate tables for appendix
stargazer(dis2, dis3)
stargazer(dis5)

### predictive probabilities
pred_dis5 <- predicts(dis5, "F;F(2);8;F(2);F(2);F(2)", sim.count = 1000, conf.int = 0.90, set.seed = 1017)

## plot
pred_dis5 %>% 
  mutate(race = fct_relevel(race, "Multiracial", "Black", "White")) %>% 
  ggplot(aes(x=race, y=mean, color=race)) +
  geom_point(size=4) + 
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  coord_flip() + 
  facet_wrap(~ level) +
  labs(title="Predicted Probability of Discharge Reason by Race",
       x="Race",
       y="Probability of Discharge Reason",
       caption = "Note: error bars are 90% credible intervals") +
  scale_color_manual(values=colorviz3, name="Race", breaks=c("White", "Black", "Multiracial"))


###### Savannah ###### 

#Code to look at case goals
cases <- foster %>% 
  filter(!(is.na(case_goal))) #copies foster data to avoid cluttering 
cases$permanancy <- (cases$case_goal=='Reunification'|cases$case_goal=='Live with relative'|
                       cases$case_goal=='Adoption')  #assigns true/false value for if case goal is a permanancy goal
cases$permanancy[cases$permanancy==TRUE] <- 1   #assigns 1 and 0 to use for binomial model
cases$permanancy[cases$permanancy==FALSE] <- 0

#model with just demographics
cases1 <- glm(permanancy ~ race + gender +
                  age_rem,
                data = cases, family = "binomial")

# model with disability 
cases2 <- glm(permanancy ~ race + gender + age_rem +
                  child_disabled,
                data = cases, family = "binomial")


#model with everything
cases3 <- glm(permanancy ~ race + ment_ab + phys_ab + gender +
                  phys_neg + sex_ab + age_rem + 
                  child_disabled,
                data = cases, family = "binomial")
#displays these different regressions
case_model <- stargazer(cases1, cases2, cases3,
                        title = "Probability of a Permanancy Case Goal",
                        covariate.labels = c("Black", "Multiracial", "Mental Abuse", "Physical Abuse",
                                             "Male", "Physical Neglect", "Sexual Abuse", 
                                             "Age at Current Removal", "Child Disabled"),
          star.cutoffs = c(0.25, 0.1, 0.05))


#####################################################
#### Logit Model: Foster Family Kin
#####################################################

## recodes as binary 
foster$ffkplace = (foster$place_now=='Foster family-kin')
foster$ffkplace[foster$ffkplace==TRUE] <- 1
foster$ffkplace[foster$ffkplace==FALSE] <- 0
summary(foster)

## single parent care binary 
foster$single_parent_care = (foster$care_structure=='Single mom' | foster$care_structure=='Single dad')
foster$single_parent_care[foster$single_parent_care==TRUE] <- 1
foster$single_parent_care[foster$single_parent_care==FALSE] <- 0

## black binary 
foster$black = (foster$race=='Black')
foster$black[foster$black==TRUE] <- 1
foster$black[foster$black==FALSE] <- 0

## multirace binary 
foster$multirace = (foster$race=='MultiRace')
foster$multirace[foster$multirace==TRUE] <- 1
foster$multirace[foster$multirace==FALSE] <- 0

## creates an age squared variable
foster$arsq = foster$age_rem * foster$age_rem

## Foster Fam Kin Logit Model with Demographic Controls Only 
ffklogit_d <- glm(ffkplace ~ race + age_rem + arsq + gender,
                  data = foster, family = "binomial", na.action = na.exclude)
summary(ffklogit_d)

## Foster Fam Kin with Demographic Controls + Disability + Family Structure
ffklogit_ddf <- glm(ffkplace ~ race + age_rem + arsq + gender
                    + child_disabled + single_parent_care,
                    data = foster, family = "binomial", na.action = na.exclude)
summary(ffklogit_ddf)

## Foster Fam Kin with Demographic Controls + Disability + Family Structure + Removal
## top five removal reasons: neglect, parent alcohol, parent drug, housing, physabuse 
ffklogit_ddfr <- glm(ffkplace ~ race + age_rem + arsq + gender 
                     + child_disabled + single_parent_care + remove_neglect + remove_parent_alc 
                     + remove_house + remove_parent_drug + remove_physabuse,
                     data = foster, family = "binomial", na.action = na.exclude)
summary(ffklogit_ddfr)

### Full Table, three models 
stargazer(ffklogit_d, ffklogit_ddf, ffklogit_ddfr,
          title = "Comparing Logit Regression of Placement with Foster Family Kin",
          covariate.labels = c("Black", "Multirace", "Age at removal", "Age at removal (sq.)",
                               "Male", "Disability Diagnosis", "Single Parent", 
                               "Removal: Neglect", "Removal: Parental Alcohol Use", 
                               "Removal: Inadequate Housing", "Removal: Parental Drug Use", 
                               "Removal: Physical Abuse"),
          type = "latex", star.cutoffs = c(0.25, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)

## One model table (last one with full set of removal controls)
stargazer(ffklogit_ddfr,
          title = "Logit Regression of Placement with Foster Family Kin",
          covariate.labels = c("Black", "Multirace", "Age at removal", "Age at removal (sq.)",
                               "Male", "Disability Diagnosis", "Single Parent", 
                               "Removal: Neglect", "Removal: Parental Alcohol Use", 
                               "Removal: Inadequate Housing", "Removal: Parental Drug Use", 
                               "Removal: Physical Abuse"),
          type = "latex", star.cutoffs = c(0.25, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)


##Generate QOI/probabilities 

pred_prob <- gen_qoi(foster, "race", ffklogit_ddfr)
pred_prob

## Plot probabilities by race 
predplot <- ggplot(pred_prob, aes(x = group, y = outcome, color = group, order = as.numeric(group))) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  scale_color_manual(values=colorviz3, name='Race') +
  coord_flip() +
  labs(title="Probability of Placement with Kin Foster Family by Race",
       x="Race",
       y="Probability of Placement",
       caption = "Note: error bars are 90% credible intervals") 
predplot
