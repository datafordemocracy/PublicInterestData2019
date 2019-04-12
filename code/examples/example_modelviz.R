# Public Interest Data Lab
# Example of visualization model results
# March 2019


# ..........................................................................................

# load libraries ----
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(MASS) # for ordered logit, negative binomial models
library(glm.predict) # for generating predicted values for logit, ordered logit, negative binomial
source("gen_qoi.R") # pull in the gen_qoi function (store the gen_quo.R file your data/scripts) 
                    # for generating predicted values for logit, negative binomial


# ..........................................................................................

# read in files ----
# read data from encrypted volume and data dictionary
setwd("/Volumes/PIDL19")
dss <- readRDS("dss.rds")
# created in data_clean.R


# ..........................................................................................

# Logit model example

# Generate a model for removal for inadequate housing ----
# subset data frame (children in foster care, white/black/multirace) for analysis, 
#   create age at removal, un-order ever_find for use as a covariate
dss_remove <- dss %>% 
  filter(fc_enter == "Yes") %>% 
  filter(race %in% c("White", "Black", "MultiRace")) %>% 
  mutate(age_rem = interval(start = dob, end = remove_currdate) /
           duration(num = 1, units = "years"),
         ever_find = factor(ever_find, ordered = FALSE),
         care_structure2 = fct_recode(care_structure,
                                      "Single" = "Single mom",
                                      "Single" = "Single dad",
                                      "Dual" = "Married couple",
                                      "Dual" = "Unmarried couple"))
dss_remove$race <- droplevels(dss_remove$race)
dss_remove$gender <- droplevels(dss_remove$gender)


rem_house <- glm(remove_house ~ race + age_rem + gender + care_structure2 + numref + ever_find, 
              data = dss_remove, family = "binomial")
summary(rem_house)
# Race does not appear to predict a significant difference in probability of removal for inadquate housing, 
# though multiracial children may be a bit more likely to experience this removal reason. Age appears related,
# with older children less likely to experience removal for this reason. And children who've had a 
# substantiated finding of moderate or high appear less likely than children with no finding to experience this removal
# reason (while only the moderate finding has a p-value we might normally suggest is "significant", 
# the coefficient for high is larger, and given the smaller number of children subject to this outcome,
# it cannot be written off).

# Let's translate the substantive effect of race into a more useful quantity

# APPROACH 1: gen_qoi function adapted from Charlotte/Kropko (takes a few seconds)
# to use: gen_qoi(df, "grp", mod)
pred_prob <- gen_qoi(dss_remove, "race", rem_house)
pred_prob # check the output
# Multiracial children have a higher probability of removal due to inadequate housing, at about 32%, 
# than do White children, at about 21%. The probability for Black children of removal for this reason
# is 22%, similar to that of White children. But these are just point estimates; the credibility intervals
# for White children range from 14% to 34%; for Black children, the credibility interval ranges from 17%
# to 31%; and for Multiracial children, the credibility interval ranges from 23% to 48%. These are all
# credible predicted probabilites based on this data.

# plot the predicted probabilities by race
p <- ggplot(pred_prob, aes(x = group, y = outcome)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Removal due to Inadequate Housing by Race",
       x="Race",
       y="Probability of Removal",
       caption = "Note: error bars are 90% credible intervals") 
p
# we'll need to think about colors/themes, ordering of factors, etc., a bit more


# APPROACH 2: predicts function from glm.predict
# to use: predicts(mod, "values", sim.count = 1000, conf.int = 0.90, set.seed = 1017)
#   values: "value1;value2;value3;..." a value for each variable in the model in sequence
#           F = all categories of factor; F(1) = first level of factor; number = value for numeric variable
#   sim.count = how many simulations to perform; conf.int = confidence level for credibility intervals; set.seed = for reproducibility
prob_pred <- predicts(rem_house, "F;8;F(2);F(2);2;F(3)", sim.count = 1000, conf.int = 0.90, set.seed = 1017)
# this says: all values of race (first variable), age = 8, second value of gender (Male), 
#   second value of family structure (single), numref = 2, third value of ever find (Find-Mod)
prob_pred # check the output

# plot the predicted probabilities by race
p2 <- ggplot(prob_pred, aes(x = race, y = mean)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Removal due to Inadequate Housing by Race",
       x="Race",
       y="Probability of Removal",
       caption = "Note: error bars are 90% credible intervals") 
p2


# ..........................................................................................

# Negative binomial model example
# Generate a model for number of referrals ----
ref_nb <- glm.nb(numref ~ race_ethn + age_ref1 + gender + ment_ab + phys_ab + phys_neg + sex_ab + substance_ex, 
                 data = dss)
summary(ref_nb)
# Race does appear related to the number of referrals made about a child, with both Black and Multiracial 
# children experiencing more referrals than White children on average. Asian adn Hispanic children appear
# subject to fewer referrals than White children on average (while only the coefficient for Hispanic children
# has a p-value we might normally consider significant, the small number of Asian children is equivalent in
# magnitude to that of Multiracial children, and given the smalle rnumber of Hispanic children present in 
# the data, this cannot be written off as unimportant). Age also appears related -- the older a child is
# at the time of first referral the fewer total referrals they experience in this three-year period. Boys
# appear to have a slightly larger number of referrals than do girls. The nature of maltreatment alleged
# is also predictive of number of referrals, with children who have allegedly experienced physical abuse,
# physical neglect, and sexual abuse receiving more referrals.

# Let's translate the substantive effect of race into a more useful quantity

# APPROACH 1: gen_qoi function adapted from Charlotte/Kropko (takes a few seconds)
pred_count <- gen_qoi(dss, "race_ethn", ref_nb)
pred_count # check the output
# The expected number of referrals for Black children (2.1) and Multiracial children (2.5) is higher than 
# that for White children (1.8), Asian children (1.3), or Hispanic children (1.5); the credibility intervals
# for Black and Multiracial children are well above the point estimates for all other children. 

# plot the predicted counts by race
p <- ggplot(pred_count, aes(x = group, y = outcome)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Predicted Number of Referrals by Race",
       x="Race",
       y="Number of Referrals",
       caption = "Note: error bars are 90% credible intervals") 
p
# we'll need to think about colors/themes, ordering of factors in figure, etc., a bit more

# APPROACH 2: predicts function from glm.predict
count_pred <- predicts(ref_nb, "F;8;F(2);F(1);F(1);F(2);F(1);F(1)", sim.count = 1000, conf.int = 0.90, set.seed = 1017)
# this says: all values of race (first variable), age = 8, second value of gender (Male), 
#   first value of mental abuse and physical abuse (No), second value of physical neglect (Yes), 
#   and first value of sexual abuse and substance exposed (No)
count_pred # check the output

# plot the predicted counts by race
p2 <- ggplot(count_pred, aes(x = race_ethn, y = mean)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Predicted Number of Referrals by Race",
       x="Race",
       y="Number of Referrals",
       caption = "Note: error bars are 90% credible intervals") 
p2


# ..........................................................................................

# Ordinal logit model example  ----

# Generate a model for removal for ever a finding ----
find_ologit <- polr(ever_find ~ race_ethn + age_ref1 + gender + numref + ment_ab + phys_ab + phys_neg + sex_ab + substance_ex, 
              data = dss, Hess = TRUE)
summary(find_ologit)
# polr doesn't generate p-values for coefficients; to generate them
find_ologit.coef <- data.frame(coef(summary(find_ologit)))
find_ologit.coef$pval = round((pnorm(abs(find_ologit.coef$t.value), lower.tail = FALSE) * 2),2)
find_ologit.coef
# Race does not appear strongly related to investigatory findings, though Multiracial and Asian children 
# appear somewhat more likely to receive more findings of more serious substantiated maltreatment relative
# to White children. Gender, number of referrals, and maltreatment allegations are more strongly related to
# the findings, with boys having a higher probability of serious findings than girls, and children with more 
# referrals (which, recall, is predicted by race) having a higher probability of more serious findings. 
# Alleged maltreatment of each kind observed in our data set is associated with a higher probabilit of 
# more serious findings.

# Let's translate the substantive effect of race into a more useful quantity

# APPROACH 1: gen_qoi function adapted from Charlotte/Kropko (takes a few seconds)
# I haven't successfully adapted the function to work for ordinal logit

# APPROACH 2: predicts function from glm.predict
ologit_pred <- predicts(find_ologit, "F;7;F(2);2;F(1);F(2);F(2);F(1);F(1)", sim.count = 1000, conf.int = 0.90, set.seed = 1017)
# this says: all values of race (first variable), age = 7, second value of gender (Male), numref = 2,
#   first value of mental abuse (No), second value of physical abuse and physical neglect (Yes), 
#   and first value of sexual abuse and substance exposed (No)
ologit_pred # check the output
# for ordered logit, you get the predicted probability of each possible outcome (no finding, finding low, etc.)

# plot the predicted probabilities by race
p2 <- ggplot(ologit_pred, aes(x = race_ethn, y = mean)) +
  geom_point(size=4) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  coord_flip() + 
  facet_wrap(~ level) +
  labs(title="Predicted Probability of Finding Level by Race",
       x="Race",
       y="Probability of Finding",
       caption = "Note: error bars are 90% credible intervals") 

p2
# we'll need to think about colors/themes, ordering of factors in figure, etc., a bit more
# this one's harder to summarize as the probability is divided among four possibilites
# (and the axis ranges from 0 to 1 making it harder to comapare the credibility intervals).


# ..........................................................................................

# Duration model example
# coming soon
