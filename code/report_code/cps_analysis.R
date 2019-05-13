######################################################################################
# LP 5440: Public Interest Data Lab 
# Estimate racial disparity models
# 1. Load libraries and data
# 2. Set color palette 
# 3. Number of referrals
# 4. Ever screened in, investigated, finding, unsafe
# 5. Removal to foster care
# Authors: Brago, Conor, Hannah, Stuart, Rishabh, Ana, MPC
# Updated: May 10, 2019 
######################################################################################


# ..........................................................................................
# 1. Load libraries and data ----
# load libraries 
library(tidyverse)
library(MASS)
library(car)
library(stargazer)
library(glm.predict) 
source("gen_qoi.R")

# load data
setwd("/Volumes/PIDL19")
load("cps_clean.RData")
# files created in cps_clean.R


# ..........................................................................................
# 2. Set color palette ----
colorviz3 <- c("#b25590","#e35d7c","#fe785b")
colorviz3_rev <- c("#fe785b","#e35d7c","#b25590")
colorviz5 <- c("#384c7d","#755391","#b25590","#e35d7c","#fe785b")
colorviz5_rev <- c("#fe785b","#e35d7c", "#b25590", "#755391", "#384c7d")


# ..........................................................................................
# 3. Number of referrals ---

# Negative binomial model for count of referrals
# demographics
negbin1 <- glm.nb(numref ~ race_ethn + gender + agemiss + age2 + I(age2^2), 
                  data = dss2)
summary(negbin1)
Anova(negbin1, type = 3)

# demographics and alleged maltreatment
negbin2 <- glm.nb(numref ~ race_ethn + gender + agemiss + age2 + I(age2^2) +
                    phys_ab + phys_neg + sex_ab + ment_ab + substance_ex, 
                  data = dss2)
summary(negbin2)
Anova(negbin2, type = 3)


# generate expected counts by race: negbin2
set.seed(1017)
pred_count_ref <- gen_qoi(dss2, "race_ethn", negbin2)
pred_count_ref # check the output
pred_count_ref <- pred_count %>% 
  mutate(group = fct_relevel(group, "White", "Black", "Multirace", "Hispanic", "Asian"))


# plot the predicted counts by race
ggplot(pred_count_ref, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=colorviz5_rev) +
  labs(title="Predicted Number of Referrals by Race",
       x = "", color = "Race",
       y = "Predicted Number of Referrals",
       caption = "Note: error bars are 90% credible intervals") + 
ggsave("figures/pred_numref_race.pdf", width=9, height=6, units="in")   


# table for appendix
stargazer(negbin1, negbin2, 
          title = "Negative Binomial Model of Number of Referrals",
          covariate.labels = c("Black", "Multiracial", "Hispanic", "Asian", "Male", "Gender Unknown",
                               "Age Missing", "Age", "Age-squared",
                               "Alleged Physical Abuse", "Alleged Physical Neglect", "Alleged Sexual Abuse",
                               "Alleged Mental Abuse", "Alleged Substance Exposed Infant"),
          dep.var.caption = "", omit.stat = "theta",
          type = "latex", star.cutoffs = c(0.2, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=TRUE)


# ..........................................................................................
# 4. Ever screened in, investigated, finding, unsafe ----

# Ever screened in - all referrals
# demographics
screen1 <- glm(ever_screened ~ race_ethn + gender + agemiss + age2,
               data=dss2, family=binomial(link="logit"))
summary(screen1)
Anova(screen1, type = 3)

# demographics and alleged maltreatment
screen2 <- glm(ever_screened ~ race_ethn + gender + agemiss + age2 + 
                 ment_ab + phys_ab + phys_neg + sex_ab,
               data=dss2, family=binomial(link="logit"))
summary(screen2)
Anova(screen2, type = 3)

# demographics, alleged maltreatment, and number of referrals
screen3 <- glm(ever_screened ~ race_ethn + gender + agemiss + age2 + 
                 ment_ab + phys_ab + phys_neg + sex_ab + 
                 numref + numref3,
               data=dss2, family=binomial(link="logit"))
summary(screen3)
Anova(screen3, type = 3)


# generate predicted probabilities by race: screen3
set.seed(1017)
pred_prob_screen <- gen_qoi(dss2, "race_ethn", screen3)
pred_prob_screen # check the output
pred_prob_screen <- pred_prob_screen %>% 
  mutate(group = fct_relevel(group, "White", "Black", "Multirace", "Hispanic", "Asian"))


# plot predicted probabilities
ggplot(pred_prob_screen, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=colorviz5_rev) +
  labs(title="Predicted Probability of being Screened In by Race",
       x = "", color = "Race",
       y = "Predicted Probability of Screened In",
       caption = "Note: error bars are 90% credible intervals")
ggsave("figures/pred_screen_race.pdf", width=9, height=6, units="in")   

# screen3_pred <- predicts(screen3, "F;F(2);0;8;0;0;0;0;0;2", sim.count = 1000, conf.int = 0.90, set.seed = 2238)
# screen3_pred


# Ever Investigated - screened in referrals
dss3 <- dss2 %>% 
  filter(ever_screened == "Yes" & gender != "Unknown") 
dss3$gender <- droplevels(dss3$gender)

# demographics
inv1 <- glm(as.factor(ever_inv2) ~ race_ethn + gender + agemiss + age2,
            data=dss3, family=binomial(link="logit"))
summary(inv1)
Anova(inv1, type = 3)

# demographics and alleged maltreatment
inv2 <- glm(as.factor(ever_inv2) ~ race_ethn + gender + agemiss + age2 + 
              ment_ab + phys_ab + phys_neg + sex_ab,
            data=dss3, family=binomial(link="logit"))
summary(inv2)
Anova(inv2, type = 3)

# demographics, alleged maltreatment, and number of referrals
inv3 <- glm(as.factor(ever_inv2) ~ race_ethn + gender + agemiss + age2 + 
              ment_ab + phys_ab + phys_neg + sex_ab + 
              numref,
            data=dss3, family=binomial(link="logit"))
summary(inv3)
Anova(inv3, type = 3)


# Generating predicted probabilites by race: inv3
set.seed(1017)
pred_prob_inv <- gen_qoi(dss3, "race_ethn", inv3)
pred_prob_inv # check the output
pred_prob_inv <- pred_prob_inv %>% 
  mutate(group = fct_relevel(group, "White", "Black", "Multirace", "Hispanic", "Asian"))


# Plot predicted probabilities
ggplot(pred_prob_inv, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=colorviz5_rev) +
  labs(title="Predicted Probability of Investigation by Race",
       x = "", color = "Race",
       y = "Predicted Probability of Investigation",
       caption = "Note: error bars are 90% credible intervals") 
ggsave("figures/pred_inv_race.pdf", width=9, height=6, units="in")   

# investigated1_pred <- predicts(investigated1, "F;F(2);0;8;0;0;0;0;0;2", sim.count = 1000, conf.int = 0.90, set.seed = 2238)
# investigated1_pred


# Ever a Finding - screened in and investigated referrals
dss4 <- dss3 %>% 
  filter(ever_inv2 == "Yes" & race_ethn != "Asian") 
dss4$race_ethn <- droplevels(dss4$race_ethn)

# demographics
find1 <- polr(ever_find ~ race_ethn + gender + agemiss + age2, 
              data = dss4, Hess = TRUE)
summary(find1)

find1.coef <- data.frame(coef(summary(find1)))
find1.coef$pval = round((pnorm(abs(find1.coef$t.value), lower.tail = FALSE) * 2),2)
find1.coef

# demographics and alleged maltreatment
find2 <- polr(ever_find ~ race_ethn + gender + agemiss + age2 + 
                ment_ab + phys_ab + phys_neg + sex_ab, 
              data = dss4, Hess = TRUE)
summary(find2)
find2.coef <- data.frame(coef(summary(find2)))
find2.coef$pval = round((pnorm(abs(find2.coef$t.value), lower.tail = FALSE) * 2),2)
find2.coef

# demographics, alleged maltreatment, and number of referrals
find3 <- polr(ever_find ~ race_ethn + gender + agemiss + age2 + 
                ment_ab + phys_ab + phys_neg + sex_ab + 
                numref, 
              data = dss4, Hess = TRUE)
summary(find3)
find3.coef <- data.frame(coef(summary(find3)))
find3.coef$pval = round((pnorm(abs(find3.coef$t.value), lower.tail = FALSE) * 2),2)
find3.coef


# Generating predicted proabilities by race: find_ologit3
pred_prob_find <- predicts(find3, "F;F(2);0;8;0;0;1;0;2", sim.count = 1000, conf.int = 0.90, set.seed = 2238)
pred_prob_find

 
# # Plot predicted probabilities
ggplot(pred_prob_find, aes(x = race_ethn, y = mean, color = race_ethn)) +
  geom_point(size = 4) + geom_pointrange(aes(ymin = lower, ymax = upper)) +
  labs(title = "Predicted Probability of Finding by Race", 
       x = "", color = "Race",
       y = "Predicted Probability of a Finding",
       caption = "Note: error bars are 90% credible intervals") + 
  coord_flip() + facet_wrap(~ level) +  
  scale_color_manual(values=colorviz5[2:5])
ggsave("figures/pred_find_race.pdf", width=9, height=6, units="in")   


# Ever Unsafe - screened in referrals
# among all Asian and Hispanic children in screened in sample, no unsafe determinations
# model perfectly predicts these; try without
# similarly, all children with missing age are never unsafe
dss5 <- dss3 %>% 
  filter(race_ethn != "Asian" & race_ethn != "Hispanic" & agemiss == 0)
dss5$race_ethn <- droplevels(dss5$race_ethn)

# demographics
unsafe1 <- glm(ever_unsafe ~ race_ethn + gender + age2,
               data=dss5, family=binomial(link="logit"))
summary(unsafe1)
Anova(unsafe1, type = 3)

# demographics and alleged maltreatment
unsafe2 <- glm(ever_unsafe ~ race_ethn + gender + age2 + 
                 ment_ab + phys_ab + phys_neg + sex_ab,
               data=dss5, family=binomial(link="logit"))
summary(unsafe2)
Anova(unsafe2, type = 3)

# demographics, alleged maltreatment and number of referrals, ever investigate4d
unsafe3 <- glm(ever_unsafe ~ race_ethn + gender + age2 + 
                 ment_ab + phys_ab + phys_neg + sex_ab + 
                 numref + ever_inv2 + ever_find2,
               data=dss5, family=binomial(link="logit"))
summary(unsafe3)
Anova(unsafe3, type = 3)


# Generating predicted probabiliites by race: unsafe3
set.seed(1017)
pred_prob_unsafe <- gen_qoi(dss5, "race_ethn", unsafe3)
pred_prob_unsafe # check the output
pred_prob_unsafe <- pred_prob_unsafe %>% 
  mutate(group = fct_relevel(group, "White", "Black", "Multirace"))


# Plot predicted probabilities
ggplot(pred_prob_unsafe, aes(x = fct_rev(group), y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  scale_color_manual(values=colorviz3_rev) +
  labs(title="Predicted Probability of Unsafe Determination by Race",
       x = "", color = "Race",
       y = "Predicted Probability of Unsafe Determination",
       caption = "Note: error bars are 90% credible intervals")
ggsave("figures/pred_unsafe_race.pdf", width=9, height=6, units="in")   

# unsafe3_pred <- predicts(unsafe3, "F;F(2);0;8;0;0;0;0;0;2", sim.count = 1000, conf.int = 0.90, set.seed = 2238)
# unsafe3_pred


# TABLES for appendix
# Ever Screened In
stargazer(screen1, screen2, screen3, 
          title = "Logit Model of Ever Screened In",
          covariate.labels=c("Black", "Multiracial", "Hispanic", "Asian", "Male", "Unknown Gender", 
                             "Age Missing", "Age", 
                             "Alleged Mental Abuse", "Alleged Physical Abuse", "Alleged Physical Neglect", "Alleged Sexual Abuse", 
                             "Number of Referrals", "More than 3 Referrals"),
          dep.var.caption = "", omit.stat = "theta",
          type = "latex", star.cutoffs = c(0.20, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)

# Ever Investigated
stargazer(inv1, inv2, inv3, 
          title = "Logit Model of Ever Investigated among Children Screened In",
          covariate.labels=c("Black", "Multiracial", "Hispanic", "Asian", "Male",
                             "Age Missing", "Age", 
                             "Alleged Mental Abuse", "Alleged Physical Abuse", "Alleged Physical Neglect", "Alleged Sexual Abuse", 
                             "Number of Referrals"),
          dep.var.caption = "", omit.stat = "theta",
          type = "latex", star.cutoffs = c(0.20, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)

# Ever a Finding
stargazer(find1, find2, find3, 
          title="Ordered Logit Model of Ever a Finding among Children Screened In and Investigated", 
          covariate.labels=c("Black", "Multiracial", "Hispanic", "Male",
                             "Age Missing", "Age", 
                             "Alleged Mental Abuse", "Alleged Physical Abuse", "Alleged Physical Neglect", "Alleged Sexual Abuse", 
                             "Number of Referrals"),
          dep.var.caption = "", summary.stat = c("n"),
          type = "latex", star.cutoffs = c(0.20, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)

# Ever Unsafe
stargazer(unsafe1, unsafe2, unsafe3, 
          title="Logit Model of Ever Unsafe among Children Screened In", 
          covariate.labels=c("Black", "Multiracial", "Male", "Age", 
                             "Alleged Mental Abuse", "Alleged Physical Abuse", "Alleged Physical Neglect", "Alleged Sexual Abuse", 
                             "Number of Referrals", "Ever Investigated", "Ever a Finding"),
          dep.var.caption = "", omit.stat = "theta",
          type = "latex", star.cutoffs = c(0.20, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)


# ..........................................................................................
# 5. Removal to foster care ----

# all referrals: 
# 36 removals were among children not screened in
# 59 removals were among children not investigated
# 96 removals were among children with no finding
# demographics
remove1 <- glm(fc_enter ~ race_ethn + age_ref1 + gender, 
               family="binomial", data = dss2)
summary(remove1)

# demographics and alleged maltreatment
remove2 <- glm(fc_enter ~ race_ethn + age_ref1 + gender + 
                 ment_ab + phys_ab + phys_neg + substance_ex, 
               family="binomial", data = dss2)
summary(remove2)

# demographics, alleged maltreatment and number of referrals
remove3 <- glm(fc_enter ~ race_ethn + age_ref1 + gender + 
                 ment_ab + phys_ab + phys_neg + substance_ex + 
                 numref, 
               family="binomial", data = dss2)
summary(remove3)

# demographics, alleged maltreatment, number of referrals, ever screened, investigated, finding, unsafe
remove4 <- glm(fc_enter ~ race_ethn + age_ref1 + gender + 
                 ment_ab + phys_ab + phys_neg + substance_ex + 
                 numref + ever_screened + ever_inv2 + ever_find2 + ever_unsafe, 
               family="binomial", data = dss2)
summary(remove4)


# among screened in referrals (in response to the curiously negative coefficient on ever_screened above)
# demographics
remove1b <- glm(fc_enter ~ race_ethn + age_ref1 + gender, 
               family="binomial", data = dss3)
summary(remove1b)

# demographics and alleged maltreatment
remove2b <- glm(fc_enter ~ race_ethn + age_ref1 + gender + 
                  ment_ab + phys_ab + phys_neg + substance_ex, 
               family="binomial", data = dss3)
summary(remove2b)

# demographics, alleged maltreatment and number of referrals
remove3b <- glm(fc_enter ~ race_ethn + age_ref1 + gender + 
                  ment_ab + phys_ab + phys_neg + substance_ex + 
                  numref, 
               family="binomial", data = dss3)
summary(remove3b)

# demographics, alleged maltreatment, number of referrals, ever investigated, finding, unsafe
remove4b <- glm(fc_enter ~ race_ethn + age_ref1 + gender + 
                 ment_ab + phys_ab + phys_neg + substance_ex + 
                 numref + ever_inv2 + ever_find2 + ever_unsafe, 
               family="binomial", data = dss3)
summary(remove4b)


# generate predicted probabilities by race
set.seed(1017)
pred_prob_remove <- gen_qoi(dss3, "race_ethn", remove4b)
pred_prob_remove
pred_prob_remove <- mutate(pred_prob_remove, 
                    group=fct_relevel(group, "White", "Black", 
                                      "Multirace", "Hispanic", "Asian"),
                    group=fct_rev(group)) # because we're using coord_flip()


# plot predicted probabilities
ggplot(pred_prob_remove, aes(x = group, y = outcome, color = group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title = "Predicted Probability of Removal to Foster Care by Race",
       x = "", color = "Race",
       y = "Predicted Probability of Removal",
       caption = "Note: error bars are 90% credible intervals") + 
  scale_color_manual(values=colorviz5)
ggsave("figures/pred_remove_race.pdf", width=9, height=6, units="in")   


# TABLES for appendix
stargazer(remove1b, remove2b, remove3b, remove4b, 
          title="Logit Model of Removal to Foster Care among Children Screened In", 
          covariate.labels=c("Black", "Multiracial", "Hispanic", "Asian", 
                             "Age", "Male",  
                             "Alleged Mental Abuse", "Alleged Physical Abuse", "Alleged Physical Neglect", "Alleged Substance Exposed", 
                             "Number of Referrals", "Ever Investigated", "Ever a Finding", "Ever Unsafe Determination"),
          dep.var.caption = "", omit.stat = "theta",
          type = "latex", star.cutoffs = c(0.20, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=FALSE)


