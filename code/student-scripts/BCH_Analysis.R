##Authors: Brago, Conor, Hannah
library(tidyverse)
library(gridExtra)
library(lubridate)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(MASS)
library(glm.predict) 
library(stargazer)

setwd("/Volumes/PIDL19")
load("BCH_clean.RData")

#MODEL: Ever screened in
screen1 <- glm(ever_screened ~ race_ethn + gender + agemiss + age2,
               data=dss, na.action=na.omit, family=binomial(link="logit"))
summary(screen1)
anova(screen1, type = 3)

screen2 <- glm(ever_screened ~ race_ethn + gender + agemiss + age2 + ment_ab + phys_ab + phys_neg + sex_ab + substance_ex,
               data=dss, na.action=na.omit, family=binomial(link="logit"))
summary(screen2)
anova(screen2, type = 3)

screen3 <- glm(ever_screened ~ race_ethn + gender + agemiss + age2 + ment_ab + phys_ab + phys_neg + sex_ab + substance_ex + numref,
               data=dss, na.action=na.omit, family=binomial(link="logit"))
summary(screen3)
anova(screen3, type = 3)

# Generating predicted values from the "screen3" model
screen3_pred <- predicts(screen3, "F;F(2);0;8;0;0;0;0;0;2", sim.count = 1000, conf.int = 0.90, set.seed = 2238)
screen3_pred
# NOTE: predicted probabilities include all categories of race_eth, male, age not missing, age 8, no forms of 
# mental abuse, physical abuse or neglect, sexual abuse, or substance abuse, 2 referrals)

# Plotting predicted values
screen3_plot <- ggplot(screen3_pred, aes(x = race_ethn, y = mean, color=race_ethn)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Being Screened In by Race/Ethnicity",
       x="Race/Ethnicity",
       y="Probability",
       color= "Race",
       caption = "Note: error bars are 90% confidence intervals") +
  scale_color_manual(values=colorviz5)
screen3_plot

##MODEL: Ever Investigated
dss_screen_only <- dss %>% 
  filter(ever_screened == "Yes") %>% 
  filter(race %in% c("White", "Black", "MultiRace"))
investigated1 <- glm(as.factor(ever_inv2) ~ race + gender + agemiss + age2 + ment_ab + phys_ab + phys_neg + sex_ab + substance_ex + numref,
                     data=dss_screen_only, na.action=na.omit, family=binomial(link="logit"))
summary(investigated1)
anova(investigated1, type = 3)

# Generating predicted values from the "investigated1" model
investigated1_pred <- predicts(investigated1, "F;F(2);0;8;0;0;0;0;0;2", sim.count = 1000, conf.int = 0.90, set.seed = 2238)
investigated1_pred
# NOTE: predicted probabilities include the three categories of race considered in this analysis (White, Black,
# and multiracial, male, age not missing, age 8, no forms of mental abuse, physical abuse or neglect, sexual 
# abuse, or substance abuse, 2 referrals)

# Plotting predicted values
investigated1_plot <- ggplot(investigated1_pred, aes(x = race, y = mean, color=race)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Being Investigated by Race",
       x="Race",
       y="Probability",
       color= "Race",
       caption = "Note: error bars are 90% confidence intervals") +
  scale_color_manual(values=colorviz3)
investigated1_plot

##MODEL: Ever a Finding
dss_inv_only <- dss %>% 
  filter(ever_inv2 == "Yes") %>% 
  filter(race %in% c("White", "Black", "MultiRace"))

find_ologit1 <- polr(ever_find ~ race + gender + agemiss + age2, 
                     data = dss_inv_only, Hess = TRUE)
summary(find_ologit1)
find_ologit1.coef <- data.frame(coef(summary(find_ologit1)))
find_ologit1.coef$pval = round((pnorm(abs(find_ologit1.coef$t.value), lower.tail = FALSE) * 2),2)
find_ologit1.coef

find_ologit2 <- polr(ever_find ~ race + gender + agemiss + age2 + ment_ab + phys_ab + phys_neg + sex_ab + substance_ex, 
                     data = dss_inv_only, Hess = TRUE)
summary(find_ologit2)
find_ologit2.coef <- data.frame(coef(summary(find_ologit2)))
find_ologit2.coef$pval = round((pnorm(abs(find_ologit2.coef$t.value), lower.tail = FALSE) * 2),2)
find_ologit2.coef

find_ologit3 <- polr(ever_find ~ race + gender + agemiss + age2 + ment_ab + phys_ab + phys_neg + sex_ab + substance_ex + numref, 
                     data = dss_inv_only, Hess = TRUE)
summary(find_ologit3)
find_ologit3.coef <- data.frame(coef(summary(find_ologit3)))
find_ologit3.coef$pval = round((pnorm(abs(find_ologit3.coef$t.value), lower.tail = FALSE) * 2),2)
find_ologit3.coef

# Generating predicted values from the "find_ologit3" model
# NOTE: I couldn't figure out a way to make this work. I think something about the intercepts from the polr
# model were making R mad. I'll leave this one to the experts.
'
find_ologit3_pred <- predicts(find_ologit3, "F;F(2);0;8;0;0;0;0;0;2", sim.count = 1000, conf.int = 0.90, set.seed = 2238)
find_ologit3_pred
# NOTE: predicted probabilities include the three categories of race considered in this analysis (White, Black,
# and multiracial, male, age not missing, age 8, no forms of mental abuse, physical abuse or neglect, sexual 
# abuse, or substance abuse, 2 referrals)

# Plotting predicted values
find_ologit3_plot <- ggplot(find_ologit3_pred, aes(x = race, y = mean, color=race)) +
geom_point(size=4) +
geom_pointrange(aes(ymin = lower, ymax=upper)) +
coord_flip() +
labs(title="Probability of Having a Finding by Race",
x="Race",
y="Probability",
color= "Race",
caption = "Note: error bars are 90% confidence intervals") +
scale_color_manual(values=colorviz3)
find_ologit3_plot
'
##MODEL: Ever Unsafe
unsafe1 <- glm(ever_unsafe ~ race + gender + agemiss + age2,
               data=dss_screen_only, na.action=na.omit, family=binomial(link="logit"))
summary(unsafe1)
anova(unsafe1, type = 3)

unsafe2 <- glm(ever_unsafe ~ race + gender + agemiss + age2 + ment_ab + phys_ab + phys_neg + sex_ab + substance_ex,
               data=dss_screen_only, na.action=na.omit, family=binomial(link="logit"))
summary(unsafe2)
anova(unsafe2, type = 3)

unsafe3 <- glm(ever_unsafe ~ race + gender + agemiss + age2 + ment_ab + phys_ab + phys_neg + sex_ab + substance_ex + numref,
               data=dss_screen_only, na.action=na.omit, family=binomial(link="logit"))
summary(unsafe3)
anova(unsafe3, type = 3)

# Generating predicted values from the "unsafe3" model
unsafe3_pred <- predicts(unsafe3, "F;F(2);0;8;0;0;0;0;0;2", sim.count = 1000, conf.int = 0.90, set.seed = 2238)
unsafe3_pred
# NOTE: predicted probabilities include the three categories of race considered in this analysis (White, Black,
# and multiracial, male, age not missing, age 8, no forms of mental abuse, physical abuse or neglect, sexual 
# abuse, or substance abuse, 2 referrals)

# Plotting predicted values
unsafe3_plot <- ggplot(unsafe3_pred, aes(x = race, y = mean, color=race)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Being Determined to be Unsafe by Race",
       x="Race",
       y="Probability",
       color= "Race",
       caption = "Note: error bars are 90% confidence intervals") +
  scale_color_manual(values=colorviz3)
unsafe3_plot

##TABLES
#TABLE: Ever Screened In
stargazer(screen1, screen2, screen3, 
          title="Comparing Ordinal Logit Regression of Ever Screened In, By Race", 
          covariate.labels=c("Black", "Multiracial", "Hispanic", "Age - Missing", "Age - New Variable", "Male",
                             "Mental Abuse", "Physical Abuse",
                             "Physical Neglect", "Sexual Abuse", "Substance Abuse", "Number of Referrals"),
          type = "latex", star.cutoffs = c(0.25, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=TRUE)

#TABLE: Ever Investigated

#TABLE: Ever a Finding
stargazer(find_ologit1, find_ologit2, find_ologit3, 
          title="Comparing Ordinal Logit Regression of Ever a Finding of Abuse, By Race", 
          covariate.labels=c("Black", "Multiracial", "Hispanic", "Age - Missing", "Age - New Variable", "Male",
                             "Mental Abuse", "Physical Abuse",
                             "Physical Neglect", "Sexual Abuse", "Substance Abuse", "Number of Referrals"),
          type = "latex", star.cutoffs = c(0.25, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=TRUE)
#TABLE: Ever Unsafe
stargazer(unsafe1, unsafe2, unsafe3, 
          title="Comparing Ordinal Logit Regression of Ever Unsafe, By Race", 
          covariate.labels=c("Black", "Multiracial", "Hispanic", "Age - Missing", "Age - New Variable", "Male",
                             "Mental Abuse", "Physical Abuse",
                             "Physical Neglect", "Sexual Abuse", "Substance Abuse", "Number of Referrals"),
          type = "latex", star.cutoffs = c(0.25, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, dep.var.labels.include=TRUE)