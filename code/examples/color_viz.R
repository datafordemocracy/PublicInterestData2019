
# Public Interest Data Lab
# Example Viz with Colors 

#.......................................................................................

library(tidyverse)
library(gridExtra)
library(lubridate)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(MASS)
library(glm.predict) 
library(stargazer)

setwd("Volumes/PIDL19")

source("gen_qoi.R")
dss <- readRDS("dss.rds")

#.......................................................................................

# order of levels for race factor
# White, Black, MultiRace, Hispanic, Asian

# set palettes
colorviz5 <- c("#384c7d","#755391","#b25590","#e35d7c","#fe785b")
colorviz3 <- c("#b25590","#e35d7c","#fe785b")

# Model from Brago, Conor, and Hannah
#recoding care_structure and foster_structure
dss2 <- dss %>% 
  filter(fc_enter == "Yes") %>% 
  filter(race %in% c("White", "Black", "MultiRace")) %>% 
  mutate(care_structure2 = fct_recode(care_structure,
                                      "Single" = "Single mom",
                                      "Single" = "Single dad",
                                      "Dual" = "Married couple",
                                      "Dual" = "Unmarried couple"),
         foster_structure2 = fct_recode(foster_structure,
                                        "Single" = "Single mom",
                                        "Single" = "Single dad",
                                        "Dual" = "Married couple",
                                        "Dual" = "Unmarried couple")) %>% 
  filter(foster_structure2 != 'Not applicable')
dss2$race <- droplevels(dss2$race)

foster_model <- glm(foster_structure2 ~ race,data = dss2, family = "binomial")
foster_app1 <- gen_qoi(dss2, "race", foster_model)
foster_app1

# reorder factors
foster_app1 <- mutate(foster_app1, 
                     group=fct_relevel(group, "White", "Black", "MultiRace"),
                     group=fct_rev(group)) # because we're using coord_flip()

foster_app1_plot <- ggplot(foster_app1, aes(x = group, y = outcome, color=group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Being Placed in a Single Parent Foster Home",
       x="Race",
       y="Probability of Single Parent",
       caption = "Note: error bars are 90% credible intervals") +
  expand_limits(y= 0) +
  scale_color_manual(values=colorviz3) # set color
foster_app1_plot

# Model from Ana, Rishabh, and Stuart
ref_nb <- glm.nb(numref ~ race_ethn + age_ref1 + gender + 
                   ment_ab + phys_ab + phys_neg + sex_ab + substance_ex, 
                 data = dss)

pred_count <- gen_qoi(dss, "race_ethn", ref_nb)

# reorder factors
pred_count <- mutate(pred_count, 
                     group=fct_relevel(group, "White", "Black", 
                                       "MultiRace", "Hispanic", "Asian"),
                     group=fct_rev(group)) # because we're using coord_flip()

p <- ggplot(subset(pred_count, !group=="Unknown"), 
            aes(x = group, y = outcome, color=group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Predicted Number of Referrals by Race",
       x="Race",
       y="Number of Referrals",
       caption = "Note: error bars are 90% credible intervals") +
  scale_color_manual(values=colorviz5) # set colors
p

# Notes: 
# drop Unknown race from visualizations
# you must have a color argument in aes() in the ggplot command 
## (probably color=group or color=race)
# command is scale_color_manual(values=colorviz6) or scale_color_manual(values=colorviz3)
# when coord_flip() is used, the levels of the factor appear backward.

