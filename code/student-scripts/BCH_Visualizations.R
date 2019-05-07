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

# set color palette
colorviz3 <- c("#b25590","#e35d7c","#fe785b")
colorviz5 <- c("#384c7d","#755391","#b25590","#e35d7c","#fe785b")

# caregiver model with race
care_model <- glm(care_structure2 ~ race, data = dss2, family = "binomial")
care_app1 <- gen_qoi(dss2, "race", care_model)
care_app1

# foster model with race
foster_model <- glm(foster_structure2 ~ race, data = dss2, family = "binomial")
foster_app1 <- gen_qoi(dss2, "race", foster_model)
foster_app1

# reorder factors
care_app1 <- mutate(care_app1,
                    group=fct_relevel(group, "White", "Black", "MultiRace"),
                    group=fct_rev(group))

foster_app1 <- mutate(foster_app1, 
                      group=fct_relevel(group, "White", "Black", "MultiRace"),
                      group=fct_rev(group))

# generate plots
care_app1_plot <- ggplot(care_app1, aes(x = group, y = outcome, color=group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Coming From a Single Parent Home",
       x="Race",
       y="Probability of Single Parent",
       caption = "Note: error bars are 90% credible intervals") +
  expand_limits(y= 0) +
  scale_color_manual(values=colorviz3) + 
  theme(legend.position= 'none',
        plot.title = element_text(hjust = 0.5))
care_app1_plot

foster_app1_plot <- ggplot(foster_app1, aes(x = group, y = outcome, color=group)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Being Placed in a Single Parent Foster Home",
       x="Race",
       y="Probability of Single Parent",
       caption = "Note: error bars are 90% credible intervals") +
  expand_limits(y= 0) +
  scale_color_manual(values=colorviz3) +
  theme(legend.position= 'none',
        plot.title = element_text(hjust = 0.5))
foster_app1_plot
