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
source("gen_qoi.R")

dss <- readRDS("dss.rds")

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
#recoding age  
 dss <- dss %>%
   mutate(age2 = ifelse(is.na(age_ref1), 0, age_ref1),
          agemiss = if_else(is.na(age_ref1), 1, 0))
#recoding race
dss <- dss %>% 
  filter(race %in% c("White", "Black", "MultiRace", "Hispanic", "Asian"))
#drop levels
dss2$race <- droplevels(dss2$race)

save.image("BCH_clean.RData")
