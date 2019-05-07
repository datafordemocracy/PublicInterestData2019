## load data
setwd("/Volumes/PIDL19/")
dss <- readRDS("dss.Rds")

# .......................................................................................
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(car) # for Anova function
library(stargazer) # for table of results
library(MASS) # for ordered logit, negative binomial models
library(survival) # for duration/survival model
library(glm.predict)
library(reshape)
library(ggplot2)
library(plyr)
# .......................................................................................

## subset race to only include White, Black and multiracial (foster care data)
dss_remove <- dss %>% 
  filter(fc_enter == "Yes") %>% 
  filter(race %in% c("White", "Black", "MultiRace")) %>% 
  mutate(age_rem = interval(start = dob, end = remove_currdate) /
           duration(num = 1, units = "years"))
dss_remove$race <- droplevels(dss_remove$race)

# change get_none to get_support 
names(dss_remove)[names(dss_remove)=="get_none"] <- "get_support"
# mutate get_support so Yes=166 and No=12
dss_remove <- mutate(dss_remove, get_support = fct_recode(get_support, "No"="Yes", "Yes"="No"))

## fix everfind 
dss_remove <- dss_remove  %>% mutate(ever_find = factor(ever_find, ordered = FALSE))

## fix caretake 
dss_remove <- dss_remove %>% 
  mutate(care_structure2 = fct_recode(care_structure,
                                      "Single" = "Single mom",
                                      "Single" = "Single dad",
                                      "Dual" = "Married couple",
                                      "Dual" = "Unmarried couple"))

# creation of variable remove_sum
var <- c(names(dss_remove)[100:114])
dss_remove[var] <- lapply(dss_remove[var], function (x) {fct_recode(x, 
                                                                    "1"="Yes",
                                                                    "0"="No")}) 
dss_remove <- dss_remove %>% 
  mutate_at(var, as.character)
dss_remove <- dss_remove %>% 
  mutate_at(var, as.numeric)
dss_remove <- mutate(dss_remove, 
                     remove_sum=remove_physabuse+remove_sexabuse+remove_neglect+
                       remove_parent_alc+remove_parent_drug+remove_alc+remove_drug+
                       remove_disable+remove_disable+remove_behave+remove_death+
                       remove_jail+remove_cope+remove_cope+remove_abandon+
                       remove_relinq+remove_house)

# .......................................................................................

dss <- dss_remove

save.image("JAC_clean.RData")