
######################################################################################
# Public Interest Data Lab 
# Foster Care: Cleaning Script 
# Combine work from Janie, Alex, Carolynn and Carolyn, Savannah, Kathryn
# Updated: May 12, 2019
######################################################################################

# load libraries
library(tidyverse)
library(lubridate)

## set working directory
setwd("/Volumes/PIDL19/")

## load data
dss <- readRDS("dss.Rds")

######################################################################################

### cleaning
foster <- dss %>% 
  filter(fc_enter == "Yes") %>% # subset to children who entered foster care
  filter(race %in% c("White", "Black", "MultiRace")) %>% # children who are white, black, and multiracial
  mutate(child_disabled = case_when(child_disabled=='No' ~ 'No',
                                    child_disabled=='Yes' ~ 'Yes',
                                    TRUE ~ 'No')) %>% # change 'undetermined' to 'no'
  mutate(child_disabled = fct_relevel(child_disabled, "No", "Yes")) %>% # relevel so 'no' is the baseline
  mutate(race = fct_relevel(race, "White", "Black", "MultiRace")) %>% # relevel so 'white' is the baseline
  mutate(race = fct_recode(race, "White" = "White",
                           "Black" = "Black",
                           "MultiRace" = "MultiRace")) %>%  # change to language used in report
  mutate(age_rem = interval(start = dob, end = remove_currdate) /
           duration(num = 1, units = "years")) %>% # create variable for age at removal from home
  mutate(discharge_reason = fct_recode(discharge_reason, "Still in Care" = "0", # add labels for discharge reason
                                       "Adoption" = "1560",
                                       "Emancipation" = "1565",
                                       "Custody Transfer (Relative)" = "1571",
                                       "Reunification" = "1572",
                                       "Custody Transfer (Agency)" = "5232")) %>% 
  mutate(discharge_reason2 = fct_recode(discharge_reason, "Still in Care" = "Still in Care", # add labels for discharge reason
                                        "Adoption/Relative Custody" = "Adoption", # and combine adoption
                                        "Emancipation" = "Emancipation",          # with relative custody
                                        "Adoption/Relative Custody" = "Custody Transfer (Relative)",
                                        "Reunification" = "Reunification",
                                        "Custody Transfer (Agency)" = "Custody Transfer (Agency)")) %>% 
  mutate(care_structure2 = fct_recode(care_structure, # collapse family structure into single/dual parent household
                                      "Single" = "Single mom",
                                      "Single" = "Single dad",
                                      "Dual" = "Married couple",
                                      "Dual" = "Unmarried couple"))

### drop unused levels of race, discharge reason, gender, child disabled, discharge reason
vars = c('race', 'discharge_reason', 'gender', 'child_disabled', 'discharge_reason', 'discharge_reason2')
foster <- foster %>% 
  mutate_at(vars, as.factor) %>% 
  mutate_at(vars, droplevels)


# change get_none to get_support 
names(foster)[names(foster)=="get_none"] <- "get_support"
# mutate get_support so Yes=166 and No=12
foster <- mutate(foster, get_support = fct_recode(get_support, "No"="Yes", "Yes"="No"))

## fix everfind 
foster <- foster  %>% mutate(ever_find = factor(ever_find, ordered = FALSE))

## fix caretake 
foster <- foster %>% 
  mutate(care_structure2 = fct_recode(care_structure,
                                      "Single" = "Single mom",
                                      "Single" = "Single dad",
                                      "Dual" = "Married couple",
                                      "Dual" = "Unmarried couple"))

# creation of variable remove_sum
var <- c(names(foster)[100:114])
foster[var] <- lapply(foster[var], function (x) {fct_recode(x, 
                                                                    "1"="Yes",
                                                                    "0"="No")}) 
foster <- foster %>% 
  mutate_at(var, as.character)
foster <- foster %>% 
  mutate_at(var, as.numeric)
foster <- mutate(foster, 
                     remove_sum=remove_physabuse+remove_sexabuse+remove_neglect+
                       remove_parent_alc+remove_parent_drug+remove_alc+remove_drug+
                       remove_disable+remove_disable+remove_behave+remove_death+
                       remove_jail+remove_cope+remove_cope+remove_abandon+
                       remove_relinq+remove_house)

######################################################################################

### save RDS file for future use - call this for analysis script
saveRDS(foster, file = "foster_clean.rds")
# readRDS("foster_clean.rds") # to load
