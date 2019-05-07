### Kathryn Bernard, Carolyn Ours, Savannah Quick
### Cleaning Script - PIDL '19
### April 26, 2019


###### Kathryn ###### 

### set working directory to encrypted volume
setwd("/Volumes/PIDL19")

### load packages needed for cleaning
library(tidyverse)
library(lubridate)

### read in data created in original cleaning script
dss <- readRDS("dss.rds")

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
                           "Multiracial" = "MultiRace")) %>%  # change to language used in report
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


### save RDS file for future use - call this for analysis script
saveRDS(foster, file = "foster_clean.rds")
# readRDS("foster_clean.rds") # to load


