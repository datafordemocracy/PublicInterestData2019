######################################################################################
# LP 5440: Public Interest Data Lab 
# Process and prepare referral/CPS data
# 1. Load libraries and data
# 2. Recode CPS variables
# 3. Recode FC variables
# 4. Save for use in visualization and analysis
# Authors: Brago, Conor, Hannah, Stuart, Rishabh, Ana, MPC
# Updated: May 10, 2019 
######################################################################################


# ..........................................................................................
# 1. Load libraries and data ----
library(tidyverse)

# load data 
setwd("/Volumes/PIDL19")
dss <- readRDS("dss.rds")
ref <- readRDS("referral.rds")
# files created in data_clean.R and referral_clean.R

# ..........................................................................................
# 2. Recode CPS variables ----

# age: agemiss
dss2 <- dss %>%
  mutate(age2 = ifelse(is.na(age_ref1), 0, age_ref1),
         agemiss = if_else(is.na(age_ref1), 1, 0))

# race: remove unknown
dss2 <- dss2 %>% 
  filter(race_ethn %in% c("White", "Black", "MultiRace", "Hispanic", "Asian")) %>% 
  mutate(race_ethn = fct_relevel(race_ethn, "Hispanic", after = 3)) %>% 
  mutate(race_ethn = fct_recode(race_ethn, Multirace = "MultiRace"))
dss2$race_ethn <- droplevels(dss2$race_ethn)

# finding: binary 
dss2 <- dss2 %>% 
  mutate(ever_find2 = if_else(ever_find == "No Find", "No", "Yes"))

# numref: binary 3 or more
dss2 <- dss2 %>% 
  mutate(numref3 = if_else(numref > 2, 1, 0))


# ..........................................................................................
# 3. Recode FC variables ----

# care_structure and foster_structure: single, dual
fc <- dss %>% 
  filter(fc_enter == "Yes") %>% 
  mutate(care_structure2 = fct_recode(care_structure,
                                      "Single" = "Single mom",
                                      "Single" = "Single dad",
                                      "Dual" = "Married couple",
                                      "Dual" = "Unmarried couple"),
         foster_structure2 = fct_recode(foster_structure,
                                        "Single" = "Single mom",
                                        "Single" = "Single dad",
                                        "Dual" = "Married couple",
                                        "Dual" = "Unmarried couple")) 

# race: only white, black, multirace
fc <- fc %>% 
  filter(race %in% c("White", "Black", "MultiRace")) %>% 
  mutate(race = fct_recode(race, Multirace = "MultiRace"))
fc$race <- droplevels(fc$race)


# ..........................................................................................
# 4. save for use in visualization and analysis ----
save.image("cps_clean.RData")
