# Public Interest Data Lab
# Data Cleaning: Matched data
# March 2019
# Contributors: PIDL 2019 Lab


# ..........................................................................................

# load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(googlesheets)


# ..........................................................................................

# read in files ----
# read data from encrypted volume and data dictionary
setwd("/Volumes/PIDL19")
dss <- read_csv("match_cville_CPS_to_FC.csv")
# match_cville_CPS_to_FC.csv created in readfile.R

# read data dictionary from google sheets using googlesheets
# nice explanation: https://datascienceplus.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/
# gs_auth(new_user = TRUE) # need to run initially to give R access to your google drive
gs_datadic <- gs_title("pidl2019_data_dictionary")
datadic <- gs_read(gs_datadic, skip = 8)

dss_copy <- dss # keep a copy of original
# dss <- dss_copy # restore and start again


# ..........................................................................................

# rename the variables ----
# from Ana, Stuart, and Rishabh
for (i in (1:131)){
  colnames(dss)[i] <- datadic$RENAME[i]
}
names(dss)


# ..........................................................................................

# reformatting the CPS variables ----
# adapted from Ana, Stuart, and Rishabh's mutate_if
# and Janie, Alex, and Carolynn's segmentation of variables

# to convert to factors
var <- c("gender", "race", "hispanic", "race_ethn", "screen1", "screen2", 
         "screen3", "screen4", "screen5", "ever_screened", "track1", 
         "track2", "track3", "track4", "track5", "ever_inv", "disp1", 
         "disp2", "disp3", "disp4", "disp5", "safety1", "safety2", 
         "safety3", "safety4", "safety5", "ever_unsafe")
dss <- dss %>% 
  mutate_at(var, as.factor)

# to convert to factors, replace NA with 0, and non NA with 1
var <- c("med_neg", "ment_ab", "phys_ab", "phys_neg", "sex_ab", "substance_ex")
dss <- dss %>% 
  mutate_at(var, list(~ ifelse(is.na(.), 0, 1))) %>% 
  mutate_at(var, as.factor)
  
# to convert to integers
var <- c("cid", "numref", "ref_id1", "ref_id2", "ref_id3", "ref_id4", 
         "ref_id5", "ref_cl_id1", "ref_cl_id2", "ref_cl_id3", "ref_cl_id4", 
         "ref_cl_id5", "ref_yr1", "ref_yr2", "ref_yr3", "ref_yr4", "ref_yr5", 
         "ref_m1", "ref_m2", "ref_m3", "ref_m4", "ref_m5", "ever_find")
dss <- dss %>% 
  mutate_at(var, as.integer)

# to convert to characters
var <- "fips_ref"
dss <- dss %>% 
  mutate_at(var, as.character)

# what's left? age
dss <- dss %>% 
  mutate(age_ref1 = ifelse(age_ref1 %in% c("Data Error", "No Age Given"), 
                                          NA, age_ref1)) %>% 
  mutate(age_ref1 = as.integer(age_ref1))


# ..........................................................................................

# recoding/leveling the CPS variables ----

# recode/relabel factors
# race, ethnicity
dss <- dss %>% 
  mutate(race_ethn = fct_recode(race_ethn, "Asian" = "AsianN",
                                "Multi-Race" = "MultiRace"),
         hispanic = fct_recode(hispanic, "Hispanic" = "Y",
                               "Non-Hispanic" = "N",
                               "Unknown" = "U"))

# all the yes, no
var <- c("med_neg", "ment_ab", "phys_ab", "phys_neg", "sex_ab", 
         "substance_ex", "screen1", "screen2", "screen3", "screen4", 
         "screen5", "ever_screened", "ever_inv", "ever_unsafe")
dss <- dss %>% 
  mutate_at(.vars = vars(var), 
            .funs = fct_recode,
            "Yes" = "1", "No" = "0")

# re-order factor levels 
# by frequency
dss <- dss %>% 
  mutate(race = fct_infreq(race),
         race_ethn = fct_infreq(race_ethn))

# impose likely correction to ever_inv
dss <- dss %>% 
  mutate(ever_inv2 = case_when(ever_screened == "No" ~ "No", 
                               ever_screened == "Yes" & (track1 == "INV_" | track2 == "INV_" | track2 == "INV_" |
                                                   track4 == "INV_" | track5 == "INV_") ~ "Yes", 
                               TRUE ~ "No"))
table(dss$ever_inv2)

# change track to match referral_clean
var <- c("track1", "track2", "track3", "track4", "track5")
dss <- dss %>% 
  mutate_at(.vars = vars(var), 
            .funs = fct_recode,
            "assess" = "FAS_", "invest" = "INV_")

# change disp to match referral_clean
var <- c("disp1", "disp2", "disp3", "disp4", "disp5")
dss <- dss %>% 
  mutate_at(.vars = vars(var),
            .funs = fct_recode,
            "FL1" = "FL1_",
            "FL2" = "FL2_",
            "FL3" = "FL3_",
            "UNF" = "UNF_",
            "SVC" = "SVC_",
            "NSV" = "SNO_")

# change safety to match
var <- c("safety1", "safety2", "safety3", "safety4", "safety5")
dss <- dss %>% 
  mutate_at(.vars = vars(var),
            .funs = fct_recode,
            "CSF" = "CSF_",
            "SAF" = "SAF_",
            "USF" = "USF_")

# ..........................................................................................

# reformatting the FC variables ----
# adapted from Kathryn, Carolyn, Savannah's lapply

# to convert to factors
cols <- c(82:83, 87, 91:97, 99:126, 128)
dss[,cols] <- lapply(dss[cols], as.factor)

# to convert to integers
cols <- c(98, 129:131)
dss[,cols] <- lapply(dss[cols], as.integer)

# to convert to characters
# and add leading 0s
dss <- dss %>% 
  mutate(fips_fc = as.character(fips_fc),
         fips_fc = str_pad(fips_fc, 3, pad = "0"))


# ..........................................................................................

# recoding/leveling the FC variables ----
# recode/relabel factors
# all the yes, no
var <- c("fc_enter", "prior_enter", "child_mr", "child_hearing", 
         "child_physdis", "child_disturbed", "child_othermed", 
         "remove_physabuse", "remove_sexabuse", "remove_neglect", 
         "remove_parent_alc", "remove_parent_drug", "remove_alc", "remove_drug", 
         "remove_disable", "remove_behave", "remove_death", 
         "remove_jail", "remove_cope", "remove_abandon",
         "remove_relinq", "remove_house", "foster_help", "get_adopt",
         "get_afdc", "get_csupport", "get_medicaid", "get_ssi", "get_none")
dss <- dss %>% 
  mutate_at(.vars = vars(var), 
            .funs = fct_recode,
            "Yes" = "1", "No" = "0")

# other variables
# need to add values for: discharge_reason, ever_adopt
dss <- dss %>% 
  mutate(child_disabled = fct_recode(child_disabled,
                                     "Yes" = "1",
                                     "No" = "2",
                                     "Not Determined" = "3"),
         prev_adopt_age = fct_recode(prev_adopt_age,
                                     "No prior adoption" = "0",
                                     "Age 2-5" = "2",
                                     "Age unknown" = "5"),
         removal_type = fct_recode(removal_type,
                                   "Voluntary" = "1",
                                   "Court-ordered" = "2"),
         place_now = fct_recode(place_now,
                                "Pre-adopt home" = "1",
                                "Foster family-kin" = "2",
                                "Foster family-not kin" = "3",
                                "Group home" = "4",
                                "Institution" = "5",
                                "Independent living" = "6",
                                "Trial home visit" = "8"),
         out_state = fct_recode(out_state,
                                "Yes" = "1",
                                "No" = "2"),
         case_goal = fct_recode(case_goal,
                                "Reunification" = "1",
                                "Live with relative" = "2",
                                "Adoption" = "3",
                                "Long-term foster care" = "4",
                                "Emancipation" = "5",
                                "Plan not established" = "7"),
         care_structure = fct_recode(care_structure,
                                     "Married couple" = "1",
                                     "Unmarried couple" = "2",
                                     "Single mom" = "3",
                                     "Single dad" = "4"),
         foster_structure = fct_recode(foster_structure,
                                       "Not applicable" = "0",
                                       "Married couple" = "1",
                                       "Unmarried couple" = "2",
                                       "Single mom" = "3",
                                       "Single dad" = "4"))


# ..........................................................................................

# save work ----
# save as RDS file for collective use 
#   this is the file to call for subsequent work (analysis, visualization)
saveRDS(dss, file = "dss.rds")
# readRDS("dss.rds") # to load

# save image for easier updating 
#   this is the file to call when we need to make changes (e.g., add values/levels)
rm(cols, i, var, gs_datadic)
save.image("dss_clean.RData")
# load("dss_clean.RData") # to load
