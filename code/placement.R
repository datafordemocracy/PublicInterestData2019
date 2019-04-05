# Public Interest Data Lab
# Reshape: placement & referral level data
# March 28, 2019

# .....................................................................................

# get working directory
setwd("Volumes/PIDL19")

# load library
library(tidyverse)
library(readxl)
library(lubridate)

# load clean dss data
load("dss_clean.RData")

# .....................................................................................

# Reshape placement history data

# read in the data
ph <- read_csv("dss-s19-refs.csv")

# check variable names for this df and for the original
names(ph)[1:131]
names(dss)

# names match, use dss to assign to matching ph columns
names(ph)[1:131] <- names(dss)[1:131]

# reshape placement variables to long
ph <- gather(ph, PlcDate.1:ResourceID.22, key="var", value="val")

# separate variable names in var column from placement number
ph <- separate(ph, var, sep="\\.", into=c("var", "placenum"))

# reorder variables for clarity
ph <- select(ph, var, placenum, val, everything())

# push var to variables 
ph <- spread(ph, var, val)

# rename placement categories
names(ph)[133:139] <- c("case_id", "num_place", "leave_date", 
                        "leave_reason", "place_date", "resourceID",
                        "typeofcare")


# ..........................................................................................

names(ph)
# to convert to factors
var <- c("gender", "race", "hispanic", "race_ethn", "screen1", "screen2", 
         "screen3", "screen4", "screen5", "ever_screened", "track1", 
         "track2", "track3", "track4", "track5", "ever_inv", "disp1", 
         "disp2", "disp3", "disp4", "disp5", "safety1", "safety2", 
         "safety3", "safety4", "safety5", "ever_unsafe")
ph <- ph %>% 
  mutate_at(var, as.factor)

# to convert to factors, replace NA with 0, and non NA with 1
var <- c("med_neg", "ment_ab", "phys_ab", "phys_neg", "sex_ab", "substance_ex")
ph <- ph %>% 
  mutate_at(var, list(~ ifelse(is.na(.), 0, 1))) %>% 
  mutate_at(var, as.factor)

# to convert to integers
var <- c("cid", "numref", "ref_id1", "ref_id2", "ref_id3", "ref_id4", 
         "ref_id5", "ref_cl_id1", "ref_cl_id2", "ref_cl_id3", "ref_cl_id4", 
         "ref_cl_id5", "ref_yr1", "ref_yr2", "ref_yr3", "ref_yr4", "ref_yr5", 
         "ref_m1", "ref_m2", "ref_m3", "ref_m4", "ref_m5", "ever_find")
ph <- ph %>% 
  mutate_at(var, as.integer)

# to convert to characters
var <- "fips_ref"
ph <- ph %>% 
  mutate_at(var, as.character)

# what's left? age
ph <- ph %>% 
  mutate(age_ref1 = ifelse(age_ref1 %in% c("Data Error", "No Age Given"), 
                           NA, age_ref1)) %>% 
  mutate(age_ref1 = as.integer(age_ref1))

# recode/relabel factors
# race, ethnicity
ph <- ph %>% 
  mutate(race_ethn = fct_recode(race_ethn, "Asian" = "AsianN",
                                "Multi-Race" = "MultiRace"),
         hispanic = fct_recode(hispanic, "Hispanic" = "Y",
                               "Non-Hispanic" = "N",
                               "Unknown" = "U"))

# all the yes, no
var <- c("med_neg", "ment_ab", "phys_ab", "phys_neg", "sex_ab", 
         "substance_ex", "screen1", "screen2", "screen3", "screen4", 
         "screen5", "ever_screened", "ever_inv", "ever_unsafe")
ph <- ph %>% 
  mutate_at(.vars = vars(var), 
            .funs = fct_recode,
            "Yes" = "1", "No" = "0")

# re-order factor levels 
# by frequency
ph <- ph %>% 
  mutate(race = fct_infreq(race),
         race_ethn = fct_infreq(race_ethn))

# to convert to factors
cols <- c(82:83, 87, 91:97, 99:126, 128)
ph[,cols] <- lapply(ph[cols], as.factor)

# to convert to integers
cols <- c(98, 129:131)
ph[,cols] <- lapply(ph[cols], as.integer)

# to convert to characters
# and add leading 0s
ph <- ph %>% 
  mutate(fips_fc = as.character(fips_fc),
         fips_fc = str_pad(fips_fc, 3, pad = "0"))

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

ph[var] <- lapply(ph[var], function(x) factor(x))

ph <- ph %>% 
  mutate_at(.vars = vars(var), 
            .funs = fct_recode,
            "Yes" = "1", "No" = "0")

# other variables
# need to add values for: discharge_reason, ever_adopt
ph <- ph %>% 
  mutate(child_disabled = fct_recode(child_disabled,
                                     "Yes" = "1",
                                     "No" = "2",
                                     "Not Determined" = "3"),
         prev_adopt_age=as.factor(prev_adopt_age),
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

var <- c("child_disabled", "prev_adopt_age", "removal_type", "place_now", "out_state",
         "case_goal", "care_structure", "foster_structure")
lapply(ph[var], function(x) class(x)) # all factors

# remove extraneous objects and save
saveRDS(ph, file = "placement.rds")
# readRDS("placement.rds") # to load
