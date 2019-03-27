# Public Interest Data Lab
# Data Cleaning: Full referral data
# March 2019


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
ref <- readRDS("ref_long.rds")
# ref_long.rds created in referral_long.R

# read data dictionary from google sheets using googlesheets
# gs_auth(new_user = TRUE) # need to run initially to give R access to your google drive
gs_datadic <- gs_title("pidl2019_data_dictionary")
datadic <- gs_read(gs_datadic, ws = "referral_full", skip = 8)

ref_copy <- ref # keep a copy of original
# ref <- ref_copy # restore and start again


# ..........................................................................................

# rename the variables ----
# from Ana, Stuart, and Rishabh
for (i in (1:50)){
  colnames(ref)[i] <- datadic$RENAME[i]
}
names(ref)


# ..........................................................................................

# reformatting the CPS variables ----

# to convert to factors
var <- c("gender", "race", "hispanic", "race_ethn", "race2", "gender2", 
         "race_ethn2", "hispanic2", "prior_risk", "region", "locality", 
         "track", "resp_priority", "disp", "safety", "screen_out", "screen_in",
         "invalid_an", "med_neg", "ment_ab", "phys_ab", "phys_neg", "sex_ab",
         "substance_ex", "ab_gender", "ab_race", "response_timely", 
         "response_vic_timely", "prior_ref")
ref <- ref %>% 
  mutate_at(var, as.factor)

# to convert to integers
var <- c("cid", "numref", "refnum", "ref_id", "ref_yr", "ref_cl_id", "age", "ab_cl_id", "ab_age", "numref2")
ref <- ref %>% 
  mutate_at(var, as.integer)

# to convert to characters
var <- "fips_ref"
ref <- ref %>% 
  mutate_at(var, as.character)

# to convert to numeric
var <- c("age_full", "response_days", "response_vic_days")
ref <- ref %>% 
  mutate_at(var, as.numeric)

# what's left? 
# tricky date: ref_dt, ref_m, ab_dob, first_contact, first_vic_contact
# dates <- ref %>% select(cid, dob, dob2, ref_yr, ref_dt, ref_m, age_full, age, ab_dob, ab_age, first_contact, response_days, first_vic_contact, response_vic_days)

# ..........................................................................................

# recoding/leveling the referral variables ----

# recode/relabel factors
# race, ethnicity variables
ref <- ref %>% 
  mutate(race_ethn = fct_recode(race_ethn, "Asian" = "AsianN",
                                "MultiRace" = "Multi-Race"),
         race = fct_recode(race, "MultiRace" = "Multi-Race"),
         race2 = fct_recode(race2, "MultiRace" = "Multi-Race"),
         race_ethn2 = fct_recode(race_ethn2, "MultiRace" = "Multi-Race"),
         hispanic = fct_recode(hispanic, "Hispanic" = "Y",
                               "Non-Hispanic" = "N",
                               "Unknown" = "U"),
         hispanic2 = fct_recode(hispanic, "Hispanic" = "Y",
                                "Non-Hispanic" = "N",
                                "Unknown" = "U"),
         ab_race = fct_recode(ab_race,"MultiRace" = "Multi-Race",
                              "Unknown" = "Declined", 
                              "NHPI" = "Native Hawaiian/Pacific Islander"))

# all the yes, no (except timely)
var <- c("screen_out", "screen_in", "invalid_an", "med_neg", "ment_ab",
         "phys_ab", "phys_neg", "sex_ab", "substance_ex")
ref <- ref %>% 
  mutate_at(.vars = vars(var), 
            .funs = fct_recode,
            "Yes" = "Y", "No" = "N")

# prior risk
ref <- ref %>% 
  mutate(prior_risk = fct_recode(prior_risk, "High" = "Risk Level: High", 
                                "Very High" = "Risk Level: Very High",
                                "Moderate" = "Risk Level: Moderate",
                                "Low" = "Risk Level: Low"),
         prior_risk = fct_relevel(prior_risk, "Very High", "High", "Moderate", "Low"))

# track: CHANGE DSS TO MATCH
ref <- ref %>% 
  mutate(track = fct_recode(track, "assess" = "Family Assessment",
                            "invest" = "Investigation"))

# disp: to match dss (CHANGE DSS TO MATCH)
ref <- ref %>% 
  mutate(disp = fct_recode(disp, "FL1" = "Founded - Level 1",
                           "FL2" = "Founded - Level 2",
                           "FL3" = "Founded - Level 3",
                           "UNF" = "Unfounded - lack of evidence",
                           "UNF" = "Unable to Complete Investigation",
                           "PND" = "Appealed",
                           "PND" = "Pending",
                           "PND" = "Other",
                           "SVC" = "Services Needed",
                           "NSV" = "Services Not Needed"),
         disp = fct_explicit_na(disp, na_level = "None"))

# safety: to align dss and referral responses, match dss
# CSF_, SAF_, USF_ (from matched data); DECISION: CONDITIONALLY SAFE, DECISION: SAFE, DECISION: UNSAFE (from referral data)
ref <- ref %>% 
  mutate(safety = fct_recode(safety, "SAF" = "SAF_",
                             "SAF" = "DECISION: SAFE",
                             "CSF" = "CSF_",
                             "CSF" = "DECISION: CONDITIONALLY SAFE",
                             "USF" = "USF_",
                             "USF" = "DECISION: UNSAFE"),
         safety = fct_explicit_na(safety, na_level = "None"))


# response_timely, response_vic_timely
ref <- ref %>% 
  mutate(response_timely = fct_recode(response_timely,
                                      "No" = "N",
                                      "Yes" = "Y",
                                      "NA" = "N/A",
                                      "NA" = "Data"),
         response_vic_timely = fct_recode(response_vic_timely,
                                          "No" = "N",
                                          "Yes" = "Y",
                                          "NA" = "N/A"))

# re-order factor levels 
# by frequency
ref <- ref %>% 
  mutate(race = fct_infreq(race),
         race_ethn = fct_infreq(race_ethn))

# impose likely correction to track
ref <- ref %>% 
  mutate(track2 = case_when(screen_out == "Yes" ~ "None",
                            screen_out == "No" & track == "invest" ~ "invest",
                            screen_out == "No" & track == "assess" ~ "assess"))
table(ref$track2)

# what's left?


# ..........................................................................................

# save work ----
# save as RDS file for collective use 
#   this is the file to call for subsequent work (analysis, visualization)
saveRDS(ref, file = "referral.rds")
# readRDS("referral.rds") # to load