# Public Interest Data Lab
# Get 2013-2017 ACS child population estimates by race
# April 2019


# install.packages("tidycensus")
library(tidycensus)
library(tidyverse)

setwd("/Volumes/PIDL19")


######################################################################################
# 1. Acquire 2013-2017 5-year Sex by Age ACS estimates for Cville
######################################################################################

# census_api_key("", install = TRUE)
data(fips_codes) # built in dataset for looking up state and county
fips_codes %>% filter(state == "VA") # find county code for CVille

### a. get data: total population (combined) ###
cville_acs_all <- get_acs(geography = "county", table = "B01001", 
                          year = 2017, state = "VA", county = "540", 
                          survey = "acs5", cache_table = TRUE)

### b. get data: white alone ###
cville_acs_white <- get_acs(geography = "county", table = "B01001A", 
                            year = 2017, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### c. get data: Black or African-American alone ###
cville_acs_black <- get_acs(geography = "county", table = "B01001B", 
                            year = 2017, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### d. get data: American Indian and Alaska Native alone ###
cville_acs_ai_an <- get_acs(geography = "county", table = "B01001C", 
                            year = 2017, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### e. get data: Asian alone ###
cville_acs_asian <- get_acs(geography = "county", table = "B01001D", 
                            year = 2017, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### f. get data: Native Hawaiian and Other Pacific Islander alone ###
cville_acs_nh_pi <- get_acs(geography = "county", table = "B01001E", 
                            year = 2017, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### g. get data: Some other race alone ###
cville_acs_other <- get_acs(geography = "county", table = "B01001F", 
                            year = 2017, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### h. get data: Two or more races ###
cville_acs_multi <- get_acs(geography = "county", table = "B01001G", 
                            year = 2017, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### i. get data: white alone, not hispanic or latino ###
cville_acs_whitenh <- get_acs(geography = "county", table = "B01001H", 
                            year = 2017, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### j. get data: hispanic or latino ###
cville_acs_hisp <- get_acs(geography = "county", table = "B01001I", 
                              year = 2017, state = "VA", county = "540", 
                              survey = "acs5", cache_table = TRUE)

# save work
save.image("cville_acs.Rdata")
# load("cville_acs.Rdata")


######################################################################################
# 2. Generate Cville child population estimates by race, create childpop dataframe
######################################################################################
# calculate number of children in cville with moe (age broken into finer intervals for total pop)
kids <- cville_acs_all[c(3:6,27:30),] %>% 
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Total")

# function for generating and adding racial counts
childsum <- function(d,r){
  sum <- d[c(3:6,18:21),] %>% 
    summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
    mutate(race = r)
  tot <- rbind(kids, sum)
}

# apply childsum function to cville_acs_xxxxx dataframes
# kids <- childsum(cville_acs_white, "White")
kids <- childsum(cville_acs_whitenh, "White")
kids <- childsum(cville_acs_black, "Black")
kids <- childsum(cville_acs_multi, "MultiRace")
kids <- childsum(cville_acs_asian, "Asian")
kids <- childsum(cville_acs_ai_an, "ai_an")
kids <- childsum(cville_acs_nh_pi, "nh_pi")
kids <- childsum(cville_acs_other, "Other")
kids <- childsum(cville_acs_hisp, "Hispanic")

# Cville 2013-2017 child pop estimates (race_ethn categories) 
# add ai_an, nh_pi, and other together
kids2 <- kids[c(6,7,8),] %>% 
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Other")

childpop <- rbind(kids[c(1:5,9),], kids2) # add "Other" estimate to total, white, black estimates

# Calculate proportions
totpop <- as_vector(c(childpop[1,1], childpop[1,2]))
childpop <- childpop %>% 
  mutate(prop = estimate/totpop[1], pmoe = moe_prop(estimate, totpop[1], moe, totpop[2]))

# save work
save.image("acs2017.RData")
# load("acs2017.Rdata")


######################################################################################
# 3. Load referral data, generate counts by race
######################################################################################
ref <- readRDS("referral.rds")
# created by referral_long.R and referral_clean.R

# referral counts by race (race4 categories) 3 year total
refpop <- ref %>% 
  mutate(source = "ref") %>% 
  group_by(race_ethn) %>% 
  summarize(source = first(source), number = n_distinct(cid)) %>% 
  mutate(moe = NA,
         race_ethn = fct_recode(race_ethn, "Other" = "Unknown")) %>% 
  rename(race = race_ethn) %>% 
  select(source, everything()) 


######################################################################################
# 4. Create combined data frames for analysis
######################################################################################
# reformat childpop to bind onto refpop (long)
childlong <- childpop %>% 
  mutate(source = "acs17") %>% 
  rename(number = estimate) %>% 
  filter(race != "Total") %>% 
  select(source, race, number, moe)

# bind acs to ref data
acs_ref <- bind_rows(childlong, refpop)
# make race a factor (and order for visualization)
acs_ref <- acs_ref %>% 
  mutate(race = factor(race, levels = c("White", "Black", "Hispanic", "MultiRace", "Asian", "Other")))

# reformat refpop to join onto childpop (wide) ###
refpopwide <- refpop %>% select(source, race, number) %>% 
  spread(key = source, value = number)

# generate total counts for each source 
reftot <- refpopwide %>% 
  summarize_at(vars(ref), list(~ sum(.))) %>% 
  mutate(race = "Total") %>% 
  select(race, ref)

# calculate proportions 
refpopwide <- refpopwide %>% 
  mutate(refprop = ref/reftot$ref)
refpopwide <- bind_rows(refpopwide, reftot)

# join acs and cws
ref_acs <- left_join(childpop, refpopwide, by="race")

# clean up and save work
rm(ref, refpop, refpopwide, reftot, fips_codes, totpop)

save.image("cville_acs.Rdata")
# load("cville_acs.Rdata")

###############################################################################################
# Key data objects
# acs_ref: acs estimates and referral totals by race in long format
# ref_acs: acs estimates and referral totals by race in wide format
###############################################################################################
