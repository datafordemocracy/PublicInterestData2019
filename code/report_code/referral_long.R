# Public Interest Data Lab
# March 26, 2019
# Work with full referral data; make long

# use full referral
library(tidyverse)
setwd("/Volumes/PIDL19")

ref <- read_csv("referral.csv")
# referral.csv created in readfile.R

# select just cps data
# SafetyDecision.1 through SafetyDecision.5 missing from referral.csv
# read in initial matched data and add these 5 columns
dss <- read_csv("match_cville_CPS_to_FC.csv")
# match_cville_CPS_to_FC.csv created in readfile.R

dss_safety <- dss %>% 
  select(CounterID, SafetyDecision.1:SafetyDecision.5)

ref <- left_join(ref, dss_safety) # join
ref <- ref %>% select(-c(AbuserClientID.1:AbuserPrimaryEthnicity.26)) # remove uinintended ariables

cols <- c(197:248,660:685, 738:763) # REFER_DT, Referral_Month_Year, FirstContractDateTime, FirstVictimContactDateTime
ref[,cols] <- lapply(ref[cols], as.character) # save dates as character for later reformatting

ref <- ref %>% select(CounterID, FIPS, DOB, Gender, Race, Hispanic, # reorder
                      Race_Ethnicity, RaceShort_Victim:OverallDisposition.26, 
                      SafetyDecision.1:SafetyDecision.5, 
                      SafetyDecision.6:PriorReferralExistsWithinYear.26)

# reshape to long
reflong <- ref %>% 
  gather(Region.1:PriorReferralExistsWithinYear.26, key = key, value = value) %>% 
  separate(key, sep = "\\.", into = c("var", "num")) %>% 
  split(.$var) %>% 
  map(~mutate(., !!.$var[1] := value)) %>%  map(~select(., -var, -value)) %>%
  Reduce(f = merge, x = .) %>%
  mutate(num = as.numeric(num)) %>% 
  arrange(CounterID, num) 

# filter out empty rows and reorder variables (to original sequence)
reflong <- reflong %>% 
  filter(!is.na(Region) & !is.na(Locality)) %>% 
  select(CounterID:num, Region, Locality, REFER_ID, ReferralYear, REFER_DT, 
         Refer_Month_Year, CAS_ID, TrackAssignment, ResponsePriority, 
         OverallDisposition, SafetyDecision, SCRN_OUT_REFER_SW, ACPT_REFER_SW, 
         VictimAgeAtReferral, VicAgeTrunc, InvalidAN, MedicalNeglect, 
         MentalAbuseNeglect, PhysicalAbuse, PhysicalNeglect, SexualAbuse, 
         SubstanceExposedInfants, 
         FirstContactDateTime, ResponseTimedays, ResponseTimeTimely, 
         FirstVictimContactDateTime, VictimResponseTimedays, 
         VictimResponseTimeTimely, PriorReferralExistsWithinYear)

# check if number of records per child equals NbrRefs
reflong <- reflong %>% 
  group_by(CounterID) %>% 
  mutate(numref = n()) %>% 
  ungroup()

sum(reflong$numref == reflong$NbrRefs) # yes

# appears to be some duplicates, let's check it out
identical(reflong[['DOB']],reflong[['VictimBirthDate']]) # identical
identical(reflong[['Race']],reflong[['RaceShort_Victim']]) # not identical
identical(reflong[['Gender']],reflong[['VictimGender']]) # identical
identical(reflong[['Hispanic']],reflong[['HISPANIC_SW']]) # not identical # no diff found?
identical(reflong[['Race_Ethnicity']],reflong[['VictimPrimaryEthnicity']]) # not identical
identical(reflong[['numref']],reflong[['NbsRefs']]) # not identical # no diff found?

# no differences found?
which(!reflong$Hispanic==reflong$HISPANIC_SW) # not different
which(!reflong$numref==reflong$NbrRefs) # not different
which(!reflong$Race_Ethnicity==reflong$VictimPrimaryEthnicity) # different

# drop duplicate and unnecessary variables
reflong <- select(reflong, -c(VictimBirthDate, VictimGender, HISPANIC_SW, NbrRefs))

# save file for cleaning and analysis
saveRDS(reflong, file = "ref_long.rds")
# readRDS("ref_long.rds") # to load

# adapted from stackoverflow example
# https://stackoverflow.com/questions/53146553/r-gather-multiple-columns-in-multiple-key-columns-with-tidyr
