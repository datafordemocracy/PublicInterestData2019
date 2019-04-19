
# Public Interest Data Lab
# Placement History Sequence Analysis
# April 19, 2019

# .....................................................................................

# load libraries
library(tidyverse)
library(lubridate)
library(TraMineR)
library(TraMineRextras)

# set working directory
setwd("../../")
setwd("Volumes/PIDL19")

# load data
ph <- readRDS("placement.Rds")

# .....................................................................................

# CLEANING

# subset for useful variables
ph <- select(ph,  names(ph)[c(1:14,99:115, 133:139)])
# reorder for ease of use
ph <- select(ph, cid, case_id, placenum, place_date, leave_date, 
             leave_reason, resourceID, typeofcare, num_place, everything())
# subset to children in care
ph <- subset(ph, !is.na(removal_total))

# are the NAs consistent?
sum(is.na(ph$place_date)) # 4035
sum(is.na(ph$leave_date)) # 4134
sum(is.na(ph$resourceID)) # 4038
sum(is.na(ph$typeofcare)) # 4035
# place_date and typeofcare are lowest and match, subset on those

ph <- subset(ph, !is.na(place_date))

ph <- arrange(ph, cid, placenum)

# clean dates
ph <- mutate(ph, 
             place_date=dmy(place_date),
             leave_date=dmy(leave_date))

# time spent in each foster care placement
ph <- ph %>% 
  mutate(place_length = as.numeric(difftime(leave_date, place_date, unit = "weeks")))

# time spent in each foster care placement
tic_client <- ph %>% 
  group_by(case_id, cid)
time_total <- summarise(tic_client, duration = sum(place_length))
ph <- left_join(ph, time_total, by=c("case_id", "cid"))

# if the child exited the placement
ph$exit_place <- ifelse(!is.na(ph$leave_date), 1, 0)


ph <- mutate(ph, 
             leave_simp=fct_recode(leave_reason, 
                                   "Custody Transfer"="Custody Transfer to a Non Relative", 
                                   "Custody Transfer"="Custody Transfer to Another Agency",
                                   "Custody Transfer"="Custody Transfer to Other Relative",
                                   "Custody Transfer"="Custody Transfer to other relative (with KinGAP)",
                                   "Custody Transfer"="Custody Transfer to other relative (without KinGAP)",
                                   "Placement Cannot Meet Child Needs"="Placement Cannot Meet Childs Behavioral Needs",
                                   "Placement Cannot Meet Child Needs"="Placement Cannot Meet Childs Medical Needs",
                                   "Other"="Placement Temporarily Unable to Care for Child"
                                   ))



# .....................................................................................

# EXPLORATORY ANALYSIS

# entries before 3 year window
ggplot(ph, aes(place_date)) + geom_density(alpha=0.5) + theme_minimal()

# number of placements
ph %>% group_by(race) %>% summarize(mean=mean(num_place, na.rm=T), 
                                    median=median(num_place, na.rm=T))
ggplot(ph, aes(num_place, fill=race)) + geom_density(alpha=0.5) + theme_minimal()

# length of each placement
ph %>% group_by(race) %>% summarize(mean=mean(place_length, na.rm=T), 
                                    median=median(place_length, na.rm=T))
ggplot(ph, aes(place_length, fill=race)) + geom_density(alpha=0.5) + theme_minimal()

# duration of OOH care
ph %>% group_by(race) %>% summarize(mean=mean(duration, na.rm=T), 
                                    median=median(duration, na.rm=T))
ggplot(ph, aes(duration, fill=race)) + geom_density(alpha=0.5) + theme_minimal()

# .....................................................................................

# SEQUENCE ANALYSIS

# follow people through the three year period? # limit to three year period

# create time stamped event df
tse <- select(ph, cid, leave_date, leave_simp)

# create an ordered factor of time by month and year
tse$month <- month(tse$leave_date)
$year <- year(tse$leave_date)
tse <- unite(tse, "ym", year, month, sep="-")
tse <- filter(tse, !ym=="NA-NA")

# convert time factor to sequence index
tse$time <- as.numeric((factor(tse$ym)))

# drop extra
tse <- select(tse, cid, time, leave_simp)

# attempt to convert to STS
events <- levels(tse$leave_simp)
stm <- seqe2stm(events, dropList=list("Adoption"=events[-1],
                                      "Allegations Against Provider"=events[-2], 
                                      "Child Requested Change of Placement"=events[-3],
                                      "Custody Transfer"=events[-4],
                                      "Emancipation"=events[-5],
                                      "Independent Living"=events[-6],
                                      "Other"=events[-7],
                                      "Placement Cannot Meet Child Needs"=events[-8],
                                      "Placement With Siblings/Proximity To Family"=events[-9],
                                      "Provider Approval Ended"=events[-10],
                                      "Provider Request"=events[-11],
                                      "Reunification"=events[-12] ))

mysts <- TSE_to_STS(tse, id=1, timestamp=2, event=3,
                    stm=stm, tmin=1, tmax=65, firstState="None")

my.seq <- seqdef(mysts)
seqdplot(my.seq)

# .....................................................................................

# NOTES: 
# Are we removing Asian and Unknown children? Keeping them as other?

# .....................................................................................

# SEQUENCE EXAMPLES

# example conversion from TSE to STS
data(actcal.tse)
events <- c("PartTime", "NoActivity", "FullTime", "LowPartTime")

## States defined by last occurred event (forgetting all previous events).
stm <- seqe2stm(events, dropList=list("PartTime"=events[-1],
                                      NoActivity=events[-2], FullTime=events[-3],
                                      LowPartTime=events[-4]))

mysts <- TSE_to_STS(actcal.tse[1:100,], id=1, timestamp=2, event=3,
                    stm=stm, tmin=1, tmax=12, firstState="None")

my.seq <- seqdef(mysts)
seqdplot(my.seq)

# example working with STS
data(mvad)
seqstatl(mvad[, 17:86])
mvad.alphabet <- c("employment", "FE", "HE", "joblessness", "school", "training")
mvad.labels <- c("employment", "further education", "higher education", 
                 "joblessness", "school", "training")
mvad.scodes <- c("EM", "FE", "HE", "JL", "SC", "TR")
mvad.seq <- seqdef(mvad, 17:86, alphabet = mvad.alphabet, states = mvad.scodes, 
                   labels = mvad.labels, xtstep = 6)
seqIplot(mvad.seq, sortv = "from.start", with.legend = FALSE)
