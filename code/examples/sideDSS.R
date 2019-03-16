
# C. McClintock
# Supplemental DSS Inquiry
# Cleaning Script

# ..........................................................................................

# load libraries
library(tidyverse)
library(readxl)
library(lubridate)

# ..........................................................................................

# read in the data, drop locality bc all are Charlottesville
clients <- read_excel("dss-investigations-2014-2017.xlsx", sheet=1) %>% select(-Locality)
referrals <- read_excel("dss-investigations-2014-2017.xlsx", sheet=2) %>% select(-Locality)

# ..........................................................................................

# 1. CLEANING

# dimension of the dataframes
dim(clients) # 317 43
dim(referrals) # 1827 10

# rename variables for ease of use
names(clients) <- c("ref_id", "client_id", "case_id", "ssn",
                    "ref_date", "screen_out", "accept", "invest_cat",
                    "disposition", "disp_date", "find_date", "close_date", "birth_date",
                    "gender", "ethnicity", "hispanic", "amer_indian",
                    "asian", "black", "pac_islander", "white", "race_unable",
                    "race_decline", "race_unknown", "age", "no_abuse", "neglect_medical",
                    "abuse_mental", "neglect_physical", "abuse_physical",
                    "abuse_sexual", "fatality", "near_fatal", "abuse_foster", "priority",
                    "first_meaningful", "first_contact", "reporter_relation",
                    "geo_id", "geo_name", "state", "county", "tract")
names(referrals) <- c("ref_id", "client_id", "ref_date", "prev_ref_id", 
                      "prev_ref_date", "prev_screen_out", "prev_accept", "prev_invest_cat",
                      "prev_disposition", "prev_service_code")

# number of distinct id values - clients
n_distinct(clients$ref_id) # 178
n_distinct(clients$client_id) # 257
n_distinct(clients$case_id) # 105

# number of distinct id values - referrals
n_distinct(referrals$ref_id) # 127
n_distinct(referrals$client_id) # 185
n_distinct(referrals$prev_ref_id) # 637

# fewer distinct referral ids, client ids in referrals
# does this mean that 72 clients had no prior referrals?

# look at previous investigation categories
table(referrals$prev_invest_cat)
# some of the prior referrals are also linked to investigations

# count the number of observations linked to each distinct id
arb <- clients %>% group_by(client_id) %>% count()
arb <- referrals %>% group_by(client_id) %>% count()
# 3 clients ids with more than 50 referrals? 
# take 214324262230 as an example, shows up 4 times in clients and 71 times in referrals?
## 3 of the observations attached to 214324262230 have same case id, 1 is different
## ref_id, ref_date are different for all 4, ssn is same for all 4

# join together, do ref_id and client_id match across tables?
refclients <- left_join(referrals, clients, by=c("ref_id", "client_id", "ref_date")) # yes

# are referral dates linked to investigations always after previous referral dates?
refclients <- filter(refclients, ref_date>prev_ref_date) # yes

# difference between investigation linked referral and previous referrals
refclients$ref_date_diff <- with(refclients, 
                                 difftime(ref_date, prev_ref_date, units = "days"))

# drop all referrals not screened in 
refclients <- filter(refclients, prev_accept=="Y")

# drop referrals more than 1 year before investigation linked referral
refclients <- filter(refclients, ref_date_diff<365) # are we sure this is calendar year?

# create a variable for number of previous valid referrals in one year
arb <- refclients %>% group_by(client_id) %>% count() %>% rename("n_prev_ref"="n")
refclients <- left_join(refclients, arb, by="client_id")

# create a binary indicator for more than three valid referrals
refclients$three_ref <- with(refclients, ifelse(n_prev_ref>=3, 1, 0))

# create a variable for previous/no previous referral
clients$prev_ref <- ifelse(clients$client_id %in% referrals$client_id, 
                           "Previous Referral", "No Previous Referral")
# join variables from referrals to clients dataframe
clients <- left_join(clients, unique(select(refclients, ref_id, client_id, 
                      n_prev_ref, three_ref)), by=c("ref_id", "client_id"))

# create a variable for whether an investigation was mandatory or discretionary using:
# 1) three prior valid (screened in) referrals in the past year 
# 2) referral includes allegation of sexual abuse
clients$invest_discretion <- with(clients, ifelse(three_ref=="1"|abuse_sexual=="1", 
                                "Mandatory Investigation", "Discretionary Investigation"))
clients$invest_discretion <- with(clients, ifelse(is.na(invest_discretion), 
                                 "Discretionary Investigation", invest_discretion))
# create a variable specifically for clients with three investigtions, not include SA allegations
clients$invest_discretion3 <- with(clients, ifelse(three_ref=="1", 
                                                  "Mandatory Investigation", "Discretionary Investigation"))
# relevel race factor for visualization
clients <- mutate(clients, ethnicity=fct_relevel(ethnicity, "White", 
                                                 "Multi-Race", "Black", "Asian", "Unknown"))
# ..........................................................................................

# 2. VISUALIZATIONS

# race by level of investigation discretion
g <- ggplot(clients, aes(x=invest_discretion, fill=ethnicity)) + geom_bar(position="fill") + 
  labs(title="Race Distribution by Investigation Type", 
         subtitle="CPS Investigations from July 2014 to June 2017",
         x="Investigation Type", 
         y="Percent of Investigated Cases", fill="Race") + 
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,5,7,8,9)]) + 
  scale_y_continuous(labels = scales::percent) + theme_gray(base_size = 10) +
  annotate("text", x=1, y=c(.89, .69, .30), label=c("22", "19", "58")) +
  annotate("text", x=2, y=c(.88, .64, .28), label=c("23", "25", "52"))
g

# ..........................................................................................

# QUESTIONS: 
# which types of referrals automatically trigger an investigation?

# ..........................................................................................

save.image("sideDSS.RData")




