# Public Interest Data Lab
# Read in encrypted excel files
# February 2019


# ..........................................................................................

# load libraries ----
library(tidyverse)
library(XLConnect)
# needed to first update JDK: https://www.oracle.com/technetwork/java/javase/downloads/jdk11-downloads-5066655.html
# install.packages("XLConnect")
# install.packages("XLConnectJars")
# install.packages('rJava', type='source') # this didn't work, followed instructions here: https://stackoverflow.com/questions/47658210/loading-rjava-on-mac-os-high-sierra

setwd("/Volumes/PIDL19")


# ..........................................................................................

# read initial file, save as csv ----
wb <- loadWorkbook("initialfiles/Match Cville CPS to FC.xlsx", password="WaHoWa2152019")
ws <- readWorksheet(wb, "Match Cville CPS to FC 2015-201")

str(ws)
summary(ws)

# generate data file to share
write_csv(ws, "match_cville_CPS_to_FC.csv")


# ..........................................................................................

# read placement history file, save as csv ----
wb2 <- loadWorkbook("initialfiles/Match feb14_PlcData Mar14.xls", password="WaHoWa")
ws2 <- readWorksheet(wb2, "Match feb14pm with key_detailed")

str(ws2)
summary(ws2)

# generate data file to share
write_csv(ws2, "placement.csv")


# ..........................................................................................

# read full referral file, save as csv ----
wb3 <- loadWorkbook("initialfiles/Match feb14pm_Add all referrals_De-identified.xlsx", password="WaHoWa")
ws3 <- readWorksheet(wb3, "Match feb14pm_Add all referrals")

str(ws3)
summary(ws3)

# generate data file to share
write_csv(ws3, "referral.csv")
