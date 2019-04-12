# Public Interest Data Lab
# Racial disproportionality in referrals
# April 2019


library(tidyverse)
library(RColorBrewer)
library(scales)

setwd("/Volumes/PIDL19")
load("cville_acs.Rdata")
# created by acsdata.R


######################################################################################
# 1. Referrals vs. population figure
######################################################################################

acs_ref %>% filter(race != "Other") %>%  
  ggplot(aes(y = number, x = source)) +
  geom_bar(stat = "identity", aes(fill = race), position = "fill") +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,5,6,7,8)]) +
  labs(title = "Population Proportions and Referral Proportions by Race",
       subtitle = "Population proportions from 2013-2017 American Community Survey",
       y = "", x = "", fill = "Race") + 
  scale_x_discrete(labels=c("acs17" = "2013-2017 Population", "ref" = "2015-2017 Referrals")) +
  scale_y_continuous(labels = percent) +
  annotate("text", x = 1, y = c(.02, .10, .20, .46, .95), 
           label = c("4.7 (±- 1.4)", "8.2 (± 2.5)", "10.2 (± 1.3)", "26.4 (± 3.2)", "50.8 (± 0.9)"), color = "white") + 
  annotate("text", x = 2, y = c(.02, .09, .16, .72, .95), 
           label = c("0.8", "10.9", "6.1", "56.0", "24.0"), color = "white")


####################################################################################
# 2. Racial disproportionality index for referrals 
####################################################################################
# Disproportionality: 
#   ratio of % of [race of] children referred to CWS to % of [race of] children in population
#   e.g., % referred to CWS who are white/% children in Cville who are white
#   calculate based on estimated proportion and lower/upper bound of estimated proportion

# generate intervals
ref_acs <- ref_acs %>% 
  mutate(rd_lo = refprop/(prop - pmoe),
         rd_mi = refprop/prop,
         rd_hi = refprop/(prop + pmoe))

# visualize
ref_acs %>% filter(race != "Total" & race != "Other") %>% 
  ggplot(aes(x = race, y = rd_mi, fill = race)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = rd_hi, ymax = rd_lo), width = .2) +
  geom_hline(yintercept = 1, color = "black") +
  scale_y_continuous(name = "Disproportionality Index (90% Confidence Intervals)", 
                     trans = "log", # log transformation
                     breaks = c(0.125, 0.25, 0.33, 0.5, 0.67, 1, 1.5, 2, 3), 
                     labels = c("0.125", "0.25", "0.33", "0.5", "0.67", "1", "1.5", "2", "3")) +
  scale_fill_manual(values = brewer.pal(9, "Blues")[c(4,5,6,7,8)]) +
  expand_limits(y = 3) +
  labs(title = "Racial Disproportionality Index in Referrals",
       subtitle = "Based on population proportions from 2013-2017\n American Community Survey",
       y = "", x = "", fill = "Race") +
  annotate("text", x = c(1,2,3,4,5), y = c(.85, 1.15, .85, 1.15, .85), label = c("0.18", "2.12", "0.60", "1.34", "0.47"), color = "white") +
  coord_flip()

