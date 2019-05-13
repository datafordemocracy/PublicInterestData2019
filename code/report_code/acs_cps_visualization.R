######################################################################################
# LP 5440: Public Interest Data Lab 
# Comparing CPS to ACS/Population data
# 1. Load libraries and data
# 2. Set color palette 
# 3. Referral disproportionality by population
# Authors: Stuart, Rishabh, Ana, MPC
# Updated: May 10, 2019 
######################################################################################


# ..........................................................................................
# 1. Load libraries and data
# load libraries
library(tidyverse)

# load data
setwd("/Volumes/PIDL19")
acs <- load("cville_acs.Rdata")
# files generated in acsdata.R


# ..........................................................................................
# 2. Set color palette ----
colorviz5 <- c("#384c7d","#755391","#b25590","#e35d7c","#fe785b")
colorviz5_rev <- c("#fe785b","#e35d7c", "#b25590", "#755391", "#384c7d")


# ..........................................................................................
# 3. Referral disproportionality by population ----

# relevel race in acs
acs_ref <- acs_ref %>% 
  mutate(race = fct_relevel(race, "MultiRace", after = 2))

# stacked barplot for comparison
acs_ref %>% filter(race != "Other") %>%  
  ggplot(aes(y = number, x = source)) +
  geom_bar(stat = "identity", aes(fill=race), position = "fill") +
  labs(title = "Population Proportions and Referral Proportions by Race",
       subtitle = "Population proportions from 2013-2017 American Community Survey",
       y = "", x = "", fill = "Race") + 
  scale_fill_manual(values=colorviz5_rev) +
  scale_x_discrete(labels=c("acs17" = "2013-2017 Population", "ref" = "2015-2017 Referrals")) +
  scale_y_continuous(labels = scales::percent) +
  annotate("text", x = 1, y = c(.02, .10, .20, .46, .95), 
           label = c("4.7 (±- 1.4)", "10.2 (± 1.3)", "8.2 (± 2.5)", "26.4 (± 3.2)", "50.8 (± 0.9)"), color = "white") + 
  annotate("text", x = 2, y = c(.01, .08, .16, .72, .95), 
           label = c("0.8", "6.1", "10.9", "56.0", "24.0"), color = "white")
ggsave("figures/ref_acs.pdf", width=9, height=6, units="in")   

# Disproportionality Index 
#   ratio of % of [race of] children referred to CWS to % of [race of] children in population
#   e.g., % referred to CWS who are white/% children in Cville who are white
#   calculate based on estimated proportion and lower/upper bound of estimated proportion

# generate intervals
ref_acs <- ref_acs %>% 
  mutate(rd_lo = refprop/(prop - pmoe),
         rd_mi = refprop/prop,
         rd_hi = refprop/(prop + pmoe))

# visualize
ref_acs2 <- ref_acs %>% 
  filter(race != "Total" & race != "Other") %>% 
  arrange(match(race, c("White", "Black", "MultiRace", "Hispanic", "Asian"))) %>% 
  mutate(race = factor(race, levels = c("White", "Black", "MultiRace", "Hispanic", "Asian")))

ggplot(ref_acs2, aes(x=fct_rev(race), y = rd_mi, fill = race, color=race)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = rd_hi, ymax = rd_lo), width = .2, color = "black") +
  geom_hline(yintercept = 1, color = "black") +
  scale_y_continuous(name = "Disproportionality Index (90% Confidence Intervals)", 
                     trans = "log",
                     breaks = c(0.125, 0.25, 0.33, 0.5, 0.67, 1, 1.5, 2, 3), 
                     labels = c("0.125", "0.25", "0.33", "0.5", "0.67", "1", "1.5", "2", "3")) +
  scale_color_manual(values=colorviz5_rev) +
  scale_fill_manual(values=colorviz5_rev) +
  expand_limits(y = 3) +
  labs(title = "Racial Disproportionality Index in Referrals",
       subtitle = "Based on population proportions from 2013-2017 American Community Survey",
       y = "", x = "", fill = "race") +
  annotate("text", x = c(1,2,3,4,5), y = c(0.85, 0.85, 1.15, 1.15, 0.85), label = c("0.18", "0.60", "1.34", "2.12", "0.47"), color = "white") +
  coord_flip()
ggsave("figures/ref_acs_disp.pdf", width=9, height=6, units="in")   
