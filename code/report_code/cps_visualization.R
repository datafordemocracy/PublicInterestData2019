######################################################################################
# LP 5440: Public Interest Data Lab 
# Generate baseline comparisons figures for report
# 1. Load libraries and data
# 2. Set color palette 
# 3. Number of referrals
# 4. Ever screened in, investigated, finding, unsafe
# 5. Age at first referral
# 6. Foster care variables: family structure
# Authors: Brago, Conor, Hannah, Stuart, Rishabh, Ana, MPC
# Updated: May 10, 2019 
######################################################################################


# ..........................................................................................
# 1. Load libraries and data ----
# load libraries
library(tidyverse)

# load data
setwd("/Volumes/PIDL19")
load("cps_clean.RData")
# files created in cps_clean.R


# ..........................................................................................
# 2. Set color palette ----
colorviz3 <- c("#b25590","#e35d7c","#fe785b")
colorviz3_rev <- c("#fe785b","#e35d7c","#b25590")
colorviz5 <- c("#384c7d","#755391","#b25590","#e35d7c","#fe785b")
colorviz5_rev <- c("#fe785b","#e35d7c", "#b25590", "#755391", "#384c7d")


# ..........................................................................................
# 3. Number of referrals ----
# average number of referrals by race
dss2 %>% group_by(race_ethn) %>% summarize(mean(numref))
# analysis of variance/difference of means test 
num_aov <- aov(numref ~ race_ethn, data = dss2) 
num_aov
# pairwise differences
TukeyHSD(num_aov)

# remove three potential outliers
dss2 %>% filter(numref < 12) %>% 
  group_by(race_ethn) %>% summarize(mean(numref))
# analysis of variance/difference of means test 
num_aov2 <- aov(numref ~ race_ethn, data = filter(dss2, numref < 12)) 
num_aov2
# pairwise differences
TukeyHSD(num_aov2)


# visualization
ggplot(dss2, aes(x = race_ethn, y = numref, fill = race_ethn)) + 
  geom_bar(stat = "summary", fun.y = "mean") +
  scale_fill_manual(values = colorviz5_rev) +
  labs(title = "Average Number of Referrals for Referred Children by Race",
       subtitle = "Among Children Referred to CPS between January 2015 and December 2017",
       y = "", x = "") +
  annotate("text", x = 2, y = 2.1, color = "black", label = " * ", size=8) +
  annotate("text", x = 3, y = 2.8, color = "black", label = " * ", size=8) +
  theme(legend.position = "none") +
  geom_label(x = 4.6, y = 2.5,  inherit.aes=FALSE,
             label = "Note: * represents significant\ndifference from White and Hispanic\nchildren at p < .10", 
             label.size = .25, size = 4)
ggsave("figures/numref_race.pdf", width=9, height=6, units="in")   

# alternative visualization (with 5 highest outliers removed)
ggplot(filter(dss2, numref < 12), aes(x = race_ethn, y = numref)) + 
  geom_jitter(position=position_jitter(0.1), aes(color = race_ethn), alpha = 1/3) +
  scale_color_manual(values = colorviz5_rev) +
  stat_summary(fun.y = mean, geom = "point", shape = 18,
                 size = 3, color = "black") 


# ..........................................................................................
# 4. Ever screened in, investigated, finding, unsafe ----
ever <- dss2 %>% 
  group_by(race_ethn) %>% 
  summarize(Refer = n(),
            Screen = sum(ever_screened == "Yes"),
            Investigate = sum(ever_inv2 == "Yes"),
            Finding = sum(ever_find2 == "Yes"),
            Unsafe = sum(ever_unsafe == "Yes"),
            Removal = sum(fc_enter == "Yes"))

# totals to print on figure
totals <- ever %>% summarise(refer = sum(Refer), screen = sum(Screen),
                             investigate = sum(Investigate), finding = sum(Finding),
                             unsafe = sum(Unsafe), removal = sum(Removal))
vtotals <- as_vector(totals[1,])

# reshape for graphing
ever_long <- gather(ever, key = "outcome", value = "count", -race_ethn)
ever_long <- ever_long %>% 
  mutate(outcome = factor(outcome, levels = c("Refer", "Screen", "Investigate", "Finding", "Unsafe", "Removal")))

# plot
ggplot(ever_long, aes(x = outcome, y = count, fill = race_ethn)) + 
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values=colorviz5_rev) + 
  labs(title = "Racial Composition of Children Subject to Decision",
       subtitle = "Among Children Referred to CPS between January 2015 and December 2017",
       y = "Proportion", x = "Decision", fill = "Race") +
  annotate("text", x = c(1,2,3,4,5,6), y = 1.03, label = vtotals) +
  annotate("text", x = 1, y = c(.95, .70, .135, .05, .01),
           label = c("24.5", "57.2", "11.2", "6.2", "0.9"), color = "white") +
  annotate("text", x = 2, y = c(.95, .70, .135, .05, .01), 
           label = c("22.0", "57.9", "12.6", "6.4", "1.1"), color = "white") +
  annotate("text", x = 3, y = c(.95, .70, .135, .05, .01),
           label = c("19.7", "58.9", "15.8", "5.1", "0.6"), color = "white") +
  annotate("text", x = 4, y = c(.95, .70, .135, .05, .01),
           label = c("19.7", "54.2", "19.2", "5.6", "1.1"), color = "white") +
  annotate("text", x = 5, y = c(.95, .70, .135),
           label = c("25.9", "57.4", "16.7"), color = "white") +
  annotate("text", x = 6, y = c(.95, .70, .135),
           label = c("22.2", "48.3", "27.2"), color = "white") 
ggsave("figures/ever_race.pdf", width=9, height=6, units="in")   

# proportions for figure
# refer
prop.table(table(dss2$race_ethn))
# screen
prop.table(table(dss2$race_ethn, dss2$ever_screened), margin = 2)
chisq.test(dss2$ever_screened, dss2$race_ethn, simulate.p.value = TRUE)  
# investigate
prop.table(table(dss2$race_ethn, dss2$ever_inv2), margin = 2)
chisq.test(dss2$ever_inv2, dss2$race_ethn, simulate.p.value = TRUE)  
# finding
prop.table(table(dss2$race_ethn, dss2$ever_find2), margin = 2)
chisq.test(dss2$ever_find2, dss2$race_ethn, simulate.p.value = TRUE)  
# unsafe
prop.table(table(dss2$race_ethn, dss2$ever_unsafe), margin = 2)
chisq.test(dss2$ever_unsafe, dss2$race_ethn, simulate.p.value = TRUE)  
# removal
prop.table(table(dss2$race_ethn, dss2$fc_enter), margin = 2)
chisq.test(dss2$fc_enter, dss2$race_ethn, simulate.p.value = TRUE)  


# ..........................................................................................
# 5. Age at referral ---

# Density graph for age at referral by race
dss2 %>% ggplot(aes(x= age_ref1, fill= race_ethn)) + geom_density(alpha= 0.5) +
  labs(x= 'Age at Referral', fill= 'Race', title= 'Age Density by Race')
# note peaks at younger ages for black and multirace

# Density graphy for age at first referral by race, facets
ggplot(dss2, aes(age_ref1)) + geom_histogram(aes(y=..density..), binwidth=1, fill = "blue") +
  geom_density(col = "orange") + facet_wrap(~ race_ethn)
# highlights the 0 spike present for al groups (except Hispanic), 
# but most pronounced for multiracial children

# Average age at first referral by race 
dss2 %>% group_by(race_ethn) %>% 
  summarize(mean(age_ref1, na.rm = TRUE))
# analysis of variance/difference of means
age_aov <- aov(age_ref1 ~ race_ethn, data = dss2) 
age_aov
summary(age_aov)
# pairwise differences
TukeyHSD(age_aov)
# black children younger than white, Hispanic children
# Multirace children younger than white, Hispanic, Asian children


# ..........................................................................................
# 6. Foster care variables: family structure -----

# family structure
ggplot(fc, aes(x = care_structure2, fill = race)) + 
  geom_bar(position = "fill") +
  scale_fill_manual(values=colorviz3_rev) +
  labs(title = "Family Structure of Children Removed from Home",
       subtitle = "Among Children Referred to CPS between January 2015 and December 2017",
       y = "Proportion", x = "Family Structure", fill = "Race") +
  annotate("text", x = 1, y = c(.95, .60, .08),
           label = c("32.7", "55.8", "11.5"), color = "white") +
  annotate("text", x = 2, y = c(.95, .60, .08),
           label = c("19.8", "49.2", "31.0"), color = "white")
  
# proportions for figure
prop.table(table(fc$race, fc$care_structure2), margin = 2)  
chisq.test(fc$care_structure2, fc$race, simulate.p.value = TRUE)  
prop.table(table(fc$race, fc$care_structure2), margin = 1)  

# structured with race as x
ggplot(fc, aes(x = race, fill = race)) + 
  geom_bar(position = "fill", aes(alpha = care_structure2)) +
  scale_alpha_manual(values = c(1, .75), name = "Family Structure") +
  scale_fill_manual(values=colorviz3_rev) +
  labs(title = "Family Structure of Children Removed from Home by Race",
       subtitle = "Among Children Referred to CPS between January 2015 and December 2017",
       y = "Proportion", x = "Race of Child", fill = "Family Structure") +
  guides(fill = FALSE) +
  annotate("text", x = 1, y = c(.95, .5),
           label = c("40.5", "59.5"), color = "white") +
  annotate("text", x = 2, y = c(.95, .5),
           label = c("31.9", "68.1"), color = "white") +
  annotate("text", x = 3, y = c(.95, .5),
           label = c("13.3", "86.7"), color = "white") 
ggsave("figures/caretaker_family_race.pdf", width=9, height=6, units="in")   


# foster family structure
ggplot(fc, aes(x = foster_structure2, fill = race)) + 
  geom_bar(position = "fill") +
  scale_fill_manual(values=colorviz3_rev) 

prop.table(table(fc$race, fc$foster_structure2), margin = 2)  
chisq.test(fc$foster_structure2, fc$race, simulate.p.value = TRUE)  

