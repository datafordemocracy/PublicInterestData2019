######################################################################################
# LP 5440: Public Interest Data Lab 
# Foster Care Placement History
# 1. Load libraries and data
# 2. Set color palette 
# 3. Number of placements
# 4. Duration of care
# 5. Time in placements
# Authors: Charlotte, MPC
# Updated: June 13, 2019 
######################################################################################


# ..........................................................................................
# 1. Load libraries and data ----
# load libraries
library(tidyverse)
library(lubridate)
library(stargazer)

ph <- readRDS("placement.rds")

# ..........................................................................................
# 2. Set color palette ----
colorviz5 <- c("#384c7d","#755391","#b25590","#e35d7c","#fe785b")
colorviz3 <- c("#b25590","#e35d7c","#fe785b")
color_swap <- c("#fe785b", "#e35d7c", "#b25590")


# ..........................................................................................
# 3. Number of placements ----
names(ph)

# distinct children
n_distinct(ph$cid) # 1427

ph %>% 
  filter(fc_enter == "Yes") %>% 
  summarize(n_distinct(cid)) # 182 in foster care

# foster care only, one record per child
fc <- ph %>% # 
  filter(fc_enter == "Yes" & race %in% c("White", "Black", "Multi-Race")) %>% 
  mutate(race = fct_recode(race, "White" = "White", # change to language used in report
                           "Black" = "Black",
                           "Multirace" = "Multi-Race"),
         race = fct_relevel(race, "White", "Black", "Multirace"),
         race = droplevels(race),
         gender = droplevels(gender),
         age_rem = interval(start = dob, end = remove_currdate) /
           duration(num = 1, units = "years"), # create variable for age at removal from home
         care_structure2 = fct_recode(care_structure, # collapse family structure into single/dual parent household
                                      "Single" = "Single mom",
                                      "Single" = "Single dad",
                                      "Dual" = "Married couple",
                                      "Dual" = "Unmarried couple"),
         child_disabled = case_when(child_disabled=='No' ~ 'No',
                                    child_disabled=='Yes' ~ 'Yes',
                                    TRUE ~ 'No'),  # change 'undetermined' to 'no'
         child_disabled = fct_relevel(child_disabled, "No", "Yes")) %>% # relevel so 'no' is the baseline 
  filter(placenum == 1)
  

# num_place by race
fc %>% group_by(race) %>% 
  summarize(mean(num_place), sd(num_place, median(num_place)), n())

# analysis of variance/difference of means test 
place_aov <- aov(num_place ~ race, data = fc) 
summary(place_aov)
# pairwise differences
TukeyHSD(place_aov)

# kruskal wallis (for non-normal, or medians)
kruskal.test(num_place ~ race, data = fc)
# pairwise comparisons
pairwise.wilcox.test(fc$num_place, fc$race,
                     p.adjust.method = "BH")

# plot
ggplot(fc, aes(x=num_place, fill=race)) + geom_density(alpha=.75) + 
  scale_fill_manual(values=color_swap, name='Race') +
  labs(title="Number of Placements By Race", 
       subtitle = "Among Children Entering Foster Care after Referral to CPS",
       x = "Placements",  y = "", fill="Race")  +
  geom_vline(xintercept = 2.2, col=color_swap[1], linetype=3, size=1.1)  +
  geom_vline(xintercept = 2.0, col=color_swap[2], linetype=2, size=1.1)  +
  geom_vline(xintercept = 4, col=color_swap[3], linetype=1, size=1.1) +
  theme(text = element_text(size = 15))
ggsave("figures/fc_place.pdf", width=9, height=6, units="in")

# models
np <-  glm(num_place ~ race + gender + age_rem,
           family = "poisson", data = fc)
summary(np)

np2 <-  glm(num_place ~ race + gender + age_rem +
             child_disabled + care_structure2, 
           family = "poisson", data = fc)
summary(np2)

np3 <-  glm(num_place ~ race + gender + age_rem +
              child_disabled + care_structure2 +
              remove_neglect + remove_parent_alc + remove_house + 
              remove_parent_drug + remove_physabuse,
            family = "poisson", data = fc)
summary(np3)

# table for appendix
stargazer(np, np2, np3,
          title = "Negative Binomial Model of Number of Placements in Foster Care",
          covariate.labels = c("Black", "Multiracial", "Male", "Age at removal", 
                               "Child Disabled", "Single Parent Family", 
                               "Removal for Physical Neglect", "Removal for Parental Alchohol Abuse",
                               "Removal for Inadequate Housing", "Removal for Parental Drug Abuse",
                               "Removal for Physical Abuse"),
          type = "latex", star.cutoffs = c(0.2, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, 
          dep.var.caption = "", dep.var.labels.include=FALSE)


# ..........................................................................................
# 4. Duration of care ----

# query date to generate duration/time in care for clients still in care
fc <- fc %>% 
  mutate(fc_exit = if_else(is.na(discharge_date), 0, 1)) # 1 if exit

query_date <- as.Date("2019-02-01") # data retrieval data for clients still in care

fc$duration <- ifelse((fc$fc_exit==0), 
                      difftime(query_date, fc$remove_currdate, units="weeks"), 
                      difftime(fc$discharge_date, fc$remove_currdate, units="weeks"))

summary(fc$duration)

# duration by race
fc %>% group_by(race) %>% 
  summarize(mean(duration), median(duration)) 

# kruskal wallis (for non-normal, or medians)
kruskal.test(duration ~ race, data = fc)
# pairwise comparisons
pairwise.wilcox.test(fc$duration, fc$race,
                     p.adjust.method = "BH")

# analysis of variance/difference of means test 
dur_aov <- aov(duration ~ race, data = fc) 
summary(dur_aov)
# pairwise differences
TukeyHSD(dur_aov)

# plot
ggplot(fc, aes(x=duration, fill=race)) + geom_density(alpha=.75) + 
  scale_fill_manual(values=color_swap, name='Race') +
  labs(title="Time in Foster Care by Race", 
       subtitle = "Among Children Entering Foster Care after Referral to CPS",
       x = "Weeks",  y = "", fill="Race")  +
  geom_vline(xintercept = 91, col=color_swap[1], linetype=3, size=1.1)  +
  geom_vline(xintercept = 82, col=color_swap[2], linetype=2, size=1.1)  +
  geom_vline(xintercept = 94, col=color_swap[3], linetype=1, size=1.1) +
  theme(text = element_text(size = 15))
ggsave("figures/fc_dur.pdf", width=9, height=6, units="in")

# Duration (Cox proportional hazard) model
library(survival)

fc <- fc %>% 
  mutate(fc_incare = if_else(is.na(discharge_date), 1, 0)) # 1 if exit

## Estimated survival function: race only
S <- Surv(fc$duration, fc$fc_incare)
dur0 <- coxph(S ~ race, data = fc)
summary(dur0)

dur1 <- coxph(S ~  race + gender + age_rem, data=fc)  
summary(dur1)  

dur2 <- coxph(S ~  race + gender + age_rem +
                child_disabled + care_structure2, data=fc)  
summary(dur2)  

dur3 <- coxph(S ~  race + gender + age_rem +
                child_disabled + care_structure2 +
                remove_neglect + remove_parent_alc + remove_house + 
                remove_parent_drug + remove_physabuse,
              data=fc)  
summary(dur3)  

# table for appendix
stargazer(dur1, dur2, dur3,
          title = "Duration Model of Time in Foster Care",
          covariate.labels = c("Black", "Multiracial", "Male", "Age at removal", 
                               "Child Disabled", "Single Parent Family", 
                               "Removal for Physical Neglect", "Removal for Parental Alchohol Abuse", 
                               "Removal for Inadequate Housing","Removal for Parental Drug Abuse", 
                               "Removal for Physical Abuse"),
          type = "latex", star.cutoffs = c(0.2, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, 
          dep.var.caption = "", dep.var.labels.include=FALSE)



# ..........................................................................................
# 6.Time in placements ----
# foster care, all placements
fcph <- ph %>% 
  filter(fc_enter == "Yes" & race %in% c("White", "Black", "Multi-Race") & as.numeric(placenum) <= num_place) %>% 
  mutate(race = fct_recode(race, "White" = "White", # change to language used in report
                           "Black" = "Black",
                           "Multirace" = "Multi-Race"),
         race = fct_relevel(race, "White", "Black", "Multirace"),
         race = droplevels(race),
         gender = droplevels(gender),
         age_rem = interval(start = dob, end = remove_currdate) /
           duration(num = 1, units = "years"), # create variable for age at removal from home
         care_structure2 = fct_recode(care_structure, # collapse family structure into single/dual parent household
                                      "Single" = "Single mom",
                                      "Single" = "Single dad",
                                      "Dual" = "Married couple",
                                      "Dual" = "Unmarried couple"),
         child_disabled = case_when(child_disabled=='No' ~ 'No',
                                    child_disabled=='Yes' ~ 'Yes',
                                    TRUE ~ 'No'),  # change 'undetermined' to 'no'
         child_disabled = fct_relevel(child_disabled, "No", "Yes")) # relevel so 'no' is the baseline 

fcph <- fcph  %>% 
  mutate(fc_inplace = if_else(is.na(leave_date), 1, 0)) # 1 if in care

fcph$time_weeks <- ifelse((fcph$fc_inplace==1), 
                     difftime(query_date, fcph$place_date, units="weeks"), 
                     difftime(fcph$leave_date, fcph$place_date, units="weeks"))


# time in placement by race
fcph %>% group_by(race) %>% 
  summarize(mean(time_weeks, na.rm=T), median(time_weeks, na.rm=T)) 

# kruskal wallis (for non-normal, or medians)
kruskal.test(time_weeks ~ race, data = fcph)
# pairwise comparisons
pairwise.wilcox.test(fcph$time_weeks, fcph$race,
                     p.adjust.method = "BH")


# analysis of variance/difference of means test 
time_aov <- aov(time_weeks ~ race, data = fcph) 
summary(time_aov)
# pairwise differences
TukeyHSD(time_aov)

# plot
ggplot(fcph, aes(x=time_weeks, fill=race)) + geom_density(alpha=.75) + 
  scale_fill_manual(values=color_swap, name='Race') +
  labs(title="Time in Each Placement by Race", 
       subtitle = "Among Children Entering Foster Care after Referral to CPS",
       x = "Weeks",  y = "", fill="Race")  +
  geom_vline(xintercept = 24, col=color_swap[1], linetype=3, size=1.1) +
  geom_vline(xintercept = 25, col=color_swap[2], linetype=2, size=1.1) +
  geom_vline(xintercept = 20, col=color_swap[3], linetype=1, size=1.1) +
  theme(text = element_text(size = 15))
ggsave("figures/fc_time.pdf", width=9, height=6, units="in")


## Estimated survival function: race only
St <- Surv(fcph$time_weeks, fcph$fc_inplace)
tim0 <- coxph(St ~ race, data = fcph)
summary(tim0)

tim1 <- coxph(St ~  race + gender + age_rem, data=fcph)  
summary(tim1)  

tim2 <- coxph(St ~  race + gender + age_rem +
                child_disabled + care_structure2, data=fcph)  
summary(tim2)  

# needs type of placement (at least ffkin, ff, other?); this needs verification
ffk <- c("Foster Home, Relative", "LDSS Foster Home, Fictive Kin", "LDSS Foster Home, Relative", "Relative Home,Unapproved")
ffnk <- c("AdoptionNon-Finalized", "Foster Home, CPA Regular", "Foster Home, Emergency", "Foster Home, Non-Relative", "LCPA Foster Home", "LDSS Foster Home")

fcph <- fcph %>% 
  mutate(place_each = if_else(typeofcare %in% ffk, "Foster family-kin", 
                              if_else(typeofcare %in% ffnk, "Foster family-not kin", "Other")),
         place_each = fct_relevel(place_each, "Other", "Foster family-not kin", "Foster family-kin"))

tim3 <- coxph(St ~  race + gender + age_rem +
                child_disabled + care_structure2 +
                remove_neglect + remove_parent_alc + remove_house + 
                remove_parent_drug + remove_physabuse,
              data=fcph)  
summary(tim3)  

tim4 <- coxph(St ~  race + gender + age_rem +
                child_disabled + care_structure2 +
                remove_neglect + remove_parent_alc + remove_house + 
                remove_parent_drug + remove_physabuse + place_each,
              data=fcph)  
summary(tim4)  


# table for appendix
stargazer(tim1, tim2, tim4,
          title = "Duration Model of Time in Individual Placement",
          covariate.labels = c("Black", "Multiracial", "Male", "Age at removal", 
                               "Child Disabled", "Single Parent Family", 
                               "Removal for Physical Neglect", "Removal for Parental Alchohol Abuse", 
                               "Removal for Inadequate Housing","Removal for Parental Drug Abuse", 
                               "Removal for Physical Abuse", "Placement is Non-Kin Foster Family",
                               "Placement is Kin Foster Family"),
          type = "latex", star.cutoffs = c(0.2, 0.1, 0.05),
          column.sep.width="1pt", align=TRUE, no.space=TRUE, 
          dep.var.caption = "", dep.var.labels.include=FALSE)
