# Public Interest Data Lab
# Child flow - Sankey
# May 2019

# ..........................................................................................

# load libraries ----
library(tidyverse)
library(ggalluvial)


# ..........................................................................................

# read in files ----
setwd("/Volumes/PIDL19")
dss <- readRDS("dss.rds")

# set palettes
colorviz5 <- c("#384c7d","#755391","#b25590","#e35d7c","#fe785b")
colorviz3 <- c("#b25590","#e35d7c","#fe785b")

# ..........................................................................................

# format, recode ----
# create ever_services variable
dss <- dss %>% mutate(ever_services = if_else(str_detect(disp_seq, "SVC_"), "Services", "No Services"),
                      ever_services = if_else(is.na(disp_seq), "No Action", ever_services))
# collapse finding 
dss <- dss %>% mutate(ever_find2 = if_else(ever_find %in% c("Find-Low", "Find-Mod", "Find-High"), 
                                          "Finding", "No Finding"))

# create reduced data frame
dss <- dss %>% 
  mutate(ever_inv2 = if_else(ever_screened == "No", "No Action", ever_inv2),
         ever_inv2 = factor(ever_inv2, levels = c("No Action", "No", "Yes")),
         ever_find = factor(ever_find, ordered = FALSE),
         ever_find = if_else(ever_inv2 == "No Action", "No Action", as.character(ever_find)),
         ever_find = factor(ever_find, levels = c("No Action", "No Find", "Find-Low", "Find-Mod", "Find-High")),
         ever_find2 = if_else(ever_inv2 == "No Action", "No Action", as.character(ever_find2)),
         ever_find2 = factor(ever_find2, levels = c("No Action", "No Finding", "Finding")),
         ever_unsafe = if_else(ever_find == "No Action", "No Action", as.character(ever_unsafe)),
         ever_unsafe = factor(ever_unsafe, levels = c("No Action", "No", "Yes")),
         fc_enter = if_else(ever_unsafe == "No Action", "No Action", as.character(fc_enter)),
         fc_enter = factor(fc_enter, levels = c("No Action", "No", "Yes")))

dss_flow <- dss %>% 
  group_by(race_ethn, ever_screened, ever_inv2, ever_services, ever_find2, ever_unsafe, fc_enter) %>% 
  summarize(weight = n()) %>% 
  ungroup()

# recode for appropriate node names
dss_flow <- dss_flow %>% 
  mutate(ever_screened = fct_recode(ever_screened,
                                    "In" = "Yes",
                                    "Out" = "No"),
         ever_inv2 = fct_recode(ever_inv2, 
                                "Assess" = "No",
                                "Investigate" = "Yes"),
         ever_unsafe = fct_recode(ever_unsafe,
                                  "Safe" = "No",
                                  "Unsafe" = "Yes"),
         fc_enter = fct_recode(fc_enter,
                               "Removal" = "Yes",
                               "No removal" = "No"))

# ..........................................................................................

# race as facet (white, black, multirace), services on axis, fc as color
ggplot(filter(dss_flow, race_ethn %in% c("White", "Black", "MultiRace")),
       aes(y = weight, 
           axis1 = ever_screened, axis2 = ever_inv2, axis3 = ever_services, axis4 = ever_find2, axis5 = ever_unsafe, axis6 = fc_enter)) +
  geom_alluvium(aes(fill= fc_enter), width = 0.5, alpha = 0.75, knot.pos = 0.5) +
  geom_stratum(width = .2, fill = "white") +
  geom_text(stat = "stratum", label.strata = TRUE, size = 3, angle = -90) +
  scale_x_discrete(limits = c("Screen", "Response", "Services", "Finding", "Safety", "Foster Care"), expand = c(.05, .05)) +
  scale_fill_manual(values=colorviz5[c(5,3,1)]) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Movement of Children through Post-Referral Decisions by Race",
       subtitle = "Among Children Referred to CPS between January 2015 and July 2017",
       y = "Frequency", fill = "Foster Care Outcome") + facet_wrap(~ race_ethn, scales = "free_y")
ggsave("figures/child_flow_alluvial.pdf", width=9, height=7, units="in")   


# not used
# race as facet (asian, n = 12, hispanic, n = 87), services on axis, fc as color
ggplot(filter(dss_flow, race_ethn %in% c("Asian", "Hispanic")),
       aes(y = weight, 
           axis1 = ever_screened, axis2 = ever_inv2, axis3 = ever_services, axis4 = ever_find2, axis5 = ever_unsafe, axis6 = fc_enter)) +
  geom_alluvium(aes(fill= fc_enter), width = 0.5, alpha = 0.75, knot.pos = 0.5) +
  geom_stratum(width = .1, fill = "white") +
  geom_text(stat = "stratum", label.strata = TRUE, size = 3) +
  scale_x_discrete(limits = c("Screen", "Response", "Services", "Finding", "Safety", "Foster Care"), expand = c(.05, .05)) +
  scale_fill_manual(values=colorviz5[c(5,3,1)]) +
  ggtitle("Movement of Children through Post-Referral Decisions by Race") +
  theme(legend.position = "bottom") +
  labs(y = "Frequency", fill = "Foster Care Outcome") + facet_wrap(~ race_ethn, scales = "free_y")

# not used
# all five race/ethnicity groups
ggplot(filter(dss_flow, race_ethn!= "Unknown"),
       aes(y = weight, 
           axis1 = ever_screened, axis2 = ever_inv2, axis3 = ever_services, axis4 = ever_find2, axis5 = ever_unsafe, axis6 = fc_enter)) +
  geom_alluvium(aes(fill= fc_enter), width = 0.5, alpha = 0.75, knot.pos = 0.5) +
  geom_stratum(width = .2, fill = "white") +
  geom_text(stat = "stratum", label.strata = TRUE, size = 3, angle = -90) +
  scale_x_discrete(limits = c("Screen", "Response", "Services", "Finding", "Safety", "Foster Care"), expand = c(.05, .05)) +
  scale_fill_manual(values=colorviz5[c(5,3,1)]) +
  ggtitle("Movement of Children through Post-Referral Decisions by Race") +
  theme(legend.position = "bottom") +
  labs(y = "Frequency", fill = "Foster Care Outcome") + facet_wrap(~ race_ethn, scales = "free_y")

# not used
# ggforce
# install.packages("ggforce")
library(ggforce)

dss_flow2 <- dss_flow %>% 
  select(fc_enter, everything()) 
dss_flow2 <- gather_set_data(dss_flow2, 1:7)

dss_flow3 <- dss_flow %>% 
  select(-race_ethn) %>% 
  select(fc_enter, everything())
dss_flow3 <- gather_set_data(dss_flow3, 1:6)

ggplot(dss_flow3, aes(x, id = id, split = y, value = weight)) +
  geom_parallel_sets(aes(fill = fc_enter), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white') 
