### Kathryn Bernard, Carolyn Ours, Savannah Quick
### Visualization Script - PIDL '19
### April 26, 2019

###### Kathryn ###### 

### set working directory to encrypted volume
#setwd("/Volumes/PIDL19")

### load packages
library(tidyverse)
library(lubridate)

### read in data created in CSK_Cleaning.R
foster <- readRDS("foster_clean.rds")

### create color palettes
colorviz5 <- c("#384c7d","#755391","#b25590","#e35d7c","#fe785b")
colorviz3 <- c("#b25590","#e35d7c","#fe785b")
## reorder color palette
color_swap <- c("#fe785b", "#e35d7c", "#b25590")

###### disability variable ###### 

### proportion of each disability by race

## total number of children of each race in foster care
tot <- (foster %>% group_by(race) %>% count())$n 
# white = 42, black = 91, multiracial = 45

## general disability (any diagnosis)
general <- foster %>% group_by(race) %>% count(child_disabled)
gen_diag <- filter(general, child_disabled=='Yes')$n # number of children diagnosed with any disability
gen_prop <- round(gen_diag/tot, 3) # proportion of children diagnosed with any disability (by race)

# chi sq test of independence - tests whether yes/no counts are different
chisq.test(cbind(gen_diag, (tot-gen_diag)), correct=FALSE)
genp <- chisq.test(cbind(gen_diag, (tot-gen_diag)), correct=FALSE)$p.value # get p value

## emotional disturbance
emotional <- foster %>% group_by(race) %>% count(child_disturbed)
em_diag <- filter(emotional, child_disturbed=='Yes')$n # number of children diagnosed with emotional disturbance
em_prop <- round(em_diag/tot, 3) # proportion of children diagnosed with emotional disturbance (by race)

# chi sq test of independence - tests whether yes/no counts are different
chisq.test(cbind(em_diag, (tot-em_diag)), correct=FALSE)
emp <- chisq.test(cbind(em_diag, (tot-em_diag)), correct=FALSE)$p.value

## medical diagnosis
medical <- foster %>% group_by(race) %>% count(child_othermed)
med_diag <- filter(medical, child_othermed=='Yes')$n # number of children diagnosed with medical disability
med_prop <- round(med_diag/tot, 3) # proportion of children diagnosed with intellectual disability (by race)

# chi sq test of independence - tests whether yes/no counts are different
chisq.test(cbind(med_diag, (tot-med_diag)), correct=FALSE)
medp <- chisq.test(cbind(med_diag, (tot-med_diag)), correct=FALSE)$p.value

## intellectual disability
intellectual <- foster %>% group_by(race) %>% count(child_mr)
int_diag <- filter(intellectual, child_mr=='Yes')$n # number of children diagnosed with intellectual disability
int_prop <- round(int_diag/tot, 3) # proportion of children diagnosed with intellectual disability (by race)

# chi sq test of independence - tests whether yes/no counts are different
chisq.test(cbind(int_diag, (tot-int_diag)), correct=FALSE)
intp <- chisq.test(cbind(int_diag, (tot-int_diag)), correct=FALSE)$p.value

## physical disability
physical <- foster %>% group_by(race) %>% count(child_physdis)
phys_diag <- c(0,filter(physical, child_physdis=='Yes')$n) # number of children diagnosed with physical disability
# add 0 for white children
phys_prop <- round(phys_diag/tot, 3) # proportion of children diagnosed with physical disability (by race)

# chi sq test of independence - tests whether yes/no counts are different
chisq.test(cbind(phys_diag, (tot-phys_diag)), correct=FALSE)
physp <- chisq.test(cbind(phys_diag, (tot-phys_diag)), correct=FALSE)$p.value

## hearing disability
hearing <- foster %>% group_by(race) %>% count(child_hearing)
hear_diag <- c(0,0,filter(hearing, child_hearing=='Yes')$n) # number of children diagnosed with hearing disability
# add 0 for white and black children
hear_prop <- round(hear_diag/tot, 3) # proportion of children diagnosed with hearing disability (by race)

# chi sq test of independence - tests whether yes/no counts are different
chisq.test(cbind(hear_diag, (tot-hear_diag)), correct=FALSE)
hearp <- chisq.test(cbind(hear_diag, (tot-hear_diag)), correct=FALSE)$p.value

## df with all proportions
race <- c('White','Black','Multiracial') 
dis_props <- as.data.frame(cbind(gen_prop,em_prop,med_prop,int_prop,phys_prop,hear_prop)) # concat all proportions
rownames(dis_props) <- race # label by race

## long form df for plots
dis_long <- gather(dis_props, key=distype, value=prop) # long form dataframe
dis_long$race <- rep(race, ncol(dis_props)) # add column for race
dis_long <- dis_long[,c(3,1,2)]
dis_long <- dis_long %>% 
  mutate(distype = factor(distype)) %>%
  mutate(race = factor(race)) %>% 
  mutate(distype = fct_relevel(distype, "gen_prop", "em_prop", "med_prop", "int_prop", "phys_prop", "hear_prop")) %>% 
  mutate(race = fct_relevel(race, "White", "Black", "Multiracial")) 

## plot: star on axis label
ggplot(dis_long, aes(x = distype, y = prop, fill = race)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=colorviz3, name='Race') +
  labs(x="Type of Diagnosis",
       y="Proportion of Children with Diagnosis",
       title="Disability Diagnosis by Race",
       caption="Note: * represents p value significant at .100,\n given by chi-squared test for indpendence") +
  scale_x_discrete(labels=c("General", "Emotional *", "Medical", "Intellectual", "Physical", "Hearing"))

## plot: star + note on graph; space between general and rest of bars
ggplot(dis_long, aes(x = distype, y = prop, fill = race)) +
  geom_col(position = 'dodge', width=.8) +
  scale_fill_manual(values=color_swap, name='Race') +
  scale_x_discrete(labels=c("General", "Emotional", "Medical", "Intellectual", "Physical", "Hearing")) +
  geom_segment(aes(x=1.5, y=0, xend=1.5, yend=.32), linetype='dashed', color="#384c7d") +
  labs(x="Type of Diagnosis",
       y="Proportion of Children with Diagnosis",
       title="Disability Diagnosis by Race") +
  annotate("text", x=2, y=.27, color='black', label=paste0(" * "), size=8) + 
  geom_label(x=4.5, y=.275, label="Note: * represents significance \n at p < .100", label.size=.5,
             inherit.aes=FALSE) +
  geom_segment(aes(x=2.1, y=.275, xend=3.2, yend=.275), linetype="solid", color="black")

## plot: star + note on graph; 'general' bars faded 
ggplot(dis_long, aes(x=distype, y=prop, fill=race, alpha=distype)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=colorviz3, name="Race") +
  scale_alpha_manual(values=c(.8,1,1,1,1,1), guide="none") +
  labs(x="Type of Diagnosis",
       y="Proportion of Children with Diagnosis",
       title="Disability Diagnosis by Race") +
  annotate("text", x=2, y=.27, color='black', label=paste0(" * "), size=8) + 
  scale_x_discrete(labels=c("General", "Emotional", "Medical", "Intellectual", "Physical", "Hearing")) +
  geom_label(x=4.5, y=.275, label="Note: * represents significance \n at p < .100", label.size=.5,
             inherit.aes=FALSE) +
  geom_segment(aes(x=2.1, y=.275, xend=3.2, yend=.275), linetype="solid", color="black")



###### discharge reason ###### 

### descriptives
rsn_race <- foster %>% group_by(race, discharge_reason) %>% count() # count of discharge reasons by race
discharged <- foster %>% 
  filter(discharge_reason != 'Still in Care') %>% # remove children who haven't been discharged
  filter(discharge_reason != 'Custody Transfer (Agency)') %>% # remove child who was transferred to another agency
  mutate_at("discharge_reason", droplevels) %>% 
  mutate(discharge_reason = fct_relevel(discharge_reason, "Emancipation", "Reunification",
                                        "Adoption", "Custody Transfer (Relative)"))

tot2 <- (discharged %>% group_by(race) %>% count())$n # total number of discharged children for each race
discharged_race <- discharged %>% group_by(race, discharge_reason) %>% count()

## reunification
reun <- (discharged_race %>% filter(discharge_reason=="Reunification"))$n
chisq.test(cbind(reun, tot2-reun), correct=FALSE)

## adoption
adopt <- (discharged_race %>% filter(discharge_reason=="Adoption"))$n
chisq.test(cbind(adopt, tot2-adopt), correct=FALSE)

## custody transfer to relative
transfer <- (discharged_race %>% filter(discharge_reason=="Custody Transfer (Relative)"))$n
chisq.test(cbind(transfer, tot2-transfer), correct=FALSE)

## emancipation
eman <- (discharged_race %>% filter(discharge_reason=="Emancipation"))$n
chisq.test(cbind(eman, tot2-eman), correct=FALSE)

## plot: proportion of children with each discharge reason (reunification, relative custody, adoption, emancipation)
## star and note on graph
discharged_race %>% 
  group_by(race) %>% 
  mutate(prop=round(n/sum(n),3)) %>% 
  mutate(discharge_reason = fct_relevel(discharge_reason, "Reunification", "Custody Transfer (Relative)", 
                                       "Adoption","Emancipation")) %>% 
  ggplot(aes(x=discharge_reason, y=prop, fill=race)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=color_swap, name='Race') +
  labs(x="Discharge Reason",
       y="Proportion of Children with Reason",
       title="Discharge Reason by Race") +
  annotate("text", x=3, y=.4, color='black', label=paste0(" * "), size=8) + 
  geom_label(x=3, y=.55, label="Note: * represents significance \n at p < .001", label.size=.5,
             inherit.aes=FALSE) +
  geom_segment(x=3, y=.5, xend=3, yend=.43, linetype="solid", color="black")


#### Savannah ####

###### case goals
#Code to look at case goals
cases <- foster %>% 
  filter(!(is.na(case_goal))) #copies foster data to avoid cluttering 
cases$permanancy <- (cases$case_goal=='Reunification'|cases$case_goal=='Live with relative'|
                       cases$case_goal=='Adoption')  #assigns true/false value for if case goal is a permanancy goal
cases$permanancy[cases$permanancy==TRUE] <- 1   #assigns 1 and 0 to use for binomial model
cases$permanancy[cases$permanancy==FALSE] <- 0

#transforms to factor for graph
cases$perm <- cases$permanancy #creates new column 
cases$perm[cases$perm=='1'] <- 'Permanancy goal'
cases$perm[cases$perm=='0'] <- 'Non-permanancy goal'
cases$perm <- as.factor(cases$perm)

#finds p-value using Fisher test
p <- chisq.test(cases$permanancy, cases$race, simulate.p.value = TRUE)
fish <- fisher.test(cases$permanancy, cases$race)


#generates proportions to add on to graph
goal_tab <- round(prop.table(table(cases$perm, cases$race),2),2)

ggplot(cases, aes(x=race, fill=perm)) + 
  geom_bar(position = 'fill') +          #creates bar graph
  labs(title='Case Goals by Race', x = 'Race', y='Proportion',    #adds labels
       fill='Case goal') +
  scale_x_discrete(labels=c('White', 'Black', 'Multiracial')) +
  scale_fill_manual(values = colorviz5[c(1,3)]) +  #sets colors
  annotate("text", x=1, y = 0.97, label= (goal_tab[1]), color='white') +   #adds proportion labels
  annotate("text", x=1, y = (goal_tab[2]-0.05), label= (goal_tab[2]), color='white') +
  annotate("text", x = 2, y = .97, label = c(goal_tab[3]), color = "white") +
  annotate("text", x=2, y = (goal_tab[4]-0.05), label= (goal_tab[4]), color='white') +
  annotate("text", x = 3, y = .97, label = c(goal_tab[5]), color = "white") +
  annotate("text", x=3, y = (goal_tab[6]-0.05), label= (goal_tab[6]), color='white') +
  annotate("label", x = 3.2, y = .043, label = paste0("p = ", round(fish$p.value, 2))) 

##### Case goals and discharge reason ----

#drops those still in foster care
reasons <- cases %>% 
  filter(discharge_reason != 'Still in Care')

reasons$discharge_reason
#new column to show graph of how often goal was achieved
reasons$perm_discharge <- dplyr::recode(reasons$discharge_reason,   #recodes discharge reasons to two outcomes
                                        'Still in Care' = 'Non-permanancy outcome',
                                        'Custody Transfer (Agency)' = 'Non-permanancy outcome',
                                        'Emancipation' = 'Non-permanancy outcome',
                                        'Adoption' = 'Permanancy outcome', 
                                        'Custody Transfer (Relative)' = 'Permanancy outcome',
                                        'Reunification' = 'Permanancy outcome')

#plots case goals and how often they're achieved 
ggplot(reasons, aes(x=perm_discharge)) + 
  geom_bar(fill=colorviz5[3])+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title="Case Goal Assignment and Discharge Reason", x='Discharge Reason', y='Count') +
  facet_wrap(.~perm)


#####################################################
#### Current Placement Setting (Carolyn)
#####################################################

## Data frame: filter by missing, relevel placement setting by most frequent 
cps_by_race <- foster %>% 
  filter(!is.na(place_now)) %>% 
  filter(race %in% c("White", "Black", "MultiRace")) %>% 
  mutate(place_now = fct_relevel(place_now, "Institution",  "Independent living", "Trial home visit",
                                 "Pre-adopt home", "Group home","Foster family-kin", 
                                 "Foster family-not kin")) %>% 
  mutate(race=fct_relevel(race,"Black", "MultiRace", "White"))

## generates frequency/percent variables for each race 
cvars <- c("race", "place_now")
place <- cps_by_race[cvars] %>% 
  group_by(race, place_now) %>% 
  summarize(n = n()) %>% 
  mutate(freq = n/sum(n))

## total number of each race in foster care
tot <- (foster %>% group_by(race) %>% count())$n 

## chi-squared test foster not kin 
tbl <- table(cps_by_race$place_now, cps_by_race$race) %>% 
  filter(cps_by_race$race %in% c("White", "Black", "MultiRace"))
ffnktbl <- cbind(tbl["Foster family-not kin","Black"], tbl["Foster family-not kin","MultiRace"], tbl["Foster family-not kin","White"])
chisq.test(ffnktbl)
ffnkp <- chisq.test(ffnktbl)$p.value ## p = 0.01 

## chi-squared test foster kin 
ffktbl <- cbind(tbl["Foster family-kin","Black"], tbl["Foster family-kin","MultiRace"], tbl["Foster family-kin","White"])
chisq.test(ffktbl)
ffkp <- chisq.test(ffktbl)$p.value ## p = 0.02

## chi-squared test group home
ghtbl <- cbind(tbl["Group home","Black"], tbl["Group home","MultiRace"], tbl["Group home","White"])
chisq.test(ghtbl)
ghp <- chisq.test(ghtbl)$p.value ## p = 0.09

## chi-squared test pre adopt home 
pahtbl <- cbind(tbl["Pre-adopt home","Black"], tbl["Pre-adopt home","MultiRace"], tbl["Pre-adopt home","White"])
chisq.test(pahtbl)
pahp <- chisq.test(pahtbl)$p.value ## p = 0.003

## chi-squared test trial home visit  
thvtbl <- cbind(tbl["Trial home visit","Black"], tbl["Trial home visit","MultiRace"], tbl["Trial home visit","White"])
chisq.test(thvtbl)
thvp <- chisq.test(thvtbl)$p.value ## p = 0.005

## chi-squared test independent living
iltbl <- cbind(tbl["Independent living","Black"], tbl["Independent living","MultiRace"], tbl["Independent living","White"])
chisq.test(iltbl)
ilp <- chisq.test(iltbl)$p.value ## p = 0.90

## chi-squared test institution
itbl <- cbind(tbl["Institution","Black"], tbl["Institution","MultiRace"], tbl["Institution","White"])
chisq.test(itbl)
ip <- chisq.test(itbl)$p.value ## p = 0.17

## creates a horizontal bar plot of the share of children of each race 
## in each placement setting 
## star: all but last two 
ggplot(place, aes(x=place_now, y = freq, fill=race, order = -as.numeric(race))) + 
  geom_bar(stat = "identity", position='dodge') +
  scale_fill_manual(values=colorviz3, name='Race') +
  labs(title="Current Placement Setting by Race", subtitle = "Among 174 children entering foster care",
       x=' Current Placement', y='Share of Foster Children in Placement',   fill='Race') + 
  coord_flip() + guides(fill = guide_legend(reverse=TRUE)) + 
  theme(axis.ticks.x = element_line(colour = c("black", "transparent", "black", "black", "black", "black", "black"))) + 
  annotate("text", x=2.8, y=.14, color='black', label=paste0(" * "), size=6) + 
  annotate("text", x=3.8, y=.17, color='black', label=paste0(" * "), size=6) + 
  annotate("text", x=4.8, y=.24, color='black', label=paste0(" * "), size=6) + 
  annotate("text", x=5.8, y=.38, color='black', label=paste0(" * "), size=6) + 
  annotate("text", x=6.8, y=.58, color='black', label=paste0(" * "), size=6) +
  geom_label(x=1.2, y=.42, label="Note: * represents significance \n at p < .100", label.size=.5,
             inherit.aes=FALSE)


