# Pre-analysis visualizations and basic inferential tests

load("JAC_clean.RData")

# ------------------------------- REMOVE REASONS -----------------------------------------
# REMOVE SUM
reg <- lm(remove_sum ~ race, data = dss_remove)
summary(reg)

set.seed(19340)
chisq.test(dss_remove$remove_sum, dss_remove$race, simulate.p.value = TRUE)  

# reshape removal reasons to long
var <- names(dss)[c(5, 100:114)]
reason <- dss_remove %>% 
  dplyr::select(var, -c(remove_death, remove_alc, remove_sexabuse, remove_drug, remove_disable)) %>% 
  gather(remove_physabuse:remove_house, key="why", value="response")

# determine more common reasons
tot <- reason %>% dplyr::group_by(why) %>% 
  dplyr::summarise(sum=sum(response))
reason <- left_join(reason, tot, by="why")

# order by most common levels
reason <- mutate(reason, why=factor(why, levels=unique(why[order(-sum,why)])))

# generate p-values
set.seed(19340)
# remove_neglect
chisq.test(dss_remove$remove_neglect, dss_remove$race, simulate.p.value = TRUE)  
# remove_parent_drug
chisq.test(dss_remove$remove_parent_drug, dss_remove$race, simulate.p.value = TRUE) 
# remove_house
chisq.test(dss_remove$remove_house, dss_remove$race, simulate.p.value = TRUE) 
# remove_cope
chisq.test(dss_remove$remove_cope, dss_remove$race, simulate.p.value = TRUE) 
# remove_parent_alc ***  
chisq.test(dss_remove$remove_parent_alc, dss_remove$race, simulate.p.value = TRUE) 
# remove_behave
chisq.test(dss_remove$remove_behave, dss_remove$race, simulate.p.value = TRUE) 
# remove_physabuse *
chisq.test(dss_remove$remove_physabuse, dss_remove$race, simulate.p.value = TRUE) 
# remove_jail ** 
chisq.test(dss_remove$remove_jail, dss_remove$race, simulate.p.value = TRUE) 
# remove_relinq - 
chisq.test(dss_remove$remove_relinq, dss_remove$race, simulate.p.value = TRUE) 
# remove_abandon
chisq.test(dss_remove$remove_abandon, dss_remove$race, simulate.p.value = TRUE) 

# graph removal reasons
colorviz3 <- c("#b25590","#e35d7c","#fe785b")

ggplot(reason, aes(x=why, y=response, fill=race)) + 
  stat_summary(fun.y="mean", geom="bar", position = "dodge") +
  labs(title = "Reasons for Removal from Home", subtitle = "By Race", 
       y = "Proportion", x = "Reason for Removal", 
       caption=("Note: * represents p value significant at .100, \n given by chi-squared test for independence") ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels=c("Neglect", "Parent Drug Use", "Housing Quality", 
                            "Inability to Cope", "* Parent Alcohol", "Behavior", 
                            "* Physical Abuse", "* Jail", "Relinquishment", "Abandonment")) +
  scale_fill_manual(values=colorviz3)

# --------------------------------- REMOVE TYPE ------------------------------------------
set.seed(19340)
chisq.test(dss$removal_type, dss$race, simulate.p.value = TRUE) 

# Nearly all children in the population are removed from the home by court-order 
# (as opposed to voluntary) but there is an indicaton that race is slightly predictive 
# in whether a child is removed (p = .1779).

# ------------------------------ FINANCIAL SUPPORT ---------------------------------------
# ANOVA for fc_monthly_pay
pay_aov <- aov(fc_monthly_pay ~ race, data = dss)
summary(pay_aov)

# A statistically significant result (p = .0312) tells us that at least two of the means
# differ. In other words, at least two of the groups (White, Black and multiracial) differ
# significantly on the monthly foster care pay they receive.

# Tukey HSD test
TukeyHSD(pay_aov)
# Black-White: p = .028
# MultiRace-White: p = .106
# MultiRace-Black: p = .967

# This means that the Black and White groups differ significantly, but the multiracial 
# and black groups do not. The difference between the multiracial and White group
# approaches significance.


# FINANCIAL SUPPORT TYPES

# test p-values
set.seed(19340)
chisq.test(dss_remove$get_afdc, dss_remove$race, simulate.p.value = TRUE) # *
chisq.test(dss_remove$get_csupport, dss_remove$race, simulate.p.value = TRUE) 
chisq.test(dss_remove$get_medicaid, dss_remove$race, simulate.p.value = TRUE) 
chisq.test(dss_remove$get_ssi, dss_remove$race, simulate.p.value = TRUE) 
chisq.test(dss_remove$get_support, dss_remove$race, simulate.p.value = TRUE) 

# reshape removal reasons to long
var <- c(names(dss_remove)[c(5, 121:126)])
dss_remove[var] <- lapply(dss_remove[var], function (x) {fct_recode(x,
                                                                    "1"="Yes",
                                                                    "0"="No")})
financial <- dss_remove %>%
  dplyr::select(var, -c(get_adopt)) %>%
  gather(get_afdc:get_support, key="why", value="response")

# determine more common reasons
financial$response <- as.numeric(financial$response)
tot <- financial %>% dplyr::group_by(race, why) %>%
  dplyr::summarise(sum = sum(response))
financial <- left_join(financial, tot, by="why")

# order by most common levels
financial <- mutate(financial, why=factor(why, levels=unique(why[order(-sum,why)])))

###
# clean data
dss_financial <- dss[c(5, 121:126)]
dss_financial[2:7] <- ifelse(dss_financial[2:7]=="Yes", 1, 0)
var <- names(dss_financial)
dss_financial <- dplyr::select(var, -c(get_adopt))

# aggregate data by proportions
aggdata_mean <- aggregate(dss_financial[2:7], by=list(race = dss_financial$race), FUN=mean)
means.long <- melt(aggdata_mean, id.vars="race")

# graph mean proportions
ggplot(means.long, aes(x=variable, y=value, fill=factor(race))) +
  geom_bar(stat="identity", position="dodge", width=0.8) +
  labs(title = "Financial Supports Received", subtitle = "By Race", 
       y = "Proportion by Race", x = "Receives Aid") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels=c("Adoption Assistance", "* AFDC",
                            "Child Support", "Medicaid", "Social Security", "Any Form of Support")) +
  scale_fill_manual(values=colorviz3) +
  geom_segment(aes(x=5.5, y=0, xend=5.5, yend=1), linetype='dashed') +
  guides(fill=guide_legend(title="Race"))

