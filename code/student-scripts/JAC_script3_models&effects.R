# models and visualization of effects

load("JAC_clean.RData")

#### 90% CI Graphs

# --------------------------- NEGLECT MODELS -------------------------------
# 1. most basic model: race, age, gender

log1 <- glm(remove_neglect ~ race + age_rem + gender, 
            data = dss_remove, family = "binomial")
# model coefficients and summary stats
summary(log1)
Anova(log1, type = 3) 

# 2. adding caretaker structure (reduced to single/two parents)
log2 <- glm(remove_neglect ~ race + age_rem + gender + care_structure2, 
            data = dss_remove, family = "binomial")
summary(log2)
Anova(log2, type = 3)


# 3. adding ever finding, ever unsafe 
log3 <- glm(remove_neglect ~ race + age_rem + gender 
            + care_structure2 
            + ever_find 
            + ever_unsafe, 
            data = dss_remove, family = "binomial")

summary(log3)
Anova(log3, type = 3)

# 4. all together~! 
stargazer(log1, log2, log3,
          type = "text", star.cutoffs = c(.3,0.25, 0.1))


# The model tests for differences in the proportion of children that are removed from the home 
# for neglect. While Black and multiracial children are slightly less likely to be removed 
# relative to White children, the differences are small and not significant at p < 0.3, despite 
# adding additional controls such as demographic characteristics (age, gender), the care structure 
# of the household (single or married) and the likelihood of a findiong; we do see, however, that 
# children who have an "ever finding" are more likely to have neglect as a remove reason, but 
# children with "ever unsafe" are slightly less likely to have neglect as a remove reason. 

# visualization for neglect 
count_pred <- predicts(log3, "F;8;F(2);F(2);F(3);F(2)", sim.count = 1000, conf.int = 0.90, set.seed = 19340)
# all values of race, age 8, male, single parent, all finding levels, ever unsafe
count_pred

colorviz3 <- c("#b25590","#e35d7c","#fe785b")

p2 <- ggplot(count_pred, aes(x = race, y = mean, color=race)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Removal for Neglect by Race",
       x="Race",
       y="Probability",
       caption = "Note: error bars are 90% credible intervals") +
  scale_color_manual(values=colorviz3)
p2


# ---------------------------HOUSING MODELS-------------------------------
# 1. most basic model: race, age, gender
log1 <- glm(remove_house ~ race + age_rem + gender, 
            data = dss_remove, family = "binomial")
# model coefficients and summary stats
summary(log1)
Anova(log1, type = 3) 

# 2. adding caretaker structure (reduced to single/two parents)
log2 <- glm(remove_house ~ race + age_rem + gender + care_structure2, 
            data = dss_remove, family = "binomial")
summary(log2)
Anova(log2, type = 3)

# 3. adding ever finding, ever unsafe 
log3 <- glm(remove_house ~ race + age_rem + gender 
            + care_structure2 
            + ever_find 
            + ever_unsafe, 
            data = dss_remove, family = "binomial")

summary(log3)
Anova(log3, type = 3)

# 5. all together~! 
stargazer(log1, log2, log3,
          type = "text", star.cutoffs = c(0.3, 0.25, 0.1))


# The model tests for differences in the proportion of children that are removed from the home 
# for inadequate housing. Multiracial children are 0.622 percentage points more likely to be 
# removed from the home for inadequate housing relative to White children; as additional control
# variables are added to the model, the difference becomes larger in magnitude and is significant
# at p < 0.10. There are 45 multiracial children in the sample size, so we think this difference 
# matters, although the magnitutde is small. Black children are not more likely to be removed 
# from the home for inadequate housing relative to White children. 


# visualization for housing 

count_pred <- predicts(log3, "F;8;F(2);F(2);F(3);F(2)", sim.count = 1000, conf.int = 0.90, set.seed = 19340)
# all values of race, age 8, male, single parent, all finding levels, ever unsafe
count_pred

p2 <- ggplot(count_pred, aes(x = race, y = mean, color=race)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Removal for Housing Quality by Race",
       x="Race",
       y="Probability",
       caption = "Note: error bars are 90% credible intervals") +
  scale_color_manual(values=colorviz3)
p2


# ---------------------------PARENT DRUG ABUSE MODELS----------------------------
# 1. most basic model: race, age, gender
log1 <- glm(remove_parent_drug ~ race + age_rem + gender, 
            data = dss_remove, family = "binomial")
# model coefficients and summary stats
summary(log1)
Anova(log1, type = 3) 

# 2. adding caretaker structure (reduced to single/two parents)
log2 <- glm(remove_parent_drug ~ race + age_rem + gender + care_structure2, 
            data = dss_remove, family = "binomial")
summary(log2)
Anova(log2, type = 3)

# 3. adding ever finding, ever unsafe 
log3 <- glm(remove_parent_drug ~ race + age_rem + gender 
            + care_structure2 
            + ever_find 
            + ever_unsafe, 
            data = dss_remove, family = "binomial")

summary(log3)
Anova(log3, type = 3)


# 5. all together~! 
stargazer(log1, log2, log3,
          type = "text", star.cutoffs = c(0.3, 0.25, 0.1))


# The model tests for differences in the proportion of children that are removed from the home
# for parent drug use. Multiracial children are 0.746 percentage points more likely to be removed
# from the home for parent drug abuse relative to White children; as additional control variables
# are added to the model, the difference becomes slightly smaller in magnitude and is significant
# at p < 0.25. There are 45 multiracial children in the sample size, so we think this difference 
# matters, although the magnitude is small. Black children are not more likely to be removed from
# the home for parent drug abuse relative to White children. Interestingly, age at removal and 
# gender are significant: for every additional year of age, a child is .12 percentage points more
# likely to be removed, and boys are 0.71 percentage points more likely to be removed relative to
# girls. 


# visualization for parent drug abuse

count_pred <- predicts(log3, "F;8;F(2);F(2);F(3);F(2)", sim.count = 1000, conf.int = 0.90, set.seed = 19340)
# all values of race, age 8, male, single parent, all finding levels, ever unsafe
count_pred

p2 <- ggplot(count_pred, aes(x = race, y = mean, color=race)) +
  geom_point(size=4) +
  geom_pointrange(aes(ymin = lower, ymax=upper)) +
  coord_flip() +
  labs(title="Probability of Removal for Parent Drug Abuse by Race",
       x="Race",
       y="Probability",
       caption = "Note: error bars are 90% credible intervals") +
  scale_color_manual(values=colorviz3)
p2
