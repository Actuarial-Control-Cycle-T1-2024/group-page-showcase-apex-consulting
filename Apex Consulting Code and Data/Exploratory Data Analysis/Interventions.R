# test script
setwd('Case Data')
# import libraries
library("dplyr")
library("ggplot2")
library("lifecontingencies")
library("reshape2")

###
# Cleaning Data
###

# Import data
inforce_raw <- read.csv("2024-srcsc-superlife-inforce-dataset.csv")
summary(inforce_raw)

lumaria_qx <- read.csv("srcsc-2024-lumaria-mortality-table-CLEANED.csv", header = TRUE)
life_table <- probs2lifetable(probs = lumaria_qx$Mortality.Rate, 
                              type = "qx", 
                              radix = 100000, 
                              name = "Lumaria_Life_Table")

inforce_clean <- inforce_raw

# Fix column name inconsistencies
colnames(inforce_clean)[colnames(inforce_clean) == "Death.indicator"] <- "Death.Indicator"

# Replace NA values with 0 for indicators
inforce_clean$Lapse.Indicator[is.na(inforce_clean$Lapse.Indicator)] <- 0
inforce_clean$Death.Indicator[is.na(inforce_clean$Death.Indicator)] <- 0

# Replace Lapse Indicator "Y" with "1" (6866 occurrences)
inforce_clean$Lapse.Indicator[inforce_clean$Lapse.Indicator == "Y"] <- "1"

inforce_clean$Lapse.Indicator <- as.numeric(inforce_clean$Lapse.Indicator)
inforce_clean$Death.Indicator <- as.numeric(inforce_clean$Death.Indicator)

inforce_clean$Underwriting.Class = factor(inforce_clean$Underwriting.Class, levels = c("very low risk", "low risk", "moderate risk", "high risk"))

# NOTE years of lapse and death have na because they have not occurred
inforce_clean$age_at_death <- inforce_clean$Issue.age + inforce_clean$Year.of.Death - inforce_clean$Issue.year
inforce_clean$curr_age <- inforce_clean$Issue.age + 2024 - inforce_clean$Issue.year


####
# Interventions
####
ints <- read.csv('srcsc-2024-interventions-CLEANED.csv')

ints$Lower_Bound_Mort_Reduction = as.numeric(gsub("%","",as.character(ints$Lower_Bound_Mort_Reduction)))
ints$Upper_Bound_Mort_Reduction = as.numeric(gsub("%","",as.character(ints$Upper_Bound_Mort_Reduction)))


# Effects are reduction in mortality % per dollar
ints$avg_effect <- (ints$Lower_Bound_Mort_Reduction + ints$Upper_Bound_Mort_Reduction)/
  (ints$Lower_Bound_Cost_Estimate + ints$Upper_Bound_Cost_Estimate)
ints$worse_case <- (ints$Lower_Bound_Mort_Reduction)/(ints$Upper_Bound_Cost_Estimate)
ints$best_case <- (ints$Upper_Bound_Mort_Reduction)/(ints$Lower_Bound_Cost_Estimate)
ints$range <- (ints$best_case - ints$worse_case)/ints$avg_effect
ints$avg_mort <- (ints$Lower_Bound_Mort_Reduction + ints$Upper_Bound_Mort_Reduction)/2
ints$avg_cost <- (ints$Lower_Bound_Cost_Estimate + ints$Upper_Bound_Cost_Estimate)/2

# Note group by similar type of potential improvements
# i.e. mental health programs, well being apps should be together
# idea is that >1 activity from a group will have reduced benefits

# select a few key features from policyholders that justify choices
# ie smoking cessation programs from smokers
p <- 'Physical Health and Wellness'
h <- 'Health Maintenance and Prevention'
m <- 'Mental and Emotional Well-being'
s <- 'Safety and Preparedness' # (different aspects thus, treat as if no interaction effect)
l <- 'Lifestyle and Community'
# relabel the holistic and specialized

ints$Group <- c(p, p, h, h, h, p, p, m, m, h, h, p, p, s, s, h, h, h, l, l, l, 
                p, l, p, s, s, l, m, p, p, h, s, m, p, h, h, h, l,
                l, s, m, m, s, m, l, m, l, l, h, l)

ints <- ints %>% arrange(Group, desc(avg_effect), range)

# Chosen
# cost-effective and specific targeting criteria
chosen_ints <- ints[c(1, 2, 3,       13,      # h
                      14, 15, 16, 17, 18, 19, # l
                      25, 26, 27,             # m
                      33, 34,        35,      # p
                      44, 45, 46, 47          # s
                      ),]
# All chosen relate to each of the main causes of death
# 13 chosen to target smoking to reduce cancer deaths
# 35 chosen to help monitor results
# exercise/fitness targets I which is most of l and p
# h and s to target A/B

write.csv(chosen_ints, 'chosen_interventions.csv')

leftover_ints <- ints[-c(1, 2, 3,       13,     # h
                        14, 15, 16, 17, 18, 19, # l
                        25, 26, 27,             # m
                        33, 34,        35,      # p
                        44, 45, 46, 47          # s
                      ),]
cost_reduction_all <- sum(ints$avg_cost) - sum (chosen_ints$avg_cost)

#########
# Mortality distributions
#########
# mortality matrix 
# will have all the features and just run a function that calculates cost/mortality

mort_dist <- function(chosen_ints, group){
  mort <- seq(sum(chosen_ints[chosen_ints$Group == group,"Lower_Bound_Mort_Reduction"]), 
                sum(chosen_ints[chosen_ints$Group == group,"Upper_Bound_Mort_Reduction"]), by = 0.001)
  dnorm(mort, sum(chosen_ints[chosen_ints$Group == group,"avg_mort"]))
}
mort_dists <- c()
for (group in unique(chosen_ints$Group)){
  mort_dists <- cbind(mort_dists, mort_dist(chosen_ints, group))
}
colnames(mort_dists) <- unique(chosen_ints$Group)

overall <- 0
for (group in unique(chosen_ints$Group)){
  nrows <- nrow(chosen_ints[chosen_ints$Group == group,])
  overall <- overall + sum(chosen_ints[chosen_ints$Group == group,"avg_mort"])/((nrows+1)/2)
}
overall


##########
# Uptake # Not using this lol
##########

# Utilization distributions from research
# Assumption exponential distribution 
# main issue is that there is probably a higher probability of doing a second
# program if they do one already 

# Also assumes that app bonus points has similar effects to financial incentives
# this is justified by this https://bmcpublichealth.biomedcentral.com/articles/10.1186/1471-2458-14-141
# in the conclusions vouchers were a close alternative to cash 
x <- seq(0, nrow(chosen_ints[chosen_ints$Group == p,]), by = 0.001)
# Effects of financial Incentives 
# B49 in sheets research tab
p_freq <- dexp(x,  0.557)
# Assuming invitation method is roughly equivalent to telephone invitation
# B54 and B44 similar results
x <- seq(0, nrow(chosen_ints[chosen_ints$Group == h,]), by = 0.001)
h_freq <- dexp(x,  0.43)

x <- seq(0, nrow(chosen_ints[chosen_ints$Group == s,]), by = 0.001)
s_freq <- dexp(x,  0.43)

x <- seq(0, nrow(chosen_ints[chosen_ints$Group == l,]), by = 0.001)
l_freq <- dexp(x,  0.43)

# B56
x <- seq(0, nrow(chosen_ints[chosen_ints$Group == m,]), by = 0.001)
m_freq <- dexp(x,  0.49)


# Main causes of death
# overwhelming biggest causes:
# C - D = cancer probably (32.8%)
# I = Circulatory system (cardiovascular ) (29.4%)
# 3-7%
# V = Accidents
# J = Respiratory system
# K = digestive
# E = Endocrine, nutritional and Metabolic Disorders
# A/B = infectious/parastic dieseases (communicable or transmissable)

# Note for T20 both V and A/B are considerably higher
# ie riskier behaviour however, that is probably because the general age group is younger
# I is higher for SPWL as I occurs more for older/longer living policyholders
dth_data <- inforce_clean[inforce_clean$Death.Indicator == 1,]
dth_data %>%
  group_by(Cause.of.Death) %>%
  summarize(count = n(),
            percent = n()/nrow(dth_data)) %>%
  arrange(desc(percent))

dth_data_t20 <- inforce_clean[(inforce_clean$Death.Indicator == 1) & (inforce_clean$Policy.type == 'T20'),]
dth_data_t20 %>%
  group_by(Cause.of.Death) %>%
  summarize(count = n(),
            percent = n()/nrow(dth_data_t20)) %>%
  arrange(desc(percent))

dth_data_spwl <- inforce_clean[(inforce_clean$Death.Indicator == 1) & (inforce_clean$Policy.type == 'SPWL'),]
dth_data_spwl %>%
  group_by(Cause.of.Death) %>%
  summarize(count = n(),
            percent = n()/nrow(dth_data_spwl)) %>%
  arrange(desc(percent))

# test
ggplot(inforce_clean, aes(x = curr_age, fill = Policy.type)) +
  geom_bar(aes(y = after_stat(prop), group = Policy.type), stat = "count", position = "dodge") +
  labs(y = "Proportion", x = "Current Age") +
  theme_minimal()

##### EDA below can ignore

ggplot(inforce_clean, aes(x = Underwriting.Class, fill = Policy.type)) +
  geom_bar(aes(y = after_stat(prop), group = Policy.type), stat = "count", position = "dodge") +
  labs(y = "Proportion", x = "Current age") +
  theme_minimal()

# Note Face amount trends seem as expected
ggplot(inforce_clean, aes(x = Face.amount, fill = Policy.type)) +
  geom_bar(aes(y = after_stat(prop), group = Policy.type), stat = "count", position = "dodge") +
  labs(y = "Proportion", x = "Face amount") +
  theme_minimal()

ggplot(inforce_clean, aes(x = Face.amount, fill = Sex)) +
  geom_bar(aes(y = after_stat(prop), group = Sex), stat = "count", position = "dodge") +
  labs(y = "Proportion", x = "Face amount") +
  theme_minimal()

ggplot(inforce_clean, aes(x = Face.amount, fill = Region)) +
  geom_bar(aes(y = after_stat(prop), group = Region), stat = "count", position = "dodge") +
  labs(y = "Proportion", x = "Face amount") +
  theme_minimal()

# somewhat significant
ggplot(inforce_clean, aes(x = Face.amount, fill = Urban.vs.Rural)) +
  geom_bar(aes(y = after_stat(prop), group = Urban.vs.Rural), stat = "count", position = "dodge") +
  labs(y = "Proportion", x = "Face amount") +
  theme_minimal()

ggplot(inforce_clean, aes(x = Face.amount, fill = Underwriting.Class)) +
  geom_bar(aes(y = after_stat(prop), group = Underwriting.Class), stat = "count", position = "dodge") +
  labs(y = "Proportion", x = "Face amount") +
  theme_minimal()

ggplot(inforce_clean, aes(x = Face.amount, fill = Smoker.Status)) +
  geom_bar(aes(y = after_stat(prop), group = Smoker.Status), stat = "count", position = "dodge") +
  labs(y = "Proportion", x = "Face amount") +
  theme_minimal()

# Alive
alive_data <- inforce_clean[inforce_clean$Death.Indicator != 1,]

ggplot(alive_data, aes(x = curr_age, fill = Smoker.Status)) +
  geom_bar(aes(y = after_stat(prop), group = Smoker.Status), stat = "count", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(y = "Proportion", x = "Current age") +
  theme_minimal()

ggplot(alive_data, aes(x = curr_age, fill = Policy.type)) +
  geom_bar(aes(y = after_stat(prop), group = Policy.type), stat = "count", position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(y = "Proportion", x = "Current age") +
  theme_minimal()


# Death
dth_data <- inforce_clean[inforce_clean$Death.Indicator == 1,]

# age at death split by policy
ggplot(dth_data, aes(x = age_at_death, fill = Policy.type)) +
  geom_bar(aes(y = after_stat(prop), group = Policy.type), stat = "count", position = "dodge") +
  labs(y = "Proportion", x = "Age at Death") +
  theme_minimal()

# cause of death graphs split by policy
t20_dth_data <- dth_data[dth_data$Policy.type == 'T20',]

ggplot(t20_dth_data, aes(x = age_at_death, fill = Cause.of.Death)) +
  geom_bar() +
  labs(y = "Deaths", x = "Age at Death", title = 'T20') +
  theme_minimal()

spwl_dth_data <- dth_data[dth_data$Policy.type == 'SPWL',]

ggplot(spwl_dth_data, aes(x = age_at_death, fill = Cause.of.Death)) +
  geom_bar() +
  labs(y = "Deaths", x = "Age at Death", title = 'SPWL') +
  theme_minimal()

# split without age
ggplot(t20_dth_data, aes(x = Cause.of.Death, fill = Cause.of.Death)) +
  geom_bar() +
  labs(y = "Deaths", x = "Age at Death", title = 'T20') +
  theme_minimal()

ggplot(spwl_dth_data, aes(x = Cause.of.Death, fill = Cause.of.Death)) +
  geom_bar() +
  labs(y = "Deaths", x = "Age at Death", title = 'SPWL') +
  theme_minimal()

ggplot(dth_data, aes(x = Face.amount, fill = Cause.of.Death)) +
  geom_bar() +
  labs(y = "Proportion", x = "Face amount") +
  theme_minimal()

# get the population at each age 
curr_age <- inforce_clean %>% group_by(curr_age) %>% summarize(count = n()) 
plot(curr_age$curr_age, (curr_age$count))

dth_data <- inforce_clean[inforce_clean$Death.Indicator == 1,]

age_dth_plot <- dth_data %>% group_by(age_at_death) %>% summarize(count = n()) 
plot(age_dth_plot$age_at_death, age_dth_plot$count)
plot(age_dth_plot$age_at_death, cumsum(age_dth_plot$count)/nrow(dth_data))

# general 
inforce_clean %>% group_by(Region) %>% summarize(count = n(), 
                                                 mean_issue_age = mean(Issue.age),
                                                 mean_death_age = mean(age_at_death, na.rm = TRUE),
                                                 urban_prop = sum(Urban.vs.Rural == "Urban")/n(), 
                                                 smoker_prop = sum(Smoker.Status == "S")/n(),
                                                 male_prop = sum(Sex == "M")/n(),
                                                 avg_face_amt = mean(Face.amount),
                                                 very_low_risk = sum(Underwriting.Class == "very low risk")/n(),
                                                 low_risk = sum(Underwriting.Class == "low risk")/n(),
                                                 moderate_risk = sum(Underwriting.Class == "moderate risk")/n(),
                                                 high_risk = sum(Underwriting.Class == "high risk")/n(),
                                                 agent_prop = sum(Distribution.Channel == "Telemarketer")/n(),
                                                 prop_death = mean(Death.Indicator))


inforce_clean %>% group_by(Underwriting.Class) %>% summarize(count = n(), 
                                                             mean_issue_age = mean(Issue.age),
                                                             mean_death_age = mean(age_at_death, na.rm = TRUE),
                                                             urban_prop = sum(Urban.vs.Rural == "Urban")/n(), 
                                                             smoker_prop = sum(Smoker.Status == "S")/n(),
                                                             male_prop = sum(Sex == "M")/n(),
                                                             avg_face_amt = mean(Face.amount),
                                                             agent_prop = sum(Distribution.Channel == "Telemarketer")/n(),
                                                             prop_death = mean(Death.Indicator))

inforce_clean %>% group_by(Smoker.Status) %>% summarize(count = n(), 
                                                 mean_issue_age = mean(Issue.age),
                                                 mean_death_age = mean(age_at_death, na.rm = TRUE),
                                                 prop_death = mean(Death.Indicator),
                                                 urban_prop = sum(Urban.vs.Rural == "Urban")/n(), 
                                                 male_prop = sum(Sex == "M")/n(),
                                                 avg_face_amt = mean(Face.amount),
                                                 very_low_risk = sum(Underwriting.Class == "very low risk")/n(),
                                                 low_risk = sum(Underwriting.Class == "low risk")/n(),
                                                 moderate_risk = sum(Underwriting.Class == "moderate risk")/n(),
                                                 high_risk = sum(Underwriting.Class == "high risk")/n(),
                                                 agent_prop = sum(Distribution.Channel == "Telemarketer")/n())

# plot comparing just the dead 
ggplot(dth_data, aes(x = age_at_death, fill = Smoker.Status)) +
  geom_bar(aes(y = ..prop.., group = Smoker.Status), stat = "count", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(y = "Proportion", x = "Age at Death") +
  theme_minimal()

# Calculate proportion of death compared to all split by smokers
num_s <- sum(inforce_clean$Smoker.Status == 'S')
num_ns <- sum(inforce_clean$Smoker.Status != 'S')

death_prop <- dth_data %>% group_by(age_at_death, Smoker.Status) %>% summarize(count = n()) 
death_prop$prop[death_prop$Smoker.Status == 'S'] <- 
  death_prop$count[death_prop$Smoker.Status == 'S']/num_s
death_prop$prop[death_prop$Smoker.Status != 'S'] <- 
  death_prop$count[death_prop$Smoker.Status != 'S']/num_ns
ggplot(death_prop, aes(age_at_death, prop, fill = Smoker.Status)) + 
  geom_bar(stat = 'identity', alpha = 0.5, position = 'identity')

# from all deaths
death_prop <- dth_data %>% group_by(age_at_death, Smoker.Status) %>% summarize(count = n()) 
death_prop$prop[death_prop$Smoker.Status == 'S'] <- 
  death_prop$count[death_prop$Smoker.Status == 'S']/nrow(inforce_clean)
death_prop$prop[death_prop$Smoker.Status != 'S'] <- 
  death_prop$count[death_prop$Smoker.Status != 'S']/nrow(inforce_clean)
ggplot(death_prop, aes(age_at_death, prop, fill = Smoker.Status)) + 
  geom_bar(stat = 'identity', alpha = 0.5, position = 'identity')
