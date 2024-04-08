# In progress - Sam 5/3/24
# import libraries
library("dplyr")
library("ggplot2")
library("lifecontingencies")
library("reshape2")
library("tidyr")
library("data.table")
library('survival')

# Import data
inforce_raw <- read.csv("~/ACTL4001/Case Data/2024-srcsc-superlife-inforce-dataset.csv")
summary(inforce_raw)
n = nrow(inforce_raw)

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

# Numeric years
inforce_clean$Year.of.Death <- as.numeric(inforce_clean$Year.of.Death)
inforce_clean$Year.of.Lapse <- as.numeric(inforce_clean$Year.of.Lapse)
inforce_clean$Issue.year <- as.numeric(inforce_clean$Issue.year)

inforce_clean$Underwriting.Class = factor(inforce_clean$Underwriting.Class, levels = c("very low risk", "low risk", "moderate risk", "high risk"))

# NOTE years of lapse and death have na because they have not occurred
inforce_clean$age_at_death <- inforce_clean$Issue.age + inforce_clean$Year.of.Death - inforce_clean$Issue.year
inforce_clean$curr_age <- inforce_clean$Issue.age + 2024 - inforce_clean$Issue.year
inforce_clean$birth_year <- inforce_clean$Issue.year - inforce_clean$Issue.age
inforce_clean$year_of_policy <- 2024 - inforce_clean$Issue.year


# For each year, for each policy in the dataset, we want to project whether they survive, die or lapse within the period of the next year.
# Everything below here will be repeated for each year
# Ignore this for now it is not working


# gender and smoking assumptions for life table
lumaria_qx <- read.csv("~/ACTL4001/Case Data/srcsc-2024-lumaria-mortality-table-CLEANED.csv", header = TRUE)
life_table <- probs2lifetable(probs = lumaria_qx$Mortality.Rate, 
                              type = "qx", 
                              radix = 100000, 
                              name = "Lumaria_Life_Table")
life_table_df <- data.frame(life_table@x, life_table@lx)


# Function to generate qx for a given hazard ratio on top of an existing qx
hazard_ratio_qx <- function(qx, fm_ratio){
  force.m <- -log(1 - qx)
  new_force.m <- force.m * fm_ratio
  new_qx <- 1 - exp(-new_force.m)
}


# future projection - based off lumaria life table (not inforce dataset) 
# dont want to capture trends such as covid for future projection

# Assumptions
male_prop <- 0.5  # no data given - estimate (should be reasonable for birth)
smoking_prop <- 0.18  # given - only for over 18s



male_RR_NS <- 1.40
male_RR_S <- 1.60
S_RR_male <- 2.24
S_RR_female <- 2.29

# Let's say baseline is female and nonsmoking

# Build Hazard Ratios

female_NS <- 0.7856
male_NS <- 1
female_S <- 0.7856*8.5915
male_S <- 8.5915 # averaging the two ways of estimating this

new_life_table <- life_table_df
new_life_table$male_NS.lx <- 0
new_life_table$female_NS.lx <- 0
new_life_table$male_S.lx <- 0
new_life_table$female_S.lx <- 0
new_life_table$male_NS.lx[1] <- life_table_df$life_table.lx[1] * male_prop
new_life_table$female_NS.lx[1] <- life_table_df$life_table.lx[1] * (1 - male_prop)

calculate_baseline <- function(baseline, 
                               female_NS, male_NS, female_S, male_S,
                               female_NS.lx, male_NS.lx, female_S.lx, male_S.lx,
                               dx){
  return(dx - ((1 - exp(-baseline * female_NS)) * female_NS.lx + 
           (1 - exp(-baseline * male_NS)) * male_NS.lx + 
           (1 - exp(-baseline * female_S)) * female_S.lx + 
           (1 - exp(-baseline * male_S)) * male_S.lx))
}

for(i in 1:(112 - 1)){
  dx <- new_life_table$life_table.lx[i] - new_life_table$life_table.lx[i+1]
  baseline_mortality <- uniroot(calculate_baseline, c(0,1000),
          female_NS = female_NS, male_NS = male_NS, female_S = female_S, male_S = male_S,
          female_NS.lx = new_life_table$female_NS.lx[i], male_NS.lx = new_life_table$male_NS.lx[i], 
          female_S.lx = new_life_table$female_S.lx[i], male_S.lx = new_life_table$male_S.lx[i],
          dx = dx, tol=0.000001)$root
  new_life_table$female_NS.lx[i+1] <- new_life_table$female_NS.lx[i] * exp(-1 * baseline_mortality * female_NS) 
  new_life_table$male_NS.lx[i+1] <- new_life_table$male_NS.lx[i] * exp(-1 * baseline_mortality * male_NS) 
  new_life_table$female_S.lx[i+1] <- new_life_table$female_S.lx[i] * exp(-1 * baseline_mortality * female_S) 
  new_life_table$male_S.lx[i+1] <- new_life_table$male_S.lx[i] * exp(-1 * baseline_mortality * male_S) 
  
  # start putting people into smoking at age 18, after calculating how many live
  if(i == 18){
    new_life_table$female_S.lx[i+1] <- new_life_table$female_NS.lx[i+1] * 0.18
    new_life_table$male_S.lx[i+1] <- new_life_table$male_NS.lx[i+1] * 0.18
    new_life_table$female_NS.lx[i+1] <- new_life_table$female_NS.lx[i+1] * 0.82
    new_life_table$male_NS.lx[i+1] <- new_life_table$male_NS.lx[i+1] * 0.82
    
  }
}

new_life_table$female_NS_base.lx <- new_life_table$female_NS.lx * c(rep(1, 18), rep(1/0.82, 102)) * 2
new_life_table$male_NS_base.lx <- new_life_table$male_NS.lx * c(rep(1, 18), rep(1/0.82, 102)) * 2
new_life_table$female_S_base.lx <- new_life_table$female_S.lx * 1/0.18 * (new_life_table$female_NS_base.lx[1]/new_life_table$female_NS_base.lx[19]) * 2
new_life_table$male_S_base.lx <- new_life_table$male_S.lx * 1/0.18 * (new_life_table$male_NS_base.lx[1]/new_life_table$male_NS_base.lx[19]) * 2


# checking if they match the aggregate numbers in original life table
new_life_table$diff <- new_life_table$life_table.lx - new_life_table$male_NS.lx - new_life_table$female_NS.lx - new_life_table$male_S.lx - new_life_table$female_S.lx
new_life_table$diff
# Creating qx values for the gender + smoking status life table
new_life_probs <- data.frame("x" = new_life_table$life_table.x, "female_S_qx" = rep(0, 120), "male_S_qx" = rep(0, 120))
for(i in 1:nrow(new_life_probs)){
  new_life_probs$female_NS_qx[i] <- 1 - new_life_table$female_NS_base.lx[i+1]/new_life_table$female_NS_base.lx[i] 
  new_life_probs$male_NS_qx[i] <- 1 - new_life_table$male_NS_base.lx[i+1]/new_life_table$male_NS_base.lx[i]
  if(i >= 19){
    new_life_probs$female_S_qx[i] <- 1 - new_life_table$female_S_base.lx[i+1]/new_life_table$female_S_base.lx[i]
    new_life_probs$male_S_qx[i] <- 1 - new_life_table$male_S_base.lx[i+1]/new_life_table$male_S_base.lx[i]  
  }
}

# The hazard ratio assumption method starts to fall apart with such low numbers
# so we take rates similar to the life table qx rates to make them logical and roughly smooth
# This is unlikely to have any real impact on the numbers

new_life_probs$female_NS_qx[111:120] <- hazard_ratio_qx(lumaria_qx$Mortality.Rate[111:120], 0.84)
new_life_probs$male_NS_qx[111:120] <- hazard_ratio_qx(lumaria_qx$Mortality.Rate[111:120], 1.3)

# Comparisons of some rates
ggplot(new_life_table) + geom_line(aes(x=life_table.x, y=male_NS_base.lx), col = "blue") +
  geom_line(aes(x=life_table.x, y=female_NS_base.lx), col = "red")

ggplot(new_life_probs) + geom_line(aes(x=x, y=male_NS_qx), col = "blue") +
  geom_line(aes(x=x, y=female_NS_qx), col = "red")


# reshaping to a long table with columns for Age, Sex and Smoker Status
base_mortality = new_life_probs %>% pivot_longer(cols = !x, names_to = c("Sex", "Smoker.Status"), 
                                names_pattern = "(.*)_(.*)_qx",
                                values_to = "qx")

base_mortality$Sex[base_mortality$Sex == "female"] <- "F"
base_mortality$Sex[base_mortality$Sex == "male"] <- "M"



# LAPSE MODELLING 
a <- inforce_clean %>% group_by(curr_age) %>% summarize(lapse_rate = sum(Lapse.Indicator)/n(), 
                                                        exposure = n(),
                                                        lapse_sd = sd(Lapse.Indicator)/n())

b <- inforce_clean %>% group_by(curr_age, Issue.age) %>%
  summarize(no = n())



c <- inforce_clean %>% filter(Policy.type == "T20", year_of_policy < 20) %>% group_by(curr_age, year_of_policy) %>%
  summarize(lapse_rate = sum(Lapse.Indicator)/n(), 
            exposure = n(),
            lapse_sd = sd(Lapse.Indicator))

d <- inforce_clean %>% filter(Policy.type == "T20", year_of_policy < 20) %>% 
  mutate(cohort = floor(Issue.year/5)) %>%
  group_by(curr_age, year_of_policy, cohort) %>%
  summarize(lapse_rate = sum(Lapse.Indicator)/n(), 
            exposure = n(),
            lapse_sd = sd(Lapse.Indicator))


ggplot(c) + geom_tile(aes(x = curr_age, y = year_of_policy, fill = lapse_rate))
ggplot(d) + geom_tile(aes(x = cohort, y = year_of_policy, fill = lapse_rate))

# lapse should be a function of current age, year of policy and cohort?
inforce_aggregated <- data.frame(matrix(ncol = 8, nrow = 0))

# for loop to go through each policy (slow)
for(i in 1:100){
  this_policy <- inforce_clean[i,]
  exit_date <- min(this_policy$Year.of.Lapse, this_policy$Year.of.Death, 2023, na.rm=T)
  final_year <- exit_date - this_policy$Issue.year + 1
  temp <- data.frame(matrix(ncol = 8, nrow = 0))
  
  if(final_year > 1){
    for(j in 1:(final_year - 1)){
      temp <- rep()
      inforce_aggregated <- rbind(inforce_aggregated, 
                                c(this_policy$Policy.number, this_policy$Issue.year + j-1, this_policy$Issue.age + j-1,
                                  this_policy$Issue.age, j,
                                  this_policy$Issue.year, 0, 0))
    }
  }
  inforce_aggregated <- rbind(inforce_aggregated, 
                              c(this_policy$Policy.number, exit_date, this_policy$Issue.age + final_year - 1, 
                                this_policy$Issue.age, final_year,
                                this_policy$Issue.year, this_policy$Death.Indicator, this_policy$Lapse.Indicator))
}

colnames(inforce_aggregated) <- c("Sex", "Smoker.Status", "Issue.age", "year_of_policy", "Issue.year", "Death.indicator", "Lapse.indicator")
inforce_aggregated


# gathering the data
# function to do the same thing
retrieve_history <- function(policy_raw){
  policy_raw <- as.data.frame(policy_raw)
  policy <- transpose(policy_raw)
  colnames(policy) <- rownames(policy_raw)
  
  
  policy$Issue.year <- as.numeric(policy$Issue.year)
  policy$Issue.age <- as.numeric(policy$Issue.age)
  policy$Year.of.Lapse <- as.numeric(policy$Year.of.Lapse)
  policy$Year.of.Death <- as.numeric(policy$Year.of.Death)
  
  exit_date <- min(policy$Year.of.Lapse, policy$Year.of.Death, 2023, na.rm=T)
  
  final_year <- exit_date - policy$Issue.year + 1
  temp <- data.frame(matrix(ncol = 8, nrow = 0))
  
  if(final_year > 1){
    for(j in 1:(final_year - 1)){
      temp <- data.frame(#policy$Issue.year + 0:(final_year - 2),
              # policy$Issue.age + 0:(final_year - 2), 
              policy$Sex, policy$Smoker.Status,
              policy$Issue.age, 1:(final_year - 1),
              policy$Issue.year, 0, 0)
      }
    }
  temp <- rbind(temp, c(#exit_date, policy$Issue.age + final_year - 1, 
                          policy$Sex, policy$Smoker.Status,
                          policy$Issue.age, final_year,
                          policy$Issue.year, policy$Death.Indicator, policy$Lapse.Indicator))
}


# Aggregate together the year by year history of each policy
# a policy that has been in force for 17 year will have 17 rows

# ==== Don't run the first commented line - it will take an hour to run. The data is 
# stored in inforce_aggregated.csv ====

inforce_aggregated <- data.frame(rbindlist(apply(inforce_clean[inforce_clean$Policy.type == "T20",], 1, function(row) retrieve_history(row))))
# inforce_aggregated <- data.frame(rbindlist(apply(inforce_clean[1:1000,], 1, function(row) retrieve_history(row))))
colnames(inforce_aggregated) <- c("Sex", "Smoker.Status", "Issue.age", "year_of_policy", "Issue.year", "Death.indicator", "Lapse.indicator")
# Missing year and current age - we can recalculate these later 


inforce_aggregated$Issue.age <- as.numeric(inforce_aggregated$Issue.age)
inforce_aggregated$year_of_policy <- as.numeric(inforce_aggregated$year_of_policy)
inforce_aggregated$Issue.year <- as.numeric(inforce_aggregated$Issue.year)
inforce_aggregated$Death.indicator <- as.numeric(inforce_aggregated$Death.indicator)
inforce_aggregated$Lapse.indicator <- as.numeric(inforce_aggregated$Lapse.indicator)

# write.csv(inforce_aggregated, "inforce_aggregated_2.csv")

inforce_aggregated <- read.csv("inforce_aggregated_2.csv")
inforce_aggregated$Year <- inforce_aggregated$Issue.year + inforce_aggregated$year_of_policy - 1
inforce_aggregated$curr_age <- inforce_aggregated$Issue.age + inforce_aggregated$year_of_policy - 1

lapse_df <- inforce_aggregated %>% group_by(curr_age, year_of_policy, Issue.year) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n()) 


lapse_df_grouped <- inforce_aggregated %>% 
  group_by(curr_age, year_of_policy, floor(Issue.year/5)) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n(),
            Issue.year = mean(Issue.year))

lapse_df_basic <- inforce_aggregated %>% 
  group_by(curr_age, year_of_policy) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n(),
            Issue.year = mean(Issue.year))
aaa <- lapse_df_basic[c(1,2,4)] %>% pivot_wider(names_from = year_of_policy, values_from = lapse_rate)
print(aaa[c(1,14:21)], n=1000)

lapse_df_basic_grouped <- inforce_aggregated %>% 
  group_by(floor(curr_age/5)*5, year_of_policy) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n(),
            Issue.year = mean(Issue.year))
colnames(lapse_df_basic_grouped)[1] <- "age_group"

# Year of policy doesn't seem to have an effect on lapse rate, apart from the final year of term
lapse_df_2 <- inforce_aggregated %>%
  group_by(year_of_policy) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(lapse_df_2, n=1000)

  # Current age doesn't seem to have an effect on lapse rate
lapse_df_3 <- inforce_aggregated %>%
  group_by(curr_age) %>% filter(year_of_policy < 20) %>%
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(lapse_df_3, n=1000)

# Issue age has an effect on lapse rate - decreases by issue age
lapse_df_4 <- inforce_aggregated %>%
  group_by(Issue.age) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(lapse_df_4, n=1000)

# Actually, after removing the final year lapses, issue age doesn't have an effect on lapse rate
lapse_df_4.1 <- inforce_aggregated %>% filter(year_of_policy < 20) %>%
  group_by(Issue.age) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())

# By issue age and year of policy - still no significant trend
lapse_df_5 <- inforce_aggregated %>%
  group_by(Issue.age, year_of_policy) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
bbb <- lapse_df_5[c(1,2,4)] %>% pivot_wider(names_from = year_of_policy, values_from = lapse_rate)
print(bbb[c(1,13:21)], n=1000)


# Investigating what happens at maturity - those who are older are slightly less 
# likely to lapse at maturity, due to higher chance they die during the year
lapse_df_maturity <- inforce_aggregated %>% filter(year_of_policy == 20) %>%
  group_by(Issue.age) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(n=1000,lapse_df_maturity)

lapse_df_maturity_2 <- inforce_aggregated %>% filter(year_of_policy == 20) %>%
  group_by(Issue.year) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(n=1000,lapse_df_maturity_2)
  

ggplot(lapse_df_basic_grouped) + geom_tile(aes(x = age_group + 2.5, y = year_of_policy, fill = lapse_rate))

# glms (now irrelevant - just use a single estimate for all years before maturity)
# glm(lapse_rate ~ curr_age + year_of_policy + Issue.year, family = "gaussian", data = lapse_df)
# log_glm <- glm(Lapse.indicator ~ curr_age + year_of_policy + Issue.year, family = "binomial", data = inforce_aggregated)
# log_glm2 <- glm(Lapse.indicator ~ year_of_policy, family = "binomial", data = inforce_aggregated)




# ==================== MOrtality INvestigation ====================
death_df <- inforce_aggregated %>% group_by(curr_age, year_of_policy, Issue.year) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n()) 
ggplot(death_df) + geom_point(aes(x=curr_age, y=death_rate))


death_df_1 <- inforce_aggregated %>% group_by(curr_age, Issue.year) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n()) 
ggplot(death_df_1) + geom_point(aes(x=curr_age, y=death_rate))
ggplot(death_df_1) + geom_point(aes(x=Issue.year, y=death_rate))


death_df_2 <- inforce_aggregated %>% group_by(curr_age) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(death_df_2, n=1000)
ggplot(death_df_2) + geom_point(aes(x=curr_age, y=death_rate))


death_df_3 <- inforce_aggregated %>%
  group_by(curr_age, Year) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(death_df_3, n=1000)

ggplot(death_df_3) + geom_tile(aes(x=curr_age, y=Year, fill=log(death_rate)))

death_df_4 <- inforce_aggregated %>% group_by(Issue.year) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(death_df_4, n=1000)
ggplot(death_df_4) + geom_point(aes(x=Issue.year, y=death_rate))


death_df_5 <- inforce_aggregated %>% group_by(curr_age, Sex) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(death_df_5, n=1000)
ggplot(death_df_5) + geom_point(aes(x=curr_age, y=death_rate, col=Sex))


death_df_6 <- inforce_aggregated %>% group_by(curr_age, Smoker.Status) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(death_df_6, n=1000)
ggplot(death_df_6) + geom_point(aes(x=curr_age, y=death_rate, col=Smoker.Status))


death_df_7 <- inforce_aggregated %>% group_by(curr_age, Smoker.Status, Sex) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(death_df_7, n=1000)
ggplot(death_df_7) + geom_point(aes(x=curr_age, y=log(death_rate), col=Sex, shape=Smoker.Status))


death_df_8 <- inforce_aggregated %>% group_by(curr_age, "covid" = factor(floor(Year/2020))) %>% 
  summarize(exposure = n(),
            lapse_rate = sum(Lapse.indicator)/n(),
            death_rate = sum(Death.indicator)/n())
print(death_df_8, n=1000)
ggplot(death_df_8) + geom_line(aes(x=curr_age, y=log(death_rate), group=covid, col = covid))



# death_glm <- glm(Death.indicator ~ curr_age + year_of_policy + Issue.year, family = "binomial", data = inforce_aggregated)

# death_glm <- glm(Death.indicator ~ curr_age + Sex + Smoker.Status, family = "binomial", data = inforce_aggregated)


# Cox regression modelling
library("survminer")
library("survival")
library("rms")

surv.cox <- coxph(Surv(rep(1, nrow(inforce_aggregated)), Death.indicator) ~ Sex + curr_age, data = inforce_aggregated)
summary(surv.cox)
# BROKEN ggsurvplot(surv.cox, data = inforce_aggregated, conf.int = TRUE, legend.labs=c("Sex=F", "Sex=M"),
#           ggtheme = theme_minimal())

surv.cox2 <- coxph(Surv(rep(1, nrow(inforce_aggregated)), Death.indicator) ~ Sex + curr_age + Issue.year, data = inforce_aggregated)

# Check this later 
# https://stackoverflow.com/questions/42069425/interval-censored-data-cox-proportional-hazard-and-surival-difference-in-r
# https://www.r-bloggers.com/2016/12/cox-proportional-hazards-model/
# https://stackoverflow.com/questions/41968606/left-censoring-for-survival-data-in-r


# using icenReg package
install.packages("icenReg")
library("icenReg")

# The +1 is because e.g. someone who has been issued a policy in 2001 and lapses in 2003
# would have had 3 years of exposure (not 2)
# Issue is assumed to be at the beginning of year and lapse at end of year
inforce_clean$Final.age <- inforce_clean$Issue.age + pmin(inforce_clean$Year.of.Lapse, 
                                                         inforce_clean$Year.of.Death, 
                                                         c(rep(2023, nrow(inforce_clean))), na.rm=T) -
                           inforce_clean$Issue.year + 1


inforce_surv <- Surv(time = inforce_clean$Issue.age, time2 = inforce_clean$Final.age, event=inforce_clean$Death.Indicator)

# inforce_fit <- ic_sp(inforce_surv ~ Sex, data = inforce_clean, model = "po")
# inforce_fit2 <- ic_sp(inforce_surv ~ Sex, data = inforce_clean, model = "po")
# plot(inforce_fit, col=c('blue', 'orange'), xlab="Time", ylab="Estimated Survival")


# ======== Cox ph fitting - covariate selection ========

# 1 - Sex
res.cox <- coxph(Surv(time = Issue.age, time2 = Final.age, Death.Indicator) ~ Sex, data = inforce_clean)
sex_df <- with(inforce_clean,
               data.frame(Sex = c("M","F")))
fit <- survfit(res.cox, newdata = sex_df)

# This survplot works whereas all the ones before it don't, investigate this in a bit
ggsurvplot(fit, conf.int = TRUE, legend.labs=c("Sex=M", "Sex=F"),
          ggtheme = theme_minimal(), data = inforce_clean)
temp <- cox.zph(res.cox)
plot(temp)

# 2 - Sex + Smoker.Status
res.cox2 <- coxph(Surv(time = Issue.age, time2 = Final.age, Death.Indicator) ~ Sex + Smoker.Status, data = inforce_clean)
cov_df <- with(inforce_clean,
               data.frame(Sex = c("M","F", "M", "F"), Smoker.Status = c("NS", "NS", "S", "S")))
fit <- survfit(res.cox2, newdata = cov_df)
ggsurvplot(fit, conf.int = TRUE,
           ggtheme = theme_minimal(), data = inforce_clean, legend.labs=c("NS M", "NS F", "S M", "S F"))

# 3 - Sex + Smoker.Status + birth_year
res.cox3 <- coxph(Surv(time = Issue.age, time2 = Final.age, Death.Indicator) ~ Sex + Smoker.Status + birth_year, data = inforce_clean)
cov_df3 <- with(inforce_clean,
               data.frame(Sex = c(rep("F", 5)), Smoker.Status = c(rep("NS", 5)), birth_year = c(1950, 1960, 1970, 1980, 1990)))
fit <- survfit(res.cox3, newdata = cov_df3)
ggsurvplot(fit, conf.int = TRUE,
           ggtheme = theme_minimal(), data = inforce_clean, legend.labs=c(1950, 1960, 1970, 1980, 1990))

  

# 4 - Sex + Smoker.Status + Underwriting.Class
# Since Underwriting class is influenced significantly by smoker status, here we compare the difference
# between underwriting classes whilst keeping Sex and Smoker.Status constant.
# For female non-smokers:
res.cox4 <- coxph(Surv(time = Issue.age, time2 = Final.age, Death.Indicator) ~ Sex + Smoker.Status + Underwriting.Class, data = inforce_clean)
cov_df4 <- with(inforce_clean,
                data.frame(Sex = c(rep("F", 4)), Smoker.Status = c(rep("NS", 4)), Underwriting.Class = c("very low risk", "low risk", "moderate risk", "high risk")))
fit <- survfit(res.cox4, newdata = cov_df4)
# Comparison of mortality when underwriting class changes where Sex and Smoker.Status stay the same
ggsurvplot(fit, conf.int = TRUE,
           ggtheme = theme_minimal(), data = inforce_clean)

# For male non-smokers:
cov_df4.1 <- with(inforce_clean,
                data.frame(Sex = c(rep("M", 4)), Smoker.Status = c(rep("NS", 4)), Underwriting.Class = c("very low risk", "low risk", "moderate risk", "high risk")))
fit <- survfit(res.cox4, newdata = cov_df4.1)
ggsurvplot(fit, conf.int = TRUE,
           ggtheme = theme_minimal(), data = inforce_clean)


# We conclude that Sex and Smoker.Status will be the only covariates used in the Cox PH model.
# Both covariates are significant, as is the model (likelihood ratio test, Wald test, Score test)
historical.coxph <- coxph(Surv(time = Issue.age, time2 = Final.age, Death.Indicator) ~ Sex + Smoker.Status, data = inforce_clean)
historical_cov_df <- with(inforce_clean,
               data.frame(Sex = c("M","F", "M", "F"), Smoker.Status = c("NS", "NS", "S", "S")))
historical_fit <- survfit(res.cox2, newdata = cov_df)
historical_fit_summary <- summary(historical_fit)
temp <- cox.zph(historical.coxph)
plot(temp)
ggforest(historical.coxph)

#--------------------------------------------------------------------------------------------

# Use proportional methods on the baseline population to seperate out

pop_raw = read.csv("~/ACTL4001/Case Data/2024-srcsc-superlife-inforce-dataset.csv")
colnames(pop_raw)[1] = "Policy.Number"

mortality_table = lumaria_qx
colnames(mortality_table)[1] = "Age"

inforce_current <- inforce_raw
inforce_current$Year.of.Death[is.na(inforce_current$Year.of.Death)] <- 2023
inforce_current$Year.of.Lapse[is.na(inforce_current$Year.of.Lapse)] <- 2023


inforce_current <- inforce_current %>%
  mutate(time = case_when(Year.of.Death < Year.of.Lapse ~ Year.of.Death,
                          Year.of.Lapse < Year.of.Death ~ Year.of.Lapse,
                          .default = 2023) + 0.5,
         censored_death = case_when(Year.of.Death < Year.of.Lapse ~ 1,
                                    Year.of.Lapse < Year.of.Death ~ 0,
                                    .default = 0),
         age_length = Issue.age + time - Issue.year,
         gender = factor(Sex, levels = c("M","F")),
         smoking = factor(Smoker.Status, levels = c("NS", "S")),
         distribution = factor(Distribution.Channel, levels = unique(inforce_current$Distribution.Channel)),
         region = factor(Region, levels = unique(inforce_current$Region)),
         Policy = factor(Policy.type, levels = unique(inforce_current$Policy.type)))


Population_makeup <- c(sum(pop_raw$Sex == "M" & pop_raw$Smoker.Status == "S")/nrow(pop_raw), 
                       sum(pop_raw$Sex == "M" & pop_raw$Smoker.Status == "NS")/nrow(pop_raw),
                       sum(pop_raw$Sex == "F" & pop_raw$Smoker.Status == "S")/nrow(pop_raw),
                       sum(pop_raw$Sex == "F" & pop_raw$Smoker.Status == "NS")/nrow(pop_raw))

hazards <- coxph(Surv(Issue.age, age_length, censored_death) ~ gender + smoking, data = inforce_current)

Relative_Risk <- c(exp(hazards$coefficients["smokingS"]),1,
                   exp(hazards$coefficients["smokingS"] + hazards$coefficients["genderF"]),exp(hazards$coefficients["genderF"]))

Relative_Risk_Pop <- Relative_Risk / sum(Relative_Risk * Population_makeup)




# Additional Risk Loading
Relative_Risk_Pop * exp(1.8)

mortality_table[,"M_S.qx"] = 1 - (1-mortality_table[,"Mortality.Rate"]) ^Relative_Risk_Pop[1]
mortality_table[,"M_NS.qx"] = 1 - (1-mortality_table[,"Mortality.Rate"])^Relative_Risk_Pop[2]
mortality_table[,"F_S.qx"] = 1 - (1-mortality_table[,"Mortality.Rate"]) ^Relative_Risk_Pop[3]
mortality_table[,"F_NS.qx"] = 1 - (1-mortality_table[,"Mortality.Rate"]) ^Relative_Risk_Pop[4]


plot(c(1:120), mortality_table$M_NS.qx, typ = "l", col = 'orange')
lines(c(1:120), mortality_table$M_S.qx, typ = "l", col = "red")
lines(c(1:120), mortality_table$Mortality.Rate, typ = "l")
lines(c(1:120), mortality_table$F_NS.qx, typ = "l", col = "blue")
lines(c(1:120), mortality_table$F_S.qx, typ = "l", col = "green")


#--------------------------------------------------------------------------------------------

# Comparison
historical_life_table <- data.frame(historical_fit_summary$time, historical_fit_summary$surv)
colnames(historical_life_table)[1:5] <- c("Age", "M_NS", "F_NS", "M_S", "F_S")

comparison_life_table <- merge(mortality_table, historical_life_table,by.y="Age")
comparison_life_table[c("M_NS", "F_NS", "M_S", "F_S")] <- comparison_life_table[c("M_NS", "F_NS", "M_S", "F_S")] * 
  c(comparison_life_table[1, c("male_NS_base.lx", "female_NS_base.lx", "male_S_base.lx", "female_S_base.lx")]) /
  c(comparison_life_table[1, c("M_NS", "F_NS", "M_S", "F_S")])



comparison_life_table


# ggplot(data = comparison_life_table) + geom_point(aes(x = life_table.x, y=F_NS)) +
#   geom_point(aes(x = life_table.x, y=female_NS_base.lx)) + xlab("Age") + ylab("lx")
# ggplot(data = comparison_life_table) + geom_point(aes(x = life_table.x, y=M_NS)) +
#   geom_point(aes(x = life_table.x, y=male_NS_base.lx), col= 'red')
# 
# ggplot(data = comparison_life_table) + geom_point(aes(x = life_table.x, y=F_S)) +
#   geom_point(aes(x = life_table.x, y=female_S_base.lx), col = 'red') + xlab("Age") + ylab("lx")
# ggplot(data = comparison_life_table) + geom_point(aes(x = life_table.x, y=M_S)) +
#   geom_point(aes(x = life_table.x, y=male_S_base.lx), col= 'red')

comparison_life_table[1:61,7:10] <- 1- comparison_life_table[2:62,7:10]/ comparison_life_table[1:61,7:10]
comparison_life_table




# # Changing from lx to qx format
# comparison_life_table$historical_male_NS_qx <- rep(0, 62)
# comparison_life_table$historical_female_NS_qx <- rep(0, 62)
# comparison_life_table$historical_male_S_qx <- rep(0, 62)
# comparison_life_table$historical_female_S_qx <- rep(0, 62)
# 
# for(i in 1:nrow(comparison_life_table)){
#   comparison_life_table$historical_female_NS_qx[i] <- 1 - comparison_life_table$F_NS[i+1]/comparison_life_table$F_NS[i] 
#   comparison_life_table$historical_male_NS_qx[i] <- 1 - comparison_life_table$M_NS[i+1]/comparison_life_table$M_NS[i]
#   comparison_life_table$historical_female_S_qx[i] <- 1 - comparison_life_table$F_S[i+1]/comparison_life_table$F_S[i]
#   comparison_life_table$historical_male_S_qx[i] <- 1 - comparison_life_table$M_S[i+1]/comparison_life_table$M_S[i]  
# }

names(comparison_life_table) <- c(names(comparison_life_table)[1:6],"historical_female_NS_qx", "historical_male_NS_qx",
                                  "historical_female_S_qx", "historical_male_S_qx")

comparison_life_table_temp <- comparison_life_table[c("Age", "historical_female_NS_qx", "historical_male_NS_qx",
                                                       "historical_female_S_qx", "historical_male_S_qx")]

comparison_life_table_clean <- merge(mortality_table, comparison_life_table_temp, by.x = "Age")

inforce_aggregated
# not too bad. slightly overestimates actual rates for ages < 65
empirical_F_NS <- inforce_aggregated %>% filter(Smoker.Status == "NS", Sex == "F") %>% group_by(curr_age) %>% dplyr::summarize(deaths = sum(Death.indicator), rate2 = mean(Death.indicator))
comparison_all <- merge(comparison_life_table_clean, empirical_F_NS, by.x = "x", by.y = "curr_age")
ggplot(comparison_all) + geom_point(aes(x = x, y = historical_female_NS_qx)) + geom_point(aes(x = x, y = rate2), col="red") +
  geom_point(aes(x = x, y = female_NS_qx), col="blue") + ylab("qx") + ggtitle("Mortality Rates for Female Non-Smokers")

# underestimates actual rates for ages < 50
empirical_M_NS <- inforce_aggregated %>% filter(Smoker.Status == "NS", Sex == "M") %>% group_by(curr_age) %>% dplyr::summarize(deaths = sum(Death.indicator), rate2 = mean(Death.indicator))
comparison_all <- merge(comparison_life_table_clean, empirical_M_NS, by.x = "x", by.y = "curr_age")
ggplot(comparison_all) + geom_point(aes(x = x, y = historical_male_NS_qx)) + geom_point(aes(x = x, y = rate2), col="red") +
  geom_point(aes(x = x, y = male_NS_qx), col="blue")

# Note: life table approach significantly overestimates mortality rates for ages > 55

# underestimates actual rates for ages > 60
empirical_F_S <- inforce_aggregated %>% filter(Smoker.Status == "S", Sex == "F") %>% group_by(curr_age) %>% dplyr::summarize(deaths = sum(Death.indicator), rate2 = mean(Death.indicator))
comparison_all <- merge(comparison_life_table_clean, empirical_F_S, by.x = "x", by.y = "curr_age")
ggplot(comparison_all) + geom_point(aes(x = x, y = historical_female_S_qx)) + geom_point(aes(x = x, y = rate2), col="red") +
  geom_point(aes(x = x, y = female_NS_qx), col="blue")

# overestimates actual rates for ages < 65
empirical_M_S <- inforce_aggregated %>% filter(Smoker.Status == "S", Sex == "M") %>% group_by(curr_age) %>% dplyr::summarize(deaths = sum(Death.indicator), rate2 = mean(Death.indicator))
comparison_all <- merge(comparison_life_table_clean, empirical_M_S, by.x = "x", by.y = "curr_age")
ggplot(comparison_all) + geom_point(aes(x = x, y = historical_male_S_qx)) + geom_point(aes(x = x, y = rate2), col="red") +
  geom_point(aes(x = x, y = female_NS_qx), col="blue")


# Try adjustments
comparison_all <- merge(comparison_life_table_clean, empirical_F_NS, by.x = "x", by.y = "curr_age")
comparison_all$female_NS_qx_adj <- hazard_ratio_qx(comparison_all$female_NS_qx, 0.75)
ggplot(comparison_all) + geom_point(aes(x = x, y = historical_female_NS_qx)) + geom_point(aes(x = x, y = rate2), col="red") +
  geom_point(aes(x = x, y = female_NS_qx_adj), col="darkgreen") + ylab("qx") + ggtitle("Mortality Rates for Female Non-Smokers")

comparison_all <- merge(comparison_life_table_clean, empirical_M_NS, by.x = "x", by.y = "curr_age")
comparison_all$male_NS_qx_adj <- hazard_ratio_qx(comparison_all$male_NS_qx, 0.8)
ggplot(comparison_all) + geom_point(aes(x = x, y = historical_male_NS_qx)) + geom_point(aes(x = x, y = rate2), col="red") +
  geom_point(aes(x = x, y = male_NS_qx_adj), col="darkgreen") + ylab("qx") + ggtitle("Mortality Rates for Male Non-Smokers")

comparison_all <- merge(comparison_life_table_clean, empirical_F_S, by.x = "x", by.y = "curr_age")
comparison_all$female_S_qx_adj <- hazard_ratio_qx(comparison_all$female_S_qx, 0.8)
ggplot(comparison_all) + geom_point(aes(x = x, y = historical_female_S_qx)) + geom_point(aes(x = x, y = rate2), col="red") +
  geom_point(aes(x = x, y = female_S_qx_adj), col="darkgreen") + ylab("qx") + ggtitle("Mortality Rates for Female Smokers")


write.csv(comparison_life_table_clean, "comparison_life_table_clean.csv")
write.csv(new_life_probs, "future_qx.csv")



# Merge with all
comparison_life_table_temp2 <- comparison_life_table[c("Age", "historical_female_NS_qx", "historical_male_NS_qx",
                                                      "historical_female_S_qx", "historical_male_S_qx")]
comparison_life_table_clean2 <- merge(mortality_table, comparison_life_table_temp2, by = "Age", all = TRUE)

for(i in 80:119){
  comparison_life_table_clean2$historical_female_NS_qx[i] <- comparison_life_table_clean2$historical_female_NS_qx[i-1] * comparison_life_table_clean2$female_NS_qx[i] / comparison_life_table_clean2$female_NS_qx[i-1]
  comparison_life_table_clean2$historical_male_NS_qx[i] <- comparison_life_table_clean2$historical_male_NS_qx[i-1] * comparison_life_table_clean2$male_NS_qx[i] / comparison_life_table_clean2$male_NS_qx[i-1]
  comparison_life_table_clean2$historical_female_S_qx[i] <- comparison_life_table_clean2$historical_female_S_qx[i-1] * comparison_life_table_clean2$female_S_qx[i] / comparison_life_table_clean2$female_S_qx[i-1]
  comparison_life_table_clean2$historical_male_S_qx[i] <- comparison_life_table_clean2$historical_male_S_qx[i-1] * comparison_life_table_clean2$male_S_qx[i] / comparison_life_table_clean2$male_S_qx[i-1]
}

ggplot(comparison_life_table_clean2) + geom_point(aes(x=x, y=female_NS_qx)) + 
  geom_point(aes(x=x, y=historical_female_NS_qx), col="red")

ggplot(comparison_life_table_clean2) + geom_point(aes(x=x, y=male_NS_qx)) + 
  geom_point(aes(x=x, y=historical_male_NS_qx), col="red")


comparison_life_table_clean2[is.na(comparison_life_table_clean2)]<- 0

weightings <- function(w){
  data.frame("Age" = comparison_life_table_clean2$Age,
    "weighted_female_NS_qx" = comparison_life_table_clean2$historical_female_NS_qx * (1-w) + comparison_life_table_clean2$F_NS.qx* w,
             "weighted_male_NS_qx" = comparison_life_table_clean2$historical_male_NS_qx * (1-w) + comparison_life_table_clean2$M_NS.qx * w,
             "weighted_female_S_qx" = comparison_life_table_clean2$historical_female_S_qx * (1-w) + comparison_life_table_clean2$F_S.qx * w,
             "weighted_male_S_qx" = comparison_life_table_clean2$historical_male_S_qx * (1-w) + comparison_life_table_clean2$M_S.qx * w)
}

weightings_list <- list(weightings(0.5), weightings(0.6), weightings(0.7), weightings(0.8), weightings(0.9),
                        weightings(c(rep(0.7, 65), (70 + 1:14 * 2)/100, rep(1, 41))))
fwrite(weightings_list[[6]], "~/ACTL4001/Initialised_Data/weighted_qx.csv")

weighted_df <- weightings_list[[6]]
ggplot(weighted_df) + geom_point(aes(x=Age, y=weighted_male_NS_qx)) + 
  geom_point(aes(x=Age, y=weighted_male_NS_qx), col="red") + 
  geom_point(aes(x=Age, y=weighted_female_NS_qx), col="blue") +  
  geom_point(aes(x=Age, y=weighted_male_S_qx), col="green") +  
  geom_point(aes(x=Age, y=weighted_female_S_qx), col="orange")

ggplot(data = comparison_life_table[1:58,]) + geom_point(aes(x = Age, y=F_NS)) +
  geom_point(aes(x = Age, y= F_NS.qx), col = 'red') + xlab("Age") + ylab("lx") +
  geom_point(aes(x= weighted_df$Age[27:84], y = weighted_df$weighted_female_NS_qx[27:84]), col = 'blue')
ggplot(data = comparison_life_table[1:58,]) + geom_point(aes(x = Age, y=M_NS)) +
  geom_point(aes(x = Age, y = M_NS.qx, col= 'red')) +
  geom_point(aes(x= weighted_df$Age[27:84], y = weighted_df$weighted_male_NS_qx[27:84]), col = 'blue')

ggplot(data = comparison_life_table[1:58,]) + geom_point(aes(x = Age, y=F_S)) +
  geom_point(aes(x = Age, y=F_S.qx), col = 'red') + xlab("Age") + ylab("lx") +
  geom_point(aes(x= weighted_df$Age[27:84], y = weighted_df$weighted_female_S_qx[27:84]), col = 'blue')

ggplot(data = comparison_life_table[1:58,]) + geom_point(aes(x = Age, y=M_S)) +
  geom_point(aes(x = Age, y=M_S.qx), col= 'red') +
  geom_point(aes(x= weighted_df$Age[27:84], y = weighted_df$weighted_male_S_qx[27:84]), col = 'blue')



temp_df <- merge(empirical_F_NS, weighted_df, by.x="curr_age", by.y="x",)
ggplot(temp_df) + geom_point(aes(x = curr_age, y = weighted_female_NS_qx)) + geom_point(aes(x = curr_age, y = rate2), col="red") + 
  ylab("qx") + ggtitle("Mortality Rates for Female Non-Smokers")

temp_df <- merge(empirical_M_NS, weighted_df, by.x="curr_age", by.y="x")
ggplot(temp_df) + geom_point(aes(x = curr_age, y = weighted_male_NS_qx)) + geom_point(aes(x = curr_age, y = rate2), col="red") + 
  ylab("qx") + ggtitle("Mortality Rates for Male Non-Smokers")

temp_df <- merge(empirical_F_S, weighted_df, by.x="curr_age", by.y="x")
ggplot(temp_df) + geom_point(aes(x = curr_age, y = weighted_female_S_qx)) + geom_point(aes(x = curr_age, y = rate2), col="red") + 
  ylab("qx") + ggtitle("Mortality Rates for Female Smokers")

temp_df <- merge(empirical_M_S, weighted_df, by.x="curr_age", by.y="x")
ggplot(temp_df) + geom_point(aes(x = curr_age, y = weighted_male_S_qx)) + geom_point(aes(x = curr_age, y = rate2), col="red") + 
  ylab("qx") + ggtitle("Mortality Rates for Male Smokers")


#---------------------- Covid Study

pop_raw = read.csv("~/ACTL4001/Case Data/2024-srcsc-superlife-inforce-dataset.csv")
colnames(pop_raw)[1] = "Policy.Number"

mortality_table = lumaria_qx
colnames(mortality_table)[1] = "Age"

inforce_current <- pop_raw
inforce_current$Year.of.Death[is.na(inforce_current$Year.of.Death)] <- 2024
inforce_current$Year.of.Lapse[is.na(inforce_current$Year.of.Lapse)] <- 2024


inforce_current <- inforce_current %>%
  filter(is.na(Year.of.Death) == FALSE) %>%
  mutate(time = case_when(Year.of.Death < Year.of.Lapse ~ Year.of.Death,
                          Year.of.Lapse < Year.of.Death ~ Year.of.Lapse,
                          .default = 2023) + 0.5,
         censored_death = case_when(Year.of.Death < Year.of.Lapse ~ 1,
                                    Year.of.Lapse < Year.of.Death ~ 0,
                                    .default = 0),
         covid = case_when(Year.of.Death >2018 ~ 1,
                           .default = 0),
         age_length = Issue.age + time - Issue.year,
         gender = factor(Sex, levels = c("M","F")),
         smoking = factor(Smoker.Status, levels = c("NS", "S")),
         distribution = factor(Distribution.Channel, levels = unique(inforce_current$Distribution.Channel)),
         region = factor(Region, levels = unique(inforce_current$Region)),
         Policy = factor(Policy.type, levels = unique(inforce_current$Policy.type)),
         covid = factor(covid))


hazards <- coxph(Surv(Issue.age, age_length, censored_death) ~ covid, data = inforce_current)

# About a 50% Increase to mortalty during covid periods

library(survminer)
ggforest(hazards)

#-------

pop_raw = read.csv("~/ACTL4001/Case Data/2024-srcsc-superlife-inforce-dataset.csv")
colnames(pop_raw)[1] = "Policy.Number"

mortality_table = lumaria_qx
colnames(mortality_table)[1] = "Age"

inforce_current <- pop_raw
inforce_current$Year.of.Death[is.na(inforce_current$Year.of.Death)] <- 2023
inforce_current$Year.of.Lapse[is.na(inforce_current$Year.of.Lapse)] <- 2023


inforce_current <- inforce_current %>%
  filter(is.na(Year.of.Lapse) == FALSE) %>%
  mutate(time = case_when(Year.of.Death < Year.of.Lapse ~ Year.of.Death,
                          Year.of.Lapse < Year.of.Death ~ Year.of.Lapse,
                          .default = 2023) + 0.5,
         censored_death = case_when(Year.of.Lapse < Year.of.Death ~ 1,
                                    Year.of.Death < Year.of.Lapse ~ 0,
                                    .default = 0),
         covid = case_when(Year.of.Lapse >2018 ~ 1,
                           .default = 0),
         age_length = Issue.age + time - Issue.year,
         gender = factor(Sex, levels = c("M","F")),
         smoking = factor(Smoker.Status, levels = c("NS", "S")),
         distribution = factor(Distribution.Channel, levels = unique(inforce_current$Distribution.Channel)),
         region = factor(Region, levels = unique(inforce_current$Region)),
         Policy = factor(Policy.type, levels = unique(inforce_current$Policy.type)),
         covid = factor(covid))


hazards <- coxph(Surv(Issue.age, age_length, censored_death) ~ covid, data = inforce_current)
hazards
# About a 50% Increase to mortalty during covid periods

ggforest(hazards)




install.packages('survminer')





# ================= OLD CODE ========================================================
# Condensed inforce dataset with only critical variables
inforce_condensed <- inforce_clean[c("Policy.type", "curr_age", "Sex", "Smoker.Status", "birth_year", "Face.amount", "year_of_policy", "Year.of.Death")]

projection_list <- list(inforce_condensed, inforce_condensed, inforce_condensed)
projection_yrs <- length(projection_list)


# Store values across years
for(i in 1:projection_yrs){
  projection_list[[i]]$curr_age <- projection_list[[i]]$curr_age + i
  projection_list[[i]]$year_of_policy <- projection_list[[i]]$year_of_policy + i
  projection_list[[i]] <- merge(projection_list[[i]], lumaria_qx, 
                                by.x=c("curr_age", "Sex", "Smoker.Status"), 
                                by.y=c("Age", "Sex", "Smoker.Status"))
  
}


for(i in 1:projection_yrs){
  results <- runif(n, 0, 1)
  stillAlive <- rep(TRUE, n)
  dyingThisYear <- rep(FALSE, n)
  
  if(i > 1){
    projection_list[[i]]$state <- projection_list[[i-1]]$state
    projection_list[[i]]$Year.of.Death <- projection_list[[i-1]]$Year.of.Death
    projection_list[[i]]$state <- rep("Survival", n)
    stillAlive <- projection_list[[i-1]]$state == "Survival"
  }
  
  dyingThisYear <- results < projection_list[[i]]$Mortality.Rate
  projection_list[[i]]$state[!stillAlive] <- "Dead"
  projection_list[[i]]$state[dyingThisYear & stillAlive] <- "Dead"
  projection_list[[i]]$state[!dyingThisYear & stillAlive] <- "Survival"
  projection_list[[i]]$Year.of.Death[dyingThisYear & stillAlive] <- 2023 + i
}

projected_sums <- c()
for(i in 1:projection_yrs){
  projected_sums[i] <- sum(projection_list[[i]]$Face.amount[projection_list[[i]]$Year.of.Death == (2023 + i)], na.rm = T)
}

# Include interest rates model 
combined_rates <- read.csv("combined_rates.csv")

discounted_sums <- c()
for(i in 1:projection_yrs){
  discounted_sums[i] <- projected_sums[i] * prod(1/(1 + combined_rates$Mean_RF1[1:i]))
}

# Total cost over projection period
sum(discounted_sums)


# data check and random testing
projection_list[[3]] %>% arrange(desc(Year.of.Death))
sum(projection_list[[3]]$state == "Dead")

test <- inforce_condensed %>% filter(Policy.type == "T20") %>% group_by(curr_age, year_of_policy) %>% summarise(pcount = n())
ggplot(test) + geom_tile(aes(x = curr_age, y = year_of_policy, fill = pcount))


test <- inforce_condensed %>% filter(Policy.type == "SPWL") %>% group_by(curr_age, year_of_policy) %>% summarise(pcount = n())
ggplot(test) + geom_tile(aes(x = curr_age, y = year_of_policy, fill = pcount))

# Adding decrement columns for lapse and death
inforce_clean <- merge(inforce_clean, lumaria_qx, by.x = "curr_age", by.y = "Age")
names(inforce_clean)[names(inforce_clean) == 'Mortality.Rate'] <- 'base_qx'
inforce_clean$force.m <- -log(1 - inforce_clean$base_qx)
inforce_clean$new_force.m <- inforce_clean$force.m * 0.95  # dummy value
inforce_clean$new_qx <- 1 - exp(-inforce_clean$new_force.m)
inforce_clean$wx <- 0.02  # dummy value


# Simulation of the result of that policy for that year
results <- runif(n, 0, 1)
inforce_clean$state <- rep("Survival", n)
inforce_clean$state[results < inforce_clean$new_qx] <- "Death"
inforce_clean$state[results > 1 - inforce_clean$wx] <- "Lapse"


# reshaping test
# IGNORE THIS NOW
# all dummy vals. Later on we can figure out how to appropriately model the assumptions for gender, smoking status and mortality improvements.
lumaria_qx <- read.csv("Case Data/srcsc-2024-lumaria-mortality-table-CLEANED.csv", header = TRUE)
lumaria_qx$Smoker.Mortality.Rate <- lumaria_qx$Mortality.Rate * 1.5
lumaria_qx <- pivot_longer(lumaria_qx, cols = c('Mortality.Rate', 'Smoker.Mortality.Rate'), 
                           names_to = 'Smoker.Status',
                           values_to = 'Mortality.Rate')
lumaria_qx$Smoker.Status[lumaria_qx$Smoker.Status == "Mortality.Rate"] <- "NS"
lumaria_qx$Smoker.Status[lumaria_qx$Smoker.Status == "Smoker.Mortality.Rate"] <- "S"

lumaria_qx$Female.Mortality.Rate <- lumaria_qx$Mortality.Rate * 0.97
lumaria_qx$Male.Mortality.Rate <- lumaria_qx$Mortality.Rate * 1.03
lumaria_qx$Mortality.Rate <- NULL
lumaria_qx <- pivot_longer(lumaria_qx, cols = c('Male.Mortality.Rate', 'Female.Mortality.Rate'), 
                           names_to = 'Sex',
                           values_to = 'Mortality.Rate')
lumaria_qx$Sex[lumaria_qx$Sex == "Male.Mortality.Rate"] <- "M"
lumaria_qx$Sex[lumaria_qx$Sex == "Female.Mortality.Rate"] <- "F"

lumaria_qx$Adj.Mortality.Rate <- lumaria_qx$Mortality.Rate * 0.9


# failed lapse stuff

splits_T20 <- inforce_clean %>% filter(Policy.type == "T20") %>% 
  group_by(curr_age, year_of_policy, Issue.year) %>% summarize(count = n())

splits_T20$deaths <- 0
splits_T20$lapses <- 0
splits_T20$exposure <- 0
splits_T20$death_rate <- 0
splits_T20$lapse_rate <- 0

for(i in 1:nrow(splits_T20)){
  this_row <- splits_T20[i,]
  
  splits_T20$exposure[i] <- sum(inforce_clean$Issue.year == this_row$Issue.year & 
                                  inforce_clean$curr_age - this_row$curr_age == inforce_clean$year_of_policy - this_row$year_of_policy)
  
  splits_T20$deaths[i] <- sum(inforce_clean$Death.Indicator == 1 & 
                                inforce_clean$curr_age == this_row$curr_age & 
                                inforce_clean$year_of_policy == this_row$year_of_policy & 
                                inforce_clean$Issue.year == this_row$Issue.year)
  splits_T20$death_rate[i] <- splits_T20$lapses[i]/splits_T20$count[i]
  
  splits_T20$lapses[i] <- sum(inforce_clean$Lapse.Indicator == 1 & 
                                inforce_clean$curr_age == this_row$curr_age & 
                                inforce_clean$year_of_policy == this_row$year_of_policy & 
                                inforce_clean$Issue.year == this_row$Issue.year)
  splits_T20$lapse_rate[i] <- splits_T20$lapses[i]/splits_T20$count[i]
}

ggplot(splits_T20) + geom_tile(aes(x = curr_age, y = year_of_policy, fill = lapse_rate))

splits_T20 %>% filter(curr_age == 60)


