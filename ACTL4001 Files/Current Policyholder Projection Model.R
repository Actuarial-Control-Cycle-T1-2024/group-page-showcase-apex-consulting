# --- Building for Current Policyholders

# ---- Variables ----

COHORTS <- 23
MAX_AGE <- 100
PROJECTION_TIME <- 20
SMOKING_TYPES <- 2
GENDER <- 2
POLICY_TYPE <- 2
WITHDRAWN <- 2

SPWL <- 1
T20 <- 2

MALE <- 1
FEMALE <- 2

NOT_LAPSED <- 1
LAPSED <- 2

SMOKER <- 2
NON_SMOKER <- 1

TIME_LENGTH <- 30

SUM_ASSURED <- 6


# ---- Data / Package Read In

library("ggplot2")
library("dplyr")
setwd("~//")
inforce_raw <- read.csv("2024-srcsc-superlife-inforce-dataset2.csv")

# ---- Main Code ----

# Array Parameters
# Group by certain cohorts for projections
#   1. Age, Smoking Status, Gender, Policy Type, Withdrawn/Surrendered
#   This should be a 120 x 30 x 2 x 2 x 2 matrix to account for this categories.
#
# - Dimension 1 - Cohort Year
# - Dimension 2 - Age
# - Dimension 3- Year of Projection (time 0 - 30)
# - Dimension 4 - Gender
# - Dimension 5 - Smoking Types (number )
# - Dimension 5 - Policy Type
# - Dimension 6 - Sum Assured

#   For reference, at cell [50,2,1,1,1] <-
#   Represents Cohort aged 50, in Year 2, non-smoker, SPWL, Not Withdrawn

# Cohort dates

Cohort_Years <- length(c(2001:(2023)))

# Build the default empty matrix structure

Policyholders_Current <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                             POLICY_TYPE, SUM_ASSURED))

Policyholders_Deaths <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                            POLICY_TYPE, SUM_ASSURED))

Policyholders_Lapse <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                            POLICY_TYPE, SUM_ASSURED))

Policyholders_Joining <- array(c(0), dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                             POLICY_TYPE, SUM_ASSURED))

# Set this order for the loop below: Projection Time, Age, Gender, Smoking, Policy, Cohorts, Sum Assured

Policyholders_Mortality <- array(c(0), dim = c(PROJECTION_TIME + 1, MAX_AGE, GENDER, SMOKING_TYPES,
                                               POLICY_TYPE, COHORTS, SUM_ASSURED))

Policyholders_Withdrawl <- array(c(0), dim = c(PROJECTION_TIME + 1, MAX_AGE, GENDER, SMOKING_TYPES,
                                               POLICY_TYPE, COHORTS, SUM_ASSURED))

# ---- Initialise Current Cohorts ----

# Initialise the matrix of number of mortalities at each time point

Parameter_Dimensions <- expand.grid(cohort = 1:COHORTS,
                                    age = 1:MAX_AGE,
                                    year = 1:PROJECTION_TIME,
                                    gender = 1:GENDER,
                                    smoking = 1:SMOKING_TYPES,
                                    policy_type = 1:POLICY_TYPE,
                                    sum_assured = 1:SUM_ASSURED)

inforce_current <- inforce_raw %>%
  mutate(age = Issue.age + Year.of.Death - Issue.year,
         grouping = paste(Issue.year, age, Year.of.Death, Sex, Smoker.Status, Policy.type,
                          Face.amount)) %>%
  filter(Year.of.Death > 2003) %>%
  group_by(grouping) %>%
  summarise(number = n(),
            age = unique(age))


Gender <- c("M", "F")
Smoke <- c("NS", "S")
Policy <- c("SPWL", "T20")
Sum_Assured <- unique(inforce_raw$Face.amount)

#------ Sort Death Timings

# Determine number of deaths in each cohort period

Parameter_Dimensions <- Parameter_Dimensions %>%
  mutate(grouping= paste(cohort +2000,age,year+2003, Gender[gender], Smoke[smoking], Policy[policy_type],
                         Sum_Assured[sum_assured]))

merge_data <- merge(Parameter_Dimensions, inforce_current, by = "grouping", all.x = TRUE)

# Reorder the merged data to match the order of Parameter_Dimensions

merge_data <- merge_data %>%
  mutate(order = match(grouping, Parameter_Dimensions$grouping)) %>%
  arrange(order)

merge_data$number[is.na(merge_data$number)] <- 0


Policyholders_Deaths[,,2:21,,,,] <- merge_data$number

# Check the total death counts are equal

sum(Policyholders_Deaths) == sum(inforce_raw$Year.of.Death > 2003, na.rm = T)

#----- Sort Withdrawl Timings ----

sum(Policyholders_Lapse) == sum(as.numeric(inforce_raw$Lapse.Indicator), na.rm = T)
# Initialise the number of withdrawls at each time point

inforce_current <- inforce_raw %>%
  mutate(age = Issue.age + Year.of.Lapse - Issue.year,
         grouping = paste(Issue.year, age, Year.of.Lapse, Sex, Smoker.Status, Policy.type,
                          Face.amount)) %>%
  filter(Year.of.Lapse > 2003) %>%
  group_by(grouping) %>%
  summarise(number = n(),
            age = unique(age))


Parameter_Dimensions <- Parameter_Dimensions %>%
  mutate(grouping= paste(cohort +2000,age,year+2003, Gender[gender], Smoke[smoking], Policy[policy_type],
                         Sum_Assured[sum_assured]))

merge_data <- merge(Parameter_Dimensions, inforce_current, by = "grouping", all.x = TRUE)

# Reorder the merged data to match the order of Parameter_Dimensions

merge_data <- merge_data %>%
  mutate(order = match(grouping, Parameter_Dimensions$grouping)) %>%
  arrange(order)

merge_data$number[is.na(merge_data$number)] <- 0

Policyholders_Lapse[,,2:21,,,,] <- merge_data$number

sum(Policyholders_Lapse) == sum(inforce_raw$Year.of.Lapse > 2003, na.rm = T)

#-------------- Sort Current Policyholders ----

# Get a starting number of policyholders in each cohort
# This means we can incrementally add them in each year of projection and apply decrements.

Parameter_Dimensions <- expand.grid(cohort = 1:COHORTS,
                                    age = 1:MAX_AGE,
                                    gender = 1:GENDER,
                                    smoking = 1:SMOKING_TYPES,
                                    policy_type = 1:POLICY_TYPE,
                                    sum_assured = 1:SUM_ASSURED)



inforce_current <- inforce_raw

inforce_current$Year.of.Death[is.na(inforce_current$Year.of.Death)] <- 10000
inforce_current$Year.of.Lapse[is.na(inforce_current$Year.of.Lapse)] <- 10000  
  
inforce_current <- inforce_current %>%
  mutate(grouping = paste(Issue.year, Issue.age, Sex, Smoker.Status, Policy.type,
                          Face.amount)) %>%
  filter(Year.of.Death > 2003, Year.of.Lapse > 2003 ) %>%
  group_by(grouping) %>%
  summarise(number = n())

Parameter_Dimensions <- Parameter_Dimensions %>%
  mutate(grouping = paste(cohort +2000,age, Gender[gender], Smoke[smoking], Policy[policy_type],
                         Sum_Assured[sum_assured]))

merge_data <- merge(Parameter_Dimensions, inforce_current, by = "grouping", all.x = TRUE)

# Reorder the merged data to match the order of Parameter_Dimensions

merge_data <- merge_data %>%
  mutate(order = match(grouping, Parameter_Dimensions$grouping)) %>%
  arrange(order)

merge_data$number[is.na(merge_data$number)] <- 0

Policyholders_Joining[,,,,,] <- merge_data$number


# 
# Face_Values <- array(Sum_Assured, dim = c(SUM_ASSURED, COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES, 
#                                           POLICY_TYPE))
# 
# Results <- (Policyholders_Deaths) * aperm(Face_Values, c(2,3,4,5,6,7,1))
# 
# year_of_projection <- apply(Results, MARGIN = c(3), FUN = sum)
# year_of_projection_deaths<- apply(Policyholders_Deaths, MARGIN = c(3), FUN = sum)
# 
# 
# plot(c(0:20)+2003, year_of_projection/10^6 ,main = "Unindexed Cost per projection time",
#      xlab = "Year", ylab = "Total Benefits ($M)")
# 
# plot(c(0:20)+2003, year_of_projection_deaths ,main = "Deaths per projection time",
#      xlab = "Year", ylab = "Total Deaths")
# 
# y <- inforce_raw %>%
#   group_by(Year.of.Death) %>%
#   summarise(number = n())
# 
# lines(c(0:20)+2003, y$number[3:23])
# 
#------- Main Loop Code ----

# Initialise the current participants (at 2003)

Policyholders_Current[1,3:MAX_AGE,1,,,,] <- Policyholders_Joining[1,1:(MAX_AGE-2),,,,]  
Policyholders_Current[2,2:MAX_AGE,1,,,,] <- Policyholders_Joining[1,1:(MAX_AGE-1),,,,]  
Policyholders_Current[3,,1,,,,] <- Policyholders_Joining[1,,,,,]

# Withdrawing assumptions for different ages, gender etc. for term insurance

withdraw_matrix_IND <- array(c(0.01), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, COHORTS, SUM_ASSURED, PROJECTION_TIME + 1))

mortality_matrix_IND <- array(Mortality_Rates, dim = c(COHORTS, MAX_AGE,PROJECTION_TIME + 1, GENDER, SMOKING_TYPES, 
                                                       POLICY_TYPE, SUM_ASSURED))

mortality_matrix_IND <- aperm(mortality_matrix_IND, c(2,4,5,6,1,7,3) )

# Make the rates dependent under the UDD assumption

mortality_matrix <- array(c(0), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, POLICY_TYPE, COHORTS, SUM_ASSURED, PROJECTION_TIME +1))

withdraw_matrix <- withdraw_matrix_IND * (1 - 0.5 * mortality_matrix_IND[,,,T20,,,])

mortality_matrix[,,,T20,,,] <- mortality_matrix_IND[,,,T20,,,] * (1 - 0.5 * withdraw_matrix_IND)

mortality_matrix[,,,SPWL,,,] <- mortality_matrix_IND[,,,SPWL,,,]


for (j in 2: (PROJECTION_TIME + 1)) {
  
  # Start by moving policy holders down their respective age for new projection
  # Move the first 1:j+21 cohorts (i.e at 2024, this would represent moving up to 2023 down)
  
  Policyholders_Current[1:(j+1),2:MAX_AGE,j,,,, ] <- Policyholders_Current[1:(j+1),1:(MAX_AGE-1),j-1,,,, ]  
  
  # Build the new cohort for example (lets just put a dummy of 1 person per cohort)
  
  Policyholders_Current[j+2,,j,,,, ] <- Policyholders_Joining[j+2,,,,,]
  
  # ---- Apply Smoking Transitions Initially ----
  
  # Turn the lifes alive matrix into format
  # Projection Time, Smoking Status, Ages, gender, Policy Type, Cohorts, Sum Assured
  
  Policyholders_Current <- aperm(Policyholders_Current, c(3,5,2,4,6,1,7))
  
  # Build a smoking transition assumption matrix between each category
  
  transition_matrix <- array(c(0.0001, 0.02), dim = c(SMOKING_TYPES, MAX_AGE, GENDER, POLICY_TYPE, COHORTS, SUM_ASSURED))
  
  # Take the current makeup of population to account for transitions into smoking etc.
  
  Current_makeup <- Policyholders_Current[j,,,,,,]
  
  # Adjust the non-smoker population (Equals number of people staying in non-smoking + Number of smokers becoming non-smokers)
  
  Policyholders_Current[j,NON_SMOKER,,,,, ] <- Current_makeup[NON_SMOKER,,,,,] * (1-transition_matrix[NON_SMOKER,,,,,]) +
    Current_makeup[SMOKER,,,,,] * transition_matrix[SMOKER,,,,,]
  
  # Adjust the smoker population (Equals number of people staying in smoking + Number of non-smokers becoming smokers)
  
  
  Policyholders_Current[j,SMOKER,,,,, ] <- Current_makeup[NON_SMOKER,,,,,] * transition_matrix[NON_SMOKER,,,,,] +
    Current_makeup[SMOKER,,,,,] * (1-transition_matrix[SMOKER,,,,,])
  
  # Turn the lifes alive matrix into format (number represents previous dimension)
  # Cohorts (6), Ages (3), Projection Time(1), gender(4), smoking status(2), policy type(5), Sum Assured(7)
  
  Policyholders_Current <- aperm(Policyholders_Current, c(6,3,1,4,2,5,7))
  
  # ---- Apply Mortality ----
  
  # Rearrange matrix (Projection Time, Age, Gender, Smoking, Policy, Cohorts, Sum Assured)
  
  Policyholders_Current <- aperm(Policyholders_Current, c(3,2,4,5,6,1,7))
  
  # Apply mortality assumptions (i.e. seperate assumptions for age, gender, smoking and policy)
  
  Policyholders_Mortality[j,,,,,,] <- mortality_matrix[,,,,,,j] * Policyholders_Current[j,,,,,,]
  
  Policyholders_Current[j,,,,,,] <- (1- mortality_matrix[,,,,,,j])* Policyholders_Current[j,,,,,,]
  
  # ---- Apply Withdraw ----
  
  # Keep track of participants who are withdrawn
  
  Policyholders_Withdrawl[j,,,,T20,,] <- withdraw_matrix[,,,,,j]* Policyholders_Current[j,,,,T20,,]
  
  # Remove Participants who are withdrawn
  
  Policyholders_Current[j,,,,T20,,] <- (1-withdraw_matrix[,,,,,j])* Policyholders_Current[j,,,,T20,,]
  
  
  # --- Apply Maturity Removal ----
  
  # In here we would need to loop through cohorts i.e. if the year of projection - cohort
  
  # i.e born in 2024, year of projection 21, 23 - 24 + 21 = 20
  
  # Put all the policies in the withdraw status as required
  
  if(j +2003 - 2020 > 0) {
    
    Policyholders_Withdrawl[j,,,,T20,j+2003-2020,] <- Policyholders_Current[j,,1,1,T20,j+2003-2020,]
  
    # Set lives in policy to 0 (at end fo the term)
    
    Policyholders_Current[j,,,,T20,j+2003-2020, ] <- 0
    
  }
  
  # Rearrange matrix back to standard form:
  # Cohorts (6), Age (2), Projection Time (1), Gender(3), Smoking (4), Policy (5), Withdrawn (7)
  #(Projection Time, Age, Gender, Smoking, Policy, Cohorts, Sum Assured)
  
  Policyholders_Current <- aperm(Policyholders_Current, c(6,2,1,3,4,5,7))
  
}


# Check the point at which they all withdrawl
# In this case for 2002 cohort should all surrender in year 2021
Mortality_Rates[2,,,1,2]
y <- Mortality_Rates[2,,,1,2]

y <- as.data.frame(y)

Save_1 <- Policyholders_Lapse

Save_2 <- aperm(Policyholders_Mortality, c(6,2,1,3,4,5,7))

Policyholders_Current[1,,,,,,]
y <- mortality_matrix[,1,1,T20,1,1,]
sum(Policyholders_Lapse)
sum(Policyholders_Withdrawl)
y <- Policyholders_Current[1,,,,,T20,]
y <- sum(Policyholders_Deaths[1,,,,,,])
sum(Policyholders_Mortality[,,,,,1,])
y <- as.data.frame(y)
Policyholders_Mortality[1,,,,,,]
Policyholders_Deaths

sum(Save_2[1:18,,,,,,] - Save_1[1:18,,,,,,])

Face_Values <- array(Sum_Assured, dim = c(SUM_ASSURED, COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                          POLICY_TYPE))

