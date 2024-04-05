# ---- Original Model ----

# ---- Variables ----

COHORTS <- 53
MAX_AGE <- 120
PROJECTION_TIME <- 30
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
# - Dimension 6 - Withdrawn Status

#   For reference, at cell [50,2,1,1,1] <- 
#   Represents Cohort aged 50, in Year 2, non-smoker, SPWL, Not Withdrawn 

# Cohort dates

Cohort_Years <- length(c(2001:(2023+30)))

# Build the default empty matrix structure

Policyholders_Future <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES, 
                                                   POLICY_TYPE, SUM_ASSURED))


# ---- Initialise Current Cohorts ----

# First Initialise Current People in policies

inforce_current <- inforce_raw[is.na(inforce_raw$Year.of.Lapse) == T & is.na(inforce_raw$Year.of.Death) == T,]


# Need to group each data point into their individual assigned group (with no withdrawls at t = 0)

inforce_current <- inforce_current %>%
  mutate(grouping = paste(Issue.year, Issue.age, Sex, Smoker.Status, Policy.type, Lapse.Indicator)) %>%
  group_by(grouping) %>%
  summarise(number = n(),
            cohort = unique(Issue.year),
            Starting_Age = unique(Issue.age + 2024 - Issue.year),
            gender  = unique(Sex),
            smoking = unique(Smoker.Status),
            policy = unique(Policy.type))

Gender <- c("M", "F")
Smoke <- c("NS", "S")
Policy <- c("SPWL", "T20")

for (i in 1:23) {
  
  for(j in 1:MAX_AGE) {
    
    for(k in 1:GENDER) {
      
      for(l in 1:SMOKING_TYPES) {
        
        for(m in 1:POLICY_TYPE) {
          
          Policyholders_Future[i,j,1,k,l,m,1] <- sum(inforce_current[inforce_current$grouping == 
                                                            paste(i+2000,
                                                                  j,
                                                                  Gender[k],
                                                                  Smoke[l],
                                                                  Policy[m], NA),2])
          
          
          
        }
      }
    }
  }
}


for (j in 2: (PROJECTION_TIME + 1)) {
    
    # Start by moving policy holders down their respective age for new projection
    # Move the first 1:j+21 cohorts (i.e at 2024, this would represent moving up to 2023 down)
  
    Policyholders_Future[1:(j+21),2:MAX_AGE,j,,,,NOT_LAPSED] <- Policyholders_Future[1:(j+21),1:(MAX_AGE-1),j-1,,,,NOT_LAPSED]  
  
    # Build the new cohort for example (lets just put a dummy of 1 person per cohort)
    
    Policyholders_Future[j+22,,j,,,,NOT_LAPSED] <- array(c(1), dim = c(MAX_AGE, GENDER, SMOKING_TYPES)) 
      
    # ---- Apply Smoking Transitions Initially ----
    
    # Turn the lifes alive matrix into format 
    # Projection Time, Smoking Status, Ages, gender, Policy Type, Cohorts, Withdrawn Status
    
    Policyholders_Future <- aperm(Policyholders_Future, c(3,5,2,4,6,1,7))
    
    # Build a smoking transition assumption matrix between each category
    
    transition_matrix <- array(c(0.0001, 0.02), dim = c(SMOKING_TYPES, MAX_AGE, GENDER, POLICY_TYPE, COHORTS))
    
    # Take the current makeup of population to account for transitions into smoking etc.
    
    Current_makeup <- Policyholders_Future[j,,,,,,NOT_LAPSED]
    
    # Adjust the non-smoker population (Equals number of people staying in non-smoking + Number of smokers becoming non-smokers)
    
    Policyholders_Future[j,NON_SMOKER,,,,,NOT_LAPSED] <- Current_makeup[NON_SMOKER,,,,] * (1-transition_matrix[NON_SMOKER,,,,]) +
                                          Current_makeup[SMOKER,,,,] * transition_matrix[SMOKER,,,,]
    
    # Adjust the smoker population (Equals number of people staying in smoking + Number of non-smokers becoming smokers)
    
    
    Policyholders_Future[j,SMOKER,,,,,NOT_LAPSED] <- Current_makeup[NON_SMOKER,,,,] * transition_matrix[NON_SMOKER,,,,] +
                                            Current_makeup[SMOKER,,,,] * (1-transition_matrix[SMOKER,,,,])
    
    # Turn the lifes alive matrix into format (number represents previous dimension)
    # Cohorts (6), Ages (3), Projection Time(1), gender(4), smoking status(2), policy type(5), withdrawn status(7) 
    
    Policyholders_Future <- aperm(Policyholders_Future, c(6,3,1,4,2,5,7))
    
    # ---- Apply Mortality ----
    
    # Rearrange matrix (Projection Time, Age, Gender, Smoking, Policy, Cohorts, Withdrawn Status)
    
    Policyholders_Future <- aperm(Policyholders_Future, c(3,2,4,5,6,1,7))
    
    # Apply mortality assumptions (i.e. seperate assumptions for age, gender, smoking and policy)
    
    mortality_matrix <- array(c(0.01), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, POLICY_TYPE, COHORTS))
    
    Policyholders_Future[j,,,,,,NOT_LAPSED] <- (1- mortality_matrix)* Policyholders_Future[j,,,,,,NOT_LAPSED]
    
    # ---- Apply Withdraw ----
    
    # Withdrawing assumptions for different ages, gender etc. for term insurance
    
    withdraw_matrix <- array(c(0.01), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, COHORTS))
    
    # Keep track of participants who are withdrawn
    
    Policyholders_Future[j,,,,T20,,LAPSED] <- withdraw_matrix* Policyholders_Future[j,,,,T20,,NOT_LAPSED]
    
    # Remove Participants who are withdrawn
    
    Policyholders_Future[j,,,,T20,,NOT_LAPSED] <- (1-withdraw_matrix)* Policyholders_Future[j,,,,T20,,NOT_LAPSED]
    
    
    # --- Apply Maturity Removal ----
    
    # In here we would need to loop through cohorts i.e. if the year of projection - cohort
    
    # i.e 2024, year of projection 21, 23 - 24 + 21 = 20
    
    # Put all the policies in the withdraw status as required
    
    Policyholders_Future[j,,,,T20,j+3,LAPSED] <- Policyholders_Future[j,,,,T20,j+3,NOT_LAPSED]
    
    # Set lives in policy to 0 (at end fo the term)
    
    Policyholders_Future[j,,,,T20,j+3,NOT_LAPSED] <- 0

    # Rearrange matrix back to standard form:
    # Cohorts (6), Age (2), Projection Time (1), Gender(3), Smoking (4), Policy (5), Withdrawn (7) 
    #(Projection Time, Age, Gender, Smoking, Policy, Cohorts, Withdrawn Status)
    
    Policyholders_Future <- aperm(Policyholders_Future, c(6,2,1,3,4,5,7))
    
}

y <- Policyholders_Future[1,,,,,,2]

y <- as.data.frame(y)





inforce_raw %>%
  mutate(policy_term = Issue.year + 19) %>%
  filter(policy_term == Year.of.Lapse & Policy.type == "T20" & Issue.year == 2005)



# 
# array(c(0.05), dim = c(10,10,10)) * array(c(0.07), dim = c(10,10,10))
# 
# dim(mortality_matrix)
# 
# # First update the first 23 years of projection to see against the past mortality rates
# 
# Policyholders_Current <- array(c(0), dim = c(23, MAX_AGE, 23, GENDER, SMOKING_TYPES, 
#                                                                 POLICY_TYPE, WITHDRAWN))
# 
# 
# c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES, 
#   POLICY_TYPE, WITHDRAWN)
# 
# 
# 
# 
# inforce_clean <- inforce_raw
# 
# Data_Set <- inforce_clean %>% 
#   mutate(Birth.Year = Issue.year - Issue.age,
#     type = paste(Issue.year, Issue.age, Sex, Smoker.Status, Policy.type, Year.of.Lapse, Year.of.Death)) %>%
#   group_by(type) %>%
#   summarise(count = n(),
#             year = mean(Issue.year),
#             age = mean(Issue.age),
#             gender = unique(Sex),
#             smoker = unique(Smoker.Status),
#             policy = unique(Policy.type),
#             lapse_year = mean(Year.of.Lapse),
#             death_year = mean(Year.of.Death))



