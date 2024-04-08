# ---- Variables ----

NUMBER_OF_SIMULATIONS <- 10

COHORTS <- 23
MAX_AGE <- 120
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

DISTRIBUTION_CHANNEL <- 3

Comission_Initial <- c(0.45,0.8,0)
Comission_Renewal <- c(0.02,0.04,0)
Comission_Single <- c(0.025,0.05,0)

# Initial Expenses
Fixed_Initial_Cost <- 360.412

Yearly_Cost <- c(96, 156)

Mortality_Cost <- c(420,720)

reserve_rate <- 0.04

mortality_rate <- fread("~/ACTL4001/Initialised_Data/weighted_qx.csv")

withdrawl_rate <- rep(0.01, 120)

# ---- Data / Package Read In

library("ggplot2")
library("dplyr")
library('git2r')
library("git2rdata")
library("data.table")
library("lifecontingencies")


setwd("~//")
inforce_raw <- read.csv("~/ACTL4001/Case Data/2024-srcsc-superlife-inforce-dataset.csv")

repo <- repository("~/ACTL4001/Initialised_Data/")

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

Policyholders_Deaths <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                            POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Policyholders_Lapse <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                           POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Policyholders_Joining <- array(c(0), dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                             POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

# Set this order for the loop below: Projection Time, Age, Gender, Smoking, Policy, Cohorts, Sum Assured

# ---- Initialise Current Cohorts ----

# Initialise the matrix of number of mortalities at each time point

Parameter_Dimensions <- expand.grid(cohort = 1:COHORTS,
                                    age = 1:MAX_AGE,
                                    year = 1:PROJECTION_TIME,
                                    gender = 1:GENDER,
                                    smoking = 1:SMOKING_TYPES,
                                    policy_type = 1:POLICY_TYPE,
                                    sum_assured = 1:SUM_ASSURED,
                                    distribution_channel = 1:DISTRIBUTION_CHANNEL)

inforce_current <- inforce_raw %>%
  mutate(age = Issue.age + Year.of.Death - Issue.year,
         grouping = paste(Issue.year, age, Year.of.Death, Sex, Smoker.Status, Policy.type,
                          Face.amount, Distribution.Channel)) %>%
  filter(Year.of.Death > 2003) %>%
  group_by(grouping) %>%
  summarise(number = n(),
            age = unique(age))


Gender <- c("M", "F")
Smoke <- c("NS", "S")
Policy <- c("SPWL", "T20")
Sum_Assured <- unique(inforce_raw$Face.amount)
Distribution <- unique(inforce_raw$Distribution.Channel)

#------ Sort Death Timings

# Determine number of deaths in each cohort period

Parameter_Dimensions <- Parameter_Dimensions %>%
  mutate(grouping= paste(cohort +2000,age,year+2003, Gender[gender], Smoke[smoking], Policy[policy_type],
                         Sum_Assured[sum_assured], Distribution[distribution_channel]))

merge_data <- merge(Parameter_Dimensions, inforce_current, by = "grouping", all.x = TRUE)

# Reorder the merged data to match the order of Parameter_Dimensions

merge_data <- merge_data %>%
  mutate(order = match(grouping, Parameter_Dimensions$grouping)) %>%
  arrange(order)

merge_data$number[is.na(merge_data$number)] <- 0


Policyholders_Deaths[,,2:21,,,,,] <- merge_data$number

# Check the total death counts are equal

sum(Policyholders_Deaths) == sum(inforce_raw$Year.of.Death > 2003, na.rm = T)

#----- Sort Withdrawl Timings ----

sum(Policyholders_Lapse) == sum(as.numeric(inforce_raw$Lapse.Indicator), na.rm = T)
# Initialise the number of withdrawls at each time point

inforce_current <- inforce_raw %>%
  mutate(age = Issue.age + Year.of.Lapse - Issue.year,
         grouping = paste(Issue.year, age, Year.of.Lapse, Sex, Smoker.Status, Policy.type,
                          Face.amount, Distribution.Channel)) %>%
  filter(Year.of.Lapse > 2003) %>%
  group_by(grouping) %>%
  summarise(number = n(),
            age = unique(age))


Parameter_Dimensions <- Parameter_Dimensions %>%
  mutate(grouping= paste(cohort +2000,age,year+2003, Gender[gender], Smoke[smoking], Policy[policy_type],
                         Sum_Assured[sum_assured], Distribution[distribution_channel]))

merge_data <- merge(Parameter_Dimensions, inforce_current, by = "grouping", all.x = TRUE)

# Reorder the merged data to match the order of Parameter_Dimensions

merge_data <- merge_data %>%
  mutate(order = match(grouping, Parameter_Dimensions$grouping)) %>%
  arrange(order)

merge_data$number[is.na(merge_data$number)] <- 0

Policyholders_Lapse[,,2:21,,,,,] <- merge_data$number

sum(Policyholders_Lapse) == sum(inforce_raw$Year.of.Lapse > 2003, na.rm = T)

#-------------- Sort Current Policyholders ----

# Get a starting number of policyholders in each cohort
# This means we can incrementally add them in each year of projection and apply decrements.

Parameter_Dimensions <- expand.grid(cohort = 1:COHORTS,
                                    age = 1:MAX_AGE,
                                    gender = 1:GENDER,
                                    smoking = 1:SMOKING_TYPES,
                                    policy_type = 1:POLICY_TYPE,
                                    sum_assured = 1:SUM_ASSURED,
                                    distribution_channel = 1:DISTRIBUTION_CHANNEL)


inforce_current <- inforce_raw

inforce_current$Year.of.Death[is.na(inforce_current$Year.of.Death)] <- 10000
inforce_current$Year.of.Lapse[is.na(inforce_current$Year.of.Lapse)] <- 10000  

inforce_current <- inforce_current %>%
  mutate(grouping = paste(Issue.year, Issue.age, Sex, Smoker.Status, Policy.type,
                          Face.amount, Distribution.Channel)) %>%
  filter(Year.of.Death > 2003, Year.of.Lapse > 2003 ) %>%
  group_by(grouping) %>%
  summarise(number = n())

Parameter_Dimensions <- Parameter_Dimensions %>%
  mutate(grouping = paste(cohort +2000,age, Gender[gender], Smoke[smoking], Policy[policy_type],
                          Sum_Assured[sum_assured], Distribution[distribution_channel]))

merge_data <- merge(Parameter_Dimensions, inforce_current, by = "grouping", all.x = TRUE)

# Reorder the merged data to match the order of Parameter_Dimensions

merge_data <- merge_data %>%
  mutate(order = match(grouping, Parameter_Dimensions$grouping)) %>%
  arrange(order)

merge_data$number[is.na(merge_data$number)] <- 0

Policyholders_Joining[,,,,,,] <- merge_data$number

#---------------------------------------------------------------------------------------------------------

Commission_Initial_Array <- array(c(0), dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                                POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

# Telemarketer, then agent, then online - for T20
Commission_Initial_Array[,,,,T20,,1] <- Comission_Initial[1]
Commission_Initial_Array[,,,,T20,,2] <- Comission_Initial[2]
Commission_Initial_Array[,,,,T20,,3] <- Comission_Initial[3]

# # Telemarketer, then agent, then online - for SPWL
Commission_Initial_Array[,,,,SPWL,,1] <- Comission_Single[1]
Commission_Initial_Array[,,,,SPWL,,2] <- Comission_Single[2]
Commission_Initial_Array[,,,,SPWL,,3] <- Comission_Single[3]

#-----------------------------------------------------------------------------------------------------------

Commission_Renewal_Array <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME+1, GENDER, SMOKING_TYPES,
                                                POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Commission_Renewal_Array[,,,,,T20,,1] <- Comission_Renewal[1]
Commission_Renewal_Array[,,,,,T20,,2] <- Comission_Renewal[2]
Commission_Renewal_Array[,,,,,T20,,3] <- Comission_Renewal[3]


#------------------------------------------------------------------------------------------------------------

Fixed_Initial_Cost_Array <- array(Fixed_Initial_Cost, dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                                             POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))


#-------------------------------------------------------------------------------------------------------------

Yearly_Cost_Array <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME+1, GENDER, SMOKING_TYPES,
                                                              POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Yearly_Cost_Array[,,,,,SPWL,,] <- Yearly_Cost[1]
Yearly_Cost_Array[,,,,,T20,,] <- Yearly_Cost[2]

#-------------------------------------------------------------------------------------------------------------

Mortality_Cost_Array <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME+1, GENDER, SMOKING_TYPES,
                                            POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Mortality_Cost_Array[,,,,,SPWL,,] <- Mortality_Cost[1]
Mortality_Cost_Array[,,,,,T20,,] <- Mortality_Cost[2]

#-------------------------------------------------------------------------------------------------------------

Term_Assurance <- function(age, length, rate) {

  Term_Assurance <- case_when(length == 0 ~ 0,
                              .default = sum(apply(matrix(c(1:length), ncol = 1, nrow = length), MARGIN = 2, function(x){
                                pxt(multi_dec_table, age,x-1)*
                                  qxt(life_table, age + x-1, 1) *
                                  (1+rate) ^-x
                              })))
  
  return(Term_Assurance)
}  



Term_Annuity <- function(age, length, rate) {
  
  Term_Annuity <- case_when(length == 0 ~ 0,
                            .default = sum(apply(matrix(c(1:length), ncol = 1, nrow = length), MARGIN = 2, function(x) {
                              pxt(multi_dec_table, age,x-1)*
                                (1+rate) ^-(x-1)
                            } )))
  return(Term_Annuity)  
}

Reserve_Array <- array(c(0), dim = c(MAX_AGE, COHORTS, GENDER, SMOKING_TYPES, POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))
Reserve_Array_eop <- array(c(0), dim = c(MAX_AGE, COHORTS, GENDER, SMOKING_TYPES, POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

reserve_function <- function(age, projection_time, policy, sums_assured) {

  if (policy == SPWL) {
    
    length = MAX_AGE - age - projection_time+1
    
    Net_Premium <- sums_assured * Term_Assurance(age,projection_time-1+length,reserve_rate)
    
    Prospective_Reserve <- sums_assured * Term_Assurance(age+(projection_time-1),length, reserve_rate) - 
      ifelse(projection_time == 1, Net_Premium, 0)
  
   } else{
     
     length = 20
     
      Net_Premium <- sums_assured * Term_Assurance(age,length,reserve_rate) / Term_Annuity(age,length, reserve_rate)
      
      Prospective_Reserve <- ifelse(length > projection_time, sums_assured * Term_Assurance(age+(projection_time-1),length-(projection_time-1), reserve_rate) - 
        Net_Premium * Term_Annuity(age+(projection_time-1), length-(projection_time-1), reserve_rate),0)
   }
  
  return(Prospective_Reserve)
}


for (k in 1:GENDER) {
    
  for (l in 1:SMOKING_TYPES) {
  
    for (m in 1:POLICY_TYPE) {
      
      if (k == 1 & l == 1) {
        mortality_prob <- mortality_rate$weighted_male_NS_qx
      } else if (k == 1 & l == 2) {
        mortality_prob <- mortality_rate$weighted_male_S_qx
      } else if (k == 2 & l == 1) {
        mortality_prob <- mortality_rate$weighted_female_NS_qx
      } else {
        mortality_prob <- mortality_rate$weighted_female_S_qx
      }
      
      
      life_table <- probs2lifetable(mortality_prob,
                                    radix = 10000,
                                    type = "qx")
      
      # Assume Withdrawl only happens at the end of the policy year
      # Noone would pay for a policy and terminate this (unless exceptional circumstances)
      
      if(m == 1) {
        withdrawl_prob <- rep(0,120)
      } else{
        withdrawl_prob <- withdrawl_rate
      }
      
      withdrawl_table <- probs2lifetable(withdrawl_prob * (1-mortality_prob), 
                                         radix = 10000,
                                         type = "qx")
      
      
      # Build Multi-Decrement Table
      
      multi_dec_table <- probs2lifetable(mortality_prob + withdrawl_prob * (1-mortality_prob),
                                         radix = 10000,
                                         type = "qx")
    
    for (i in 25:75) {
      
      for(j in 1:(COHORTS)){
        
          for (n in 1:SUM_ASSURED) {
         
            Reserve_Array[i+j-1,j,k,l,m,n,] <- reserve_function(i,j,m,Sum_Assured[n])
            Reserve_Array_eop[i+j-1,j,k,l,m,n,] <- reserve_function(i,j+1,m,Sum_Assured[n])
            
          } 
        }
      }
    }
  }
}


Cohort_Reserve_Array <- array(c(0), dim(Policyholders_Deaths))
Cohort_Reserve_Array_eop <- array(c(0), dim(Policyholders_Deaths))

# Sort the reserves into the correct cohort timings

# For the first three years
Cohort_Reserve_Array[1,,,,,,,] <- Reserve_Array[1:MAX_AGE,3:(PROJECTION_TIME+3),,,,,]
Cohort_Reserve_Array[2,,,,,,,] <- Reserve_Array[1:MAX_AGE,2:(PROJECTION_TIME+2),,,,,]

Cohort_Reserve_Array_eop[1,,,,,,,] <- Reserve_Array_eop[,3:(PROJECTION_TIME+3),,,,,]
Cohort_Reserve_Array_eop[2,,,,,,,] <- Reserve_Array_eop[,2:(PROJECTION_TIME+2),,,,,]

for (i in 1:(PROJECTION_TIME+1)) {
  
  Cohort_Reserve_Array[i+2,,i:(PROJECTION_TIME+1),,,,,] <- Reserve_Array[,1:(PROJECTION_TIME+2-i),,,,,]
  Cohort_Reserve_Array_eop[i+2,,(i):(PROJECTION_TIME+1),,,,,] <- Reserve_Array_eop[,1:(PROJECTION_TIME+2-i),,,,,]
}
Cohort_Reserve_Array[1,,,,,,,]
#-----------------------------------------------------------------------------------------------------------
Reserves[3,,,,,,4,]
Premium_Array[,,1,1,4]

Premium <- array(c(0), dim = dim(Policyholders_Deaths))

Term_Premium <- array(unlist(fread("~/ACTL4001/Initialised_Data/Net_Premium_Term.csv")),
                      dim = c(120, GENDER, SMOKING_TYPES, DISTRIBUTION_CHANNEL, SUM_ASSURED))

Term_Premium_Reorder <- Term_Premium
Term_Premium_Reorder[,,,1,] <- Term_Premium[,,,2,]
Term_Premium_Reorder[,,,2,] <- Term_Premium[,,,1,]

Term_Premium <- Term_Premium_Reorder

WL_Single <- array(unlist(fread("~/ACTL4001/Initialised_Data/Net_Premium_SPWL.csv")),
                      dim = c(120, GENDER, SMOKING_TYPES, DISTRIBUTION_CHANNEL, SUM_ASSURED))

WL_Single_Reorder <- WL_Single
WL_Single_Reorder[,,,1,] <- WL_Single[,,,2,]
WL_Single_Reorder[,,,2,] <- WL_Single[,,,1,]

WL_Single <- WL_Single_Reorder
WL_Single

for (i in 1:COHORTS){
  
  for (j in 1:PROJECTION_TIME+1) {
    
    
    if (100 + j-i+3 <= 121 & i<= j+2 & j+2-i < 20) {
      
      Premium[i,(j-i+3):(100+j-i+3-1),j,,,T20,,] <- Term_Premium[1:(100),,,,]
      
    } else if (i <= j+2 & j+2-i <= 21) {  
      Premium[i,(j+3-i):(120),j,,,T20,,] <- Term_Premium[1:(120-j-2+i),,,,]
    }
    length((j+3-i):(120))
    if (i == j+2) {
      Premium[i,,j,,,SPWL,,] <- WL_Single
      
    }
  }  
}



#-------------------------------------------------------------------------------------------------------------

fwrite(as.data.frame(Policyholders_Deaths), "~/ACTL4001/Initialised_Data/Deaths_Policyholders.csv")
fwrite(as.data.frame(Policyholders_Lapse), "~/ACTL4001/Initialised_Data/Lapsing_Policyholders.csv")
fwrite(as.data.frame(Policyholders_Joining), "~/ACTL4001/Initialised_Data/Starting_Policyholders.csv")
fwrite(as.data.frame(Commission_Initial_Array), "~/ACTL4001/Initialised_Data/Initial_Commission.csv")
fwrite(as.data.frame(Commission_Renewal_Array), "~/ACTL4001/Initialised_Data/Renewal_Commission.csv")
fwrite(as.data.frame(Fixed_Initial_Cost_Array), "~/ACTL4001/Initialised_Data/Fixed_Initial_Cost.csv")
fwrite(as.data.frame(Yearly_Cost_Array), "~/ACTL4001/Initialised_Data/Yearly_Cost.csv")
fwrite(as.data.frame(Mortality_Cost_Array), file = "~/ACTL4001/Initialised_Data/Mortality_Cost.csv")
fwrite(as.data.frame(Cohort_Reserve_Array), file = "~/ACTL4001/Initialised_Data/Current_Reserves.csv" )
fwrite(as.data.frame(Cohort_Reserve_Array_eop), file = "~/ACTL4001/Initialised_Data/Current_Reserves_eop.csv" )
fwrite(as.data.frame(Premium), file = "~/ACTL4001/Initialised_Data/Premium.csv")
