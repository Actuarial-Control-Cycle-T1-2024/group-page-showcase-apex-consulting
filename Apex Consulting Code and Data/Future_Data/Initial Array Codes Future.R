##############################
######### Introduction #######
##############################

# Code has been developed to build all the inputs 
# into the future projection model

# This is building it into a similar format, then turning it into a matrix
# structure which is read in the future projection model.


# ---- Variables ----

COHORTS <- 43
MAX_AGE <- 120
PROJECTION_TIME <- 20
SMOKING_TYPES <- 2
GENDER <- 2
POLICY_TYPE <- 2


SPWL <- 1
T20 <- 2

MALE <- 1
FEMALE <- 2

NOT_LAPSED <- 1
LAPSED <- 2

SMOKER <- 2
NON_SMOKER <- 1

SUM_ASSURED <- 6

DISTRIBUTION_CHANNEL <- 3

Comission_Initial <- c(0.45,0.8,0)
Comission_Renewal <- c(0.02,0.04,0)
Comission_Single <- c(0.025,0.05,0)

# Initial Expenses
Fixed_Initial_Cost <- 150

Yearly_Cost <- c(35, 45)

Mortality_Cost <- c(150,300)

reserve_rate <- 0.045

mortality_rate <- fread("~/ACTL4001/Initialised_Data/weighted_qx.csv")

withdrawl_rate <- rep(0.01, 120)

expense_margin <- c(0.1)


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

#-------------- Sort Current Policyholders ----

Policyholders_Future <- array(c(0), dim = c(23, MAX_AGE, GENDER, SMOKING_TYPES,
                                            POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

inforce_current <- inforce_raw[is.na(inforce_raw$Year.of.Lapse) == T & is.na(inforce_raw$Year.of.Death) == T,]

# Need to group each data point into their individual assigned group (with no withdrawls at t = 0)

inforce_current <- inforce_current %>%
  mutate(Starting_Age = Issue.age + 2023 - Issue.year,
         grouping = paste(Issue.year, Starting_Age, Sex, Smoker.Status, Policy.type,
                          Face.amount, Distribution.Channel)) %>%
  group_by(grouping) %>%
  summarise(number = n(),
            cohort = unique(Issue.year),
            Starting_Age = unique(Starting_Age),
            gender  = unique(Sex),
            smoking = unique(Smoker.Status),
            policy = unique(Policy.type),
            sum_assured = unique(Face.amount),
            distribution_channel = unique(Distribution.Channel))



Gender <- c("M", "F")
Smoke <- c("NS", "S")
Policy <- c("SPWL", "T20")
Sum_Assured <- unique(inforce_raw$Face.amount)
Distribution <- unique(inforce_raw$Distribution.Channel)


# Get a starting number of policyholders in each cohort
# This means we can incrementally add them in each year of projection and apply decrements.

Parameter_Dimensions <- expand.grid(cohort = 1:23,
                                    age = 1:MAX_AGE,
                                    gender = 1:GENDER,
                                    smoking = 1:SMOKING_TYPES,
                                    policy_type = 1:POLICY_TYPE,
                                    sum_assured = 1:SUM_ASSURED,
                                    distribution_channel = 1:DISTRIBUTION_CHANNEL)


Parameter_Dimensions <- Parameter_Dimensions %>%
  mutate(grouping = paste(cohort +2000,age, Gender[gender], Smoke[smoking], Policy[policy_type],
                          Sum_Assured[sum_assured], Distribution[distribution_channel]))

merge_data <- merge(Parameter_Dimensions, inforce_current, by = "grouping", all.x = TRUE)

# Reorder the merged data to match the order of Parameter_Dimensions

merge_data <- merge_data %>%
  mutate(order = match(grouping, Parameter_Dimensions$grouping)) %>%
  arrange(order)

merge_data$number[is.na(merge_data$number)] <- 0

Policyholders_Future[,,,,,,] <- merge_data$number

#---------------------------------------------------------------------------------------------------------

# Use the past 5 years of information to reflect the future 5 years

T20_mix <- inforce_raw %>%
  filter(Policy.type == "T20") %>%
  filter(Issue.year > 2018) %>%
  mutate(grouping = paste(Issue.age, Sex, Smoker.Status, Face.amount, Distribution.Channel)) %>%
  group_by(grouping) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count))

T20_mix_array <- array(c(0), dim=  c(MAX_AGE, GENDER, SMOKING_TYPES, 
                                     SUM_ASSURED, DISTRIBUTION_CHANNEL, COHORTS-23))

Parameter_Dimensions <- expand.grid(age = 1:MAX_AGE,
                                    gender = 1:GENDER,
                                    smoking = 1:SMOKING_TYPES,
                                    sum_assured = 1:SUM_ASSURED,
                                    distribution_channel = 1:DISTRIBUTION_CHANNEL)


Parameter_Dimensions <- Parameter_Dimensions %>%
  mutate(grouping = paste(age, Gender[gender], Smoke[smoking],
                          Sum_Assured[sum_assured], Distribution[distribution_channel]))

merge_data <- merge(Parameter_Dimensions, T20_mix, by = "grouping", all.x = TRUE)

# Reorder the merged data to match the order of Parameter_Dimensions

merge_data <- merge_data %>%
  mutate(order = match(grouping, Parameter_Dimensions$grouping)) %>%
  arrange(order)

merge_data$proportion[is.na(merge_data$proportion)] <- 0

T20_mix_array[,,,,,] <- merge_data$proportion

T20_count <- inforce_raw %>%
  filter(Policy.type == "T20") %>%
  group_by(Issue.year) %>%
  summarise(count = n())


plot(T20_count, col = graph4col[1], xlab = "Issue Year", ylab=  "Policies Written",
     main=  "T20 policies written over time")
# Relatively Linear growth in number of policies issued
coefficients <- lm(T20_count$count ~ T20_count$Issue.year)
abline(coef = coefficients$coefficients, col = graph4col[4], lwd=  2)


T20_policies <- array(coefficients$coefficients[1] + (2000+ c(24:COHORTS))*coefficients$coefficients[2],
                      dim = c(COHORTS-23, MAX_AGE, GENDER, SMOKING_TYPES, 
                              SUM_ASSURED, DISTRIBUTION_CHANNEL))

T20_New_Policies <- T20_policies * aperm(T20_mix_array, c(6,1,2,3,4,5))

#--------------------------------------------------------------------------------------------------------

# Use the last 5 years of mix experience to provide a proxy for future new policies

SPWL_mix <- inforce_raw %>%
  filter(Policy.type == "SPWL") %>%
  filter(Issue.year > 2018) %>%
  mutate(grouping = paste(Issue.age, Sex, Smoker.Status, Face.amount, Distribution.Channel)) %>%
  group_by(grouping) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count))

SPWL_mix_array <- array(c(0), dim=  c(MAX_AGE, GENDER, SMOKING_TYPES, 
                                     SUM_ASSURED, DISTRIBUTION_CHANNEL, COHORTS-23))

Parameter_Dimensions <- expand.grid(age = 1:MAX_AGE,
                                    gender = 1:GENDER,
                                    smoking = 1:SMOKING_TYPES,
                                    sum_assured = 1:SUM_ASSURED,
                                    distribution_channel = 1:DISTRIBUTION_CHANNEL)

Parameter_Dimensions <- Parameter_Dimensions %>%
  mutate(grouping = paste(age, Gender[gender], Smoke[smoking],
                          Sum_Assured[sum_assured], Distribution[distribution_channel]))


merge_data <- merge(Parameter_Dimensions, SPWL_mix, by = "grouping", all.x = TRUE)

# Reorder the merged data to match the order of Parameter_Dimensions

merge_data <- merge_data %>%
  mutate(order = match(grouping, Parameter_Dimensions$grouping)) %>%
  arrange(order)

merge_data$proportion[is.na(merge_data$proportion)] <- 0

SPWL_mix_array[,,,,,] <- merge_data$proportion

SPWL_count <- inforce_raw %>%
  filter(Policy.type == "SPWL") %>%
  group_by(Issue.year) %>%
  summarise(count = n())

plot(SPWL_count, col = graph4col[1], xlab = "Issue Year", ylab=  "Policies Written",
     main=  "SPWL policies written over time")
# Relatively Linear growth in number of policies issued
coefficients <- lm(SPWL_count$count ~ SPWL_count$Issue.year)
abline(coef = coefficients$coefficients)


SPWL_policies <- array(coefficients$coefficients[1] + (2000+ c(24:COHORTS))*coefficients$coefficients[2],
                      dim = c(COHORTS-23, MAX_AGE, GENDER, SMOKING_TYPES, 
                              SUM_ASSURED, DISTRIBUTION_CHANNEL))

SPWL_New_Policies <- SPWL_policies * aperm(SPWL_mix_array, c(6,1,2,3,4,5))

New_Policies <- array(c(SPWL_New_Policies, T20_New_Policies), dim = c(dim(SPWL_New_Policies),2))

New_Policies <- aperm(New_Policies, c(1,2,3,4,7,5,6))

#--------------------------------------------------------------------------------------------------------

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

Commission_Renewal_Array <- array(c(0), dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                                POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Commission_Renewal_Array[,,,,T20,,1] <- Comission_Renewal[1]
Commission_Renewal_Array[,,,,T20,,2] <- Comission_Renewal[2]
Commission_Renewal_Array[,,,,T20,,3] <- Comission_Renewal[3]


#------------------------------------------------------------------------------------------------------------

Fixed_Initial_Cost_Array <- array(Fixed_Initial_Cost, dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                                              POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))


#-------------------------------------------------------------------------------------------------------------

Yearly_Cost_Array <- array(c(0), dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                         POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Yearly_Cost_Array[,,,,SPWL,,] <- Yearly_Cost[1]
Yearly_Cost_Array[,,,,T20,,] <- Yearly_Cost[2]

#-------------------------------------------------------------------------------------------------------------

Mortality_Cost_Array <- array(c(0), dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                            POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Mortality_Cost_Array[,,,,SPWL,,] <- Mortality_Cost[1]
Mortality_Cost_Array[,,,,T20,,] <- Mortality_Cost[2]

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

Reserve_Array <- array(c(0), dim = c(MAX_AGE, COHORTS, GENDER, SMOKING_TYPES, POLICY_TYPE, SUM_ASSURED,DISTRIBUTION_CHANNEL))
Reserve_Array_eop <- array(c(0), dim = c(MAX_AGE, COHORTS, GENDER, SMOKING_TYPES, POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

reserve_function <- function(age, projection_time, policy) {
  
  if (policy == SPWL) {
    
    length = max(MAX_AGE - age - projection_time,1)
    
    Net_Premium <- Term_Assurance(age,length,reserve_rate)
    
    Prospective_Reserve <- Term_Assurance(age+(projection_time-1),length, reserve_rate)  - 
      ifelse(projection_time == 1, Net_Premium, 0)
  } else {
    
    length = 20
    
    Net_Premium <-Term_Assurance(age,length,reserve_rate) / Term_Annuity(age,length, reserve_rate)
    
    Prospective_Reserve <- ifelse(length > projection_time,Term_Assurance(age+(projection_time-1),length-(projection_time-1), reserve_rate) -
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
    
    life_table <- probs2lifetable(c(0,mortality_prob),
                                  radix = 10000,
                                  type = "qx")
    
    # Assume Withdrawl only happens at the end of the policy year
    # Noone would pay for a policy and terminate this (unless exceptional circumstances)
    
    if (m == 1) {
      withdrawl_rate <- rep(0,120)
    } else {
      withdrawl_rate <- rep(0.01,120)
    }
    
    withdrawl_table <- probs2lifetable(c(0,withdrawl_rate * (1-mortality_prob)), 
                                       radix = 10000,
                                       type = "qx")
    
    # Build Multi-Decrement Table
    
    multi_dec_table <- probs2lifetable(c(0,mortality_prob + withdrawl_rate * (1-mortality_prob)),
                                       radix = 10000,
                                       type = "qx")
  
    
  for (i in 25:88) {
    
    for(j in 1:(COHORTS)) {

          if (i +j - 1 < MAX_AGE){
            
            Reserve_Array[i+j-1,j,k,l,m,,] <- reserve_function(i,j,m)
          
            Reserve_Array_eop[i+j-1,j,k,l,m,,] <- reserve_function(i,j+1,m)
          }
        }
      }
    }
  }
}

Face_Values <- array(Sum_Assured, dim = c(SUM_ASSURED, COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                              POLICY_TYPE,DISTRIBUTION_CHANNEL))

Face_Values <- aperm(Face_Values, c(3,2,4:6,1,7))

Reserve_Array <- Reserve_Array * Face_Values

Reserve_Array_eop <- Reserve_Array_eop * Face_Values

#----------------------------------------------------------------------------------------------------------------------------------

Premium <- array(c(0),dim = c(MAX_AGE,COHORTS, GENDER, SMOKING_TYPES, POLICY_TYPE, DISTRIBUTION_CHANNEL, SUM_ASSURED))

Term_Premium <- array(unlist(fread("~/ACTL4001/Future_Data/Net_Premium_Term.csv")),
                      dim = c(MAX_AGE, GENDER, SMOKING_TYPES, DISTRIBUTION_CHANNEL, SUM_ASSURED))

Term_Premium_Reorder <- Term_Premium
Term_Premium_Reorder[,,,1,] <- Term_Premium[,,,2,]
Term_Premium_Reorder[,,,2,] <- Term_Premium[,,,1,]

Term_Premium <- Term_Premium_Reorder

WL_Single <- array(unlist(fread("~/ACTL4001/Future_Data/Net_Premium_SPWL.csv")),
                   dim = c(MAX_AGE, GENDER, SMOKING_TYPES, DISTRIBUTION_CHANNEL, SUM_ASSURED))

WL_Single_Reorder <- WL_Single
WL_Single_Reorder[,,,1,] <- WL_Single[,,,2,]
WL_Single_Reorder[,,,2,] <- WL_Single[,,,1,]

WL_Single <- WL_Single_Reorder
WL_Single

for (i in 1:COHORTS){
  
  Premium[i:(MAX_AGE),i,,,T20,,] <- Term_Premium[1:(MAX_AGE-i+1),,,,]
  Premium[,i,,,SPWL,,] <- WL_Single
      
}  

#-------------------------------------------------------------------------------------------------------------

fwrite(as.data.frame(Policyholders_Future), "~/ACTL4001/Future_Data/Future_Policyholders.csv")
fwrite(as.data.frame(New_Policies), "~/ACTL4001/Future_Data/New_Policyholders.csv")
fwrite(as.data.frame(Commission_Initial_Array), "~/ACTL4001/Future_Data/Initial_Commission.csv")
fwrite(as.data.frame(Commission_Renewal_Array), "~/ACTL4001/Future_Data/Renewal_Commission.csv")
fwrite(as.data.frame(Fixed_Initial_Cost_Array), "~/ACTL4001/Future_Data/Fixed_Initial_Cost.csv")
fwrite(as.data.frame(Yearly_Cost_Array), "~/ACTL4001/Future_Data/Yearly_Cost.csv")
fwrite(as.data.frame(Mortality_Cost_Array), file = "~/ACTL4001/Future_Data/Mortality_Cost.csv")
fwrite(as.data.frame(Reserve_Array), file = "~/ACTL4001/Future_Data/Current_Reserves2.csv" )
fwrite(as.data.frame(Reserve_Array_eop), file = "~/ACTL4001/Future_Data/Current_Reserves_eop2.csv" )
fwrite(as.data.frame(Premium), file = "~/ACTL4001/Future_Data/Premium.csv" )
