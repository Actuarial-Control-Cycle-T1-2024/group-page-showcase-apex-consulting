# Use Script to test specific scenarios


# Change depending if STOCHASTIC or Non-STOCHASTIC run

#Base run is STOCHASTIC 0 or off
STOCHASTIC <- 0
NUMBER_OF_SIMULATIONS <- 5
INITIATIVE <- 0

#--------------- Past Mortality Savings

COHORTS <- 43
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

SUM_ASSURED <- 6

risk_discount_rate <- 0.1

DISTRIBUTION_CHANNEL <- 3

# ---- Data / Package Read In

library("ggplot2")
library("dplyr")
library("data.table")

inforce_raw <- read.csv("~/ACTL4001/Case Data/2024-srcsc-superlife-inforce-dataset.csv")

Gender <- c("M", "F")
Smoke <- c("NS", "S")
Policy <- c("SPWL", "T20")
Sum_Assured <- unique(inforce_raw$Face.amount)
Distribution <- unique(inforce_raw$Distribution.Channel)

#---------------- Array Read In

Policyholders_Inforce <- array(unlist(fread("~/ACTL4001/Future_Data/Future_Policyholders.csv")), 
                               dim = c(23, MAX_AGE, GENDER, SMOKING_TYPES,
                                       POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Policyholders_New<- array(unlist(fread("~/ACTL4001/Future_Data/New_Policyholders.csv")), 
                          dim = c(COHORTS-23,MAX_AGE, GENDER, SMOKING_TYPES,
                                  POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))


Commission_Initial <- array(unlist(fread("~/ACTL4001/Future_Data/Initial_Commission.csv")), 
                            dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                    POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL)) 

Commission_Renewal <- array(unlist(fread("~/ACTL4001/Future_Data/Renewal_Commission.csv")),
                            dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                    POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

# Initial Expenses
Fixed_Initial_Cost <- array(unlist(fread("~/ACTL4001/Future_Data/Fixed_Initial_Cost.csv")),
                                 dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                         POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))


Yearly_Cost <- array(unlist(fread("~/ACTL4001/Future_Data/Yearly_Cost.csv")),
                          dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                  POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL)) 

Mortality_Cost <- array(unlist(fread("~/ACTL4001/Future_Data/Mortality_Cost.csv")),
                             dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                     POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Reserves <- array(unlist(fread("~/ACTL4001/Future_Data/Current_Reserves2.csv")), 
                  dim = c(MAX_AGE, COHORTS, GENDER, SMOKING_TYPES,
                          POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))


Reserves_eop <- array(unlist(fread("~/ACTL4001/Future_Data/Current_Reserves_eop2.csv")), 
                      dim = c(MAX_AGE, COHORTS, GENDER, SMOKING_TYPES,
                              POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Premium <- array(unlist(fread("~/ACTL4001/Future_Data/Premium.csv")),
                 dim = c(MAX_AGE, COHORTS, GENDER, SMOKING_TYPES,
                         POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))


Mortality_Reduce <- array(unlist(fread("~/ACTL4001/Future_Data/Mortality_Reducs-NonSmokes.csv")), dim =c(120,10000,25))

Mortality_Reduce_smokers <- array(unlist(fread("~/ACTL4001/Future_Data/Mortality_Reducs-Smokes.csv")), dim =c(120,10000,25))

Cost_Reduce <- array(unlist(fread("~/ACTL4001/Future_Data/Cost_Amounts-NonSmokes.csv")), dim = c(120,10000,25))
Cost_Reduce_smokers <- array(unlist(fread("~/ACTL4001/Future_Data/Cost_Amounts-Smokes.csv")),dim =c(120,10000,25))

# Assumptions

Mortality_Adjustment <- seq(0.8, 1.2, by = 0.1)

# Investments will maintain at a steady rate (based on diversification of resorces)
# Inflation will return to Pre-Covid Levels

# Apply a discount to the premiums at 3.5%, assuming 10% expense
# Hence, term premium is lower for the last 20 years

Investment_Rate <- rep(0.05,21)

historical_inflation <- apply(matrix(c(0:22), nrow = 1), 2, function (x) {(1+0.035)^(x)})

Mortality_Rates <- fread("~/ACTL4001/Initialised_Data/weighted_qx.csv")

mortality_base <- array(c(0), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, POLICY_TYPE, COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL))

# Initialise the historical mortality rates

mortality_base[,MALE,NON_SMOKER,,,,] <- Mortality_Rates$weighted_male_NS_qx
mortality_base[,MALE,SMOKER,,,,] <- Mortality_Rates$weighted_male_S_qx
mortality_base[,FEMALE,NON_SMOKER,,,,] <- Mortality_Rates$weighted_female_NS_qx
mortality_base[,FEMALE,SMOKER,,,,] <- Mortality_Rates$weighted_female_S_qx

#------- Main Loop Code ----

Profit_Simulations <- matrix(c(0), nrow = NUMBER_OF_SIMULATIONS, ncol = PROJECTION_TIME+1)
Profit_Simulations_SPWL <- matrix(c(0), nrow = NUMBER_OF_SIMULATIONS, ncol = PROJECTION_TIME+1)
Profit_Simulations_T20 <- matrix(c(0), nrow = NUMBER_OF_SIMULATIONS, ncol = PROJECTION_TIME+1)

for (n in 1:NUMBER_OF_SIMULATIONS) {
  
  Inflation <- historical_inflation
  
  Policyholders_Mortality <- array(c(0), dim = c(PROJECTION_TIME + 1, MAX_AGE, GENDER, SMOKING_TYPES,
                                                 POLICY_TYPE, COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL))
  
  Policyholders_Withdrawl <- array(c(0), dim = c(PROJECTION_TIME + 1, MAX_AGE, GENDER, SMOKING_TYPES,
                                                 POLICY_TYPE, COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL))
  
  Policyholders_Current <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                               POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))
  
  # Initialise the current participants (at 2003)
  
  
  for (j in 2: (PROJECTION_TIME + 1)) {
    
    reduction_matrix <- array(c(Mortality_Reduce[,n,j], Mortality_Reduce_smokers[,n,j] ),
                              dim = c(MAX_AGE, SMOKING_TYPES, GENDER, POLICY_TYPE ,COHORTS,
                                      SUM_ASSURED, DISTRIBUTION_CHANNEL))
    
    reduction_matrix <- aperm(reduction_matrix, c(1,3,2,4:7))
    
    Policyholders_Current[1:23,,1,,,,,] <- Policyholders_Inforce  
    
    # Withdrawing assumptions for different ages, gender etc. for term insurance
    
    withdraw_matrix_IND <- array(c(0.01), dim = c(MAX_AGE, GENDER, SMOKING_TYPES,COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL))
    
    mortality_matrix_IND <- mortality_base *Mortality_Adjustment[n]
    
    # Make the rates dependent under the UDD assumption (Withdrawls assumed at end of period)
    
    mortality_matrix <- mortality_matrix_IND
    
    withdraw_matrix <- withdraw_matrix_IND * (1 - mortality_matrix_IND[,,,T20,,,])
    
    
    
    # Start by moving policy holders down their respective age for new projection
    # Move the first 1:j+21 cohorts (i.e at 2024, this would represent moving up to 2023 down)
    
    if (j != 1) {
      
      Policyholders_Current[1:(j+21),2:MAX_AGE,j,,,,,] <- Policyholders_Current[1:(j+21),1:(MAX_AGE-1),j-1,,,,,]  
      
      # Build the new cohort for example (lets just put a dummy of 1 person per cohort)
      
      Policyholders_Current[j+22,,j,,,,,] <- Policyholders_New[j-1,,,,,,]
      
    }
    # ---- Apply Smoking Transitions Initially ----
    
    # Turn the lifes alive matrix into format
    # Projection Time, Smoking Status, Ages, gender, Policy Type, Cohorts, Sum Assured
    
    Policyholders_Current <- aperm(Policyholders_Current, c(3,5,2,4,6,1,7,8))
    
    # Build a smoking transition assumption matrix between each category
    
    # #transition_matrix <- array(c(0.0001, 0.01), dim = c(SMOKING_TYPES, MAX_AGE, GENDER, POLICY_TYPE, COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL))
    # 
    # # Take the current makeup of population to account for transitions into smoking etc.
    # 
    # Current_makeup <- Policyholders_Current[j,,,,,,,]
    # 
    # # Adjust the non-smoker population (Equals number of people staying in non-smoking + Number of smokers becoming non-smokers)
    # 
    # Policyholders_Current[j,NON_SMOKER,,,,,,] <- Current_makeup[NON_SMOKER,,,,,,] * (1-transition_matrix[NON_SMOKER,,,,,,]) +
    #   Current_makeup[SMOKER,,,,,,] * transition_matrix[SMOKER,,,,,,]
    # 
    # # Adjust the smoker population (Equals number of people staying in smoking + Number of non-smokers becoming smokers)
    # 
    # 
    # Policyholders_Current[j,SMOKER,,,,,,] <- Current_makeup[NON_SMOKER,,,,,,] * transition_matrix[NON_SMOKER,,,,,,] +
    #   Current_makeup[SMOKER,,,,,,] * (1-transition_matrix[SMOKER,,,,,,])
    # 
    # # Turn the lifes alive matrix into format (number represents previous dimension)
    # # Cohorts (6), Ages (3), Projection Time(1), gender(4), smoking status(2), policy type(5), Sum Assured(7)
    # 
    
    Policyholders_Current <- aperm(Policyholders_Current, c(6,3,1,4,2,5,7,8))
    
    # ---- Apply Mortality ----
    
    # Rearrange matrix (Projection Time, Age, Gender, Smoking, Policy, Cohorts, Sum Assured)
    
    Policyholders_Current <- aperm(Policyholders_Current, c(3,2,4,5,6,1,7,8))
    
    # Apply mortality assumptions (i.e. seperate assumptions for age, gender, smoking and policy)
    
    Policyholders_Mortality[j,,,,,,,] <- mortality_matrix * Policyholders_Current[j,,,,,,,]
    
    # Keep track of participants who are withdrawn
    
    Policyholders_Withdrawl[j,,,,T20,,,] <- withdraw_matrix* Policyholders_Current[j,,,,T20,,,]
    #mortality_matrix[,,,,,,,j] * Policyholders_Current[j,,,,,,,]
    
    Policyholders_Current[j,,,,,,,] <-  Policyholders_Current[j,,,,,,,] * (1- mortality_matrix)
    
    # ---- Apply Withdraw ----
    
    # Remove Participants who are withdrawn
    
    Policyholders_Current[j,,,,T20,,,] <- (1-withdraw_matrix) * Policyholders_Current[j,,,,T20,,,]
    
    
    # --- Apply Maturity Removal ----
    
    # In here we would need to loop through cohorts i.e. if the year of projection - cohort
    
    # i.e born in 2024, year of projection 21, 23 - 24 + 21 = 20
    
    # Put all the policies in the withdraw status as required # At 2023 all 2004 cohort needs to be removed
    
    
    Policyholders_Withdrawl[j,,,,T20,j+3,,] <- Policyholders_Current[j,,,,T20,j+3,,]
    
    # Set lives in policy to 0 (at end fo the term)
    
    Policyholders_Current[j,,,,T20,j+3,, ] <- 0
    
    
    # Rearrange matrix back to standard form:
    # Cohorts (6), Age (2), Projection Time (1), Gender(3), Smoking (4), Policy (5), Withdrawn (7)
    #(Projection Time, Age, Gender, Smoking, Policy, Cohorts, Sum Assured)
    
    Policyholders_Current <- aperm(Policyholders_Current, c(6,2,1,3,4,5,7,8))
    
  }
  
  # ---- Expense Calculation
  
  #----- Calculating Mortality Savings
  
  Face_Values <- array(Sum_Assured, dim = c(SUM_ASSURED, COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                            POLICY_TYPE,DISTRIBUTION_CHANNEL))
  
  Face_Values <- aperm(Face_Values, c(2,3,4,5,6,1,7))
  
  Policyholders_Mortality <- aperm(Policyholders_Mortality, c(6,2,1,3,4,5,7,8))
  
  Policyholders_Withdrawl <- aperm(Policyholders_Withdrawl, c(6,2,1,3,4,5,7,8))
  
  Initial_Premium <- array(Premium[,1,,,,,], dim = c(MAX_AGE, GENDER, SMOKING_TYPES,
                                                     POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL, 20))
  
  Initial_Premium <- aperm(Initial_Premium, c(7, 1:6))
  
  Initial_Expenses <- (Fixed_Initial_Cost[1:(COHORTS-23),,,,,,] + Commission_Initial[1:(COHORTS-23),,,,,,] * Initial_Premium ) *
    Policyholders_New
  
  for (i in 2:(PROJECTION_TIME+1)) {
    
    Premium_at_point <- array(c(0), dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES, POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))
    Reserves_at_point_sop <- array(c(0), dim = dim(Premium_at_point))
    Reserves_at_point_eop <- array(c(0), dim = dim(Premium_at_point))
    
    Intervention_Cost <- array(c(Cost_Reduce[,n,i], Cost_Reduce_smokers[,n,i]), 
                               dim = c(MAX_AGE, SMOKING_TYPES, COHORTS, GENDER,
                                       POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))
    
    Intervention_Cost <- aperm(Intervention_Cost, c(3,1,4,2,5:7))
    
    # 2020 when j = 2 should be 2024
    # This is also cohort 20
    
    # All the policyholders at the start of the period
    for (j in 1:COHORTS) {
      
      if (i+22 >= j){
        
        Premium_at_point[j,,,,T20,,] <- Premium[,i+23-j,,,T20,,]   
        Reserves_at_point_sop[j,,,,,,] <- Reserves[,i+23-j,,,,,]
        Reserves_at_point_eop[j,,,,,,] <- Reserves_eop[,i+23-j,,,,,]
        
      }
      
      if (i+22 == j) {
        Premium_at_point[j,,,,SPWL,,] <- Premium[,j,,,SPWL,,]
      }
    }
    
    Premium_Income <- Premium_at_point * (Policyholders_Current[,,i,,,,,]+ Policyholders_Mortality[,,i,,,,,] + Policyholders_Withdrawl[,,i,,,,,])
    
    Duration_Expense <- (Commission_Renewal * Premium_at_point + Yearly_Cost + ifelse(INITIATIVE ==1, 
                                                                                      Intervention_Cost, 0)) * 
      (Policyholders_Current[,,i,,,,,] + Policyholders_Mortality[,,i,,,,,] + Policyholders_Withdrawl[,,i,,,,,])
    
    
    Death_Costs <- (Face_Values + Mortality_Cost) * Policyholders_Mortality[,,i,,,,,]
    
    Reserve_Amount <- Reserves_at_point_sop * (Policyholders_Current[,,i,,,,,] 
                                               + Policyholders_Mortality[,,i,,,,,] 
                                               + Policyholders_Withdrawl[,,i,,,,,])
    
    Net_Inflow <- Premium_Income - Duration_Expense * Inflation[i]
    
    
    Net_Inflow[i+22,,,,,,] <- Net_Inflow[i+22,,,,,,] - Initial_Expenses[i-1,,,,,,] +
      Policyholders_New[i-1,,,,,,] * (Commission_Renewal[i+22,,,,,,] * Premium_at_point[i+22,,,,,,] + 
                                        Yearly_Cost[i+22,,,,,,])
    
    Interest <- ( Net_Inflow + Reserve_Amount) * Investment_Rate[i]
    
    Reserve_Amount_eop <- Reserves_at_point_eop * (Policyholders_Current[,,i,,,,,])
    
    Profit_Array <-  (Net_Inflow) + Reserve_Amount +
      Interest- Death_Costs - Reserve_Amount_eop
    
    sum(Reserve_Amount - Reserve_Amount_eop)
    
    # Mortality Savings
    
    Profit_Simulations_T20[n,i] <- sum(Profit_Array[,,,,T20,,])
    Profit_Simulations_SPWL[n,i] <- sum(Profit_Array[,,,,SPWL,,])
    Profit_Simulations[n,i] <- sum(Profit_Array[,,,,,,])
  }
}




ggplot() +
  geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 1.05, xmax = 1.05000001, fill = "Baseline Model"))+
  geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 1.05, xmax = 1.05000001, fill = "+- 10% Mortality Adjustment"))+
  geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 1.05, xmax = 1.05000001, fill = "+- 20% Mortality Adjustment"))+
  geom_line(mapping=aes(x = c(0:20)+2023, y = Profit_Simulations[1,]), col = "#0676C6", lwd = 1.5, lty = "dashed")+
  geom_line(mapping=aes(x = c(0:20)+2023, y = Profit_Simulations[2,]), col = "#129B99", lwd = 1.5, lty = "dashed")+
  geom_line(mapping=aes(x = c(0:20)+2023, y = Profit_Simulations[3,]), col = "black", lwd = 1.5)+
  geom_line(mapping=aes(x = c(0:20)+2023, y = Profit_Simulations[4,]), col = "#129B99", lwd = 1.5, lty = "dashed")+
  geom_line(mapping=aes(x = c(0:20)+2023, y = Profit_Simulations[5,]), col = "#0676C6", lwd = 1.5, lty = "dashed") +
  labs(title = "Sensitivity of baseline mortality assumptions",
       x = "Projection Time",
       y = "Profit (Crowns)") +
  scale_fill_manual(name = "Percentile Range",
                    values = c("Baseline Model" =  "black",
                               "+- 10% Mortality Adjustment" = "#129B99", "+- 20% Mortality Adjustment" = "#0676C6"),
                    labels = c("+-20% Mortality Adjustment", "+- 10% Mortality Adjustment","Baseline Model")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box.just = "center",
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(color = "black")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous("Projection Time", expand = c(0,0), limits = c(2024,2023+PROJECTION_TIME)) +
  scale_y_continuous("Profit (Crowns)", labels = scales::unit_format(unit = "M", scale  =1e-6))

Profit_Simulations[2,]/Profit_Simulations[3,] -1
Profit_Simulations[1,]/Profit_Simulations[3,] -1

