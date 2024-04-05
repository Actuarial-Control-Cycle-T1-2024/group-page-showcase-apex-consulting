# --- Building for Current Policyholders

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



discount_rate <- 0.12
reserve_rate <- 0.04
investment_rate <- 0.05

# ---- Data / Package Read In

library("ggplot2")
library("dplyr")
library("data.table")

#---------------- Array Read In

Policyholders_Joining <- array(unlist(fread("~/ACTL4001/Initialised_Data/Starting_Policyholders.csv")), 
                               dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                 POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))


Commission_Initial <- array(read.csv("~/ACTL4001/Initialised_Data/Initial_Commission.csv"), 
                        dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Commission_Renewal <- array(read.csv("~/ACTL4001/Initialised_Data/Renewal_Commission.csv"),
                            dim = c(COHORTS, MAX_AGE, PROJECTION_TIME+1, GENDER, SMOKING_TYPES,
                                          POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

# Initial Expenses
Fixed_Initial_Cost <- array(read.csv("~/ACTL4001/Initialised_Data/Fixed_Initial_Cost.csv"),
                            dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                      POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))


Yearly_Cost <- array(read.csv("~/ACTL4001/Initialised_Data/Yearly_Cost.csv"),
                     dim = c(COHORTS, MAX_AGE, PROJECTION_TIME+1, GENDER, SMOKING_TYPES,
                             POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Mortality_Cost <- array(read.csv("~/ACTL4001/Intialised_Data/Mortality_Cost.csv"),
                        dim = c(COHORTS, MAX_AGE, PROJECTION_TIME+1, GENDER, SMOKING_TYPES,
                                 POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Investment_Rate <- matrix(read.csv("~/ACTL4001/Initialised_Data/Investment_Rate.csv"), ncol = 30)
Inflation_Rate <- matrix(read.csv("~/ACTL4001/Initialised_Data/Inflation_Rates.csv"))


#------- Main Loop Code ----

Profit_Simulations <- matrix(c(0), nrow = NUMBER_OF_SIMULATIONS, ncol = PROJECTION_TIME)

for (n in 1:NUMBER_OF_SIMULATIONS) {

  Policyholders_Mortality <- array(c(0), dim = c(PROJECTION_TIME + 1, MAX_AGE, GENDER, SMOKING_TYPES,
                                                 POLICY_TYPE, COHORTS, SUM_ASSURED))
  
  Policyholders_Withdrawl <- array(c(0), dim = c(PROJECTION_TIME + 1, MAX_AGE, GENDER, SMOKING_TYPES,
                                                 POLICY_TYPE, COHORTS, SUM_ASSURED))
  
  Policyholders_Current <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                               POLICY_TYPE, SUM_ASSURED))
  
  
  # Initialise the current participants (at 2003)
  
  Policyholders_Current[1,3:MAX_AGE,1,,,,,] <- Policyholders_Joining[1,1:(MAX_AGE-2),,,,,]  
  Policyholders_Current[2,2:MAX_AGE,1,,,,,] <- Policyholders_Joining[1,1:(MAX_AGE-1),,,,,]  
  Policyholders_Current[3,,1,,,,,] <- Policyholders_Joining[1,,,,,,]
  
  # Withdrawing assumptions for different ages, gender etc. for term insurance
  
  withdraw_matrix_IND <- array(c(0.01), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL, PROJECTION_TIME + 1))
  
  mortality_matrix_IND <- array(c(0.01), dim = c(COHORTS, MAX_AGE,PROJECTION_TIME + 1, GENDER, SMOKING_TYPES, 
                                                 POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL)) * rnorm(1, mean = 1, sd = 0.3)
  
  mortality_matrix_IND <- aperm(mortality_matrix_IND, c(2,4,5,6,1,7,8,3) )
  
  # Make the rates dependent under the UDD assumption
  
  mortality_matrix <- array(c(0), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, POLICY_TYPE, COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL ,PROJECTION_TIME +1))
  
  withdraw_matrix <- withdraw_matrix_IND * (1 - 0.5 * mortality_matrix_IND[,,,T20,,,])
  
  mortality_matrix[,,,T20,,,,] <- mortality_matrix_IND[,,,T20,,,,] * (1 - 0.5 * withdraw_matrix_IND)
  
  mortality_matrix[,,,SPWL,,,,] <- mortality_matrix_IND[,,,SPWL,,,,]
  
  for (j in 2: (PROJECTION_TIME + 1)) {
    
    # Start by moving policy holders down their respective age for new projection
    # Move the first 1:j+21 cohorts (i.e at 2024, this would represent moving up to 2023 down)
    
    Policyholders_Current[1:(j+1),2:MAX_AGE,j,,,,,] <- Policyholders_Current[1:(j+1),1:(MAX_AGE-1),j-1,,,,,]  
    
    # Build the new cohort for example (lets just put a dummy of 1 person per cohort)
    
    Policyholders_Current[j+2,,j,,,,,] <- Policyholders_Joining[j+2,,,,,,]
    
    # ---- Apply Smoking Transitions Initially ----
    
    # Turn the lifes alive matrix into format
    # Projection Time, Smoking Status, Ages, gender, Policy Type, Cohorts, Sum Assured
    
    Policyholders_Current <- aperm(Policyholders_Current, c(3,5,2,4,6,1,7,8))
    
    # Build a smoking transition assumption matrix between each category
    
    transition_matrix <- array(c(0.0001, 0.02), dim = c(SMOKING_TYPES, MAX_AGE, GENDER, POLICY_TYPE, COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL))
    
    # Take the current makeup of population to account for transitions into smoking etc.
    
    Current_makeup <- Policyholders_Current[j,,,,,,,]
    
    # Adjust the non-smoker population (Equals number of people staying in non-smoking + Number of smokers becoming non-smokers)
    
    Policyholders_Current[j,NON_SMOKER,,,,,,] <- Current_makeup[NON_SMOKER,,,,,,] * (1-transition_matrix[NON_SMOKER,,,,,,]) +
      Current_makeup[SMOKER,,,,,,] * transition_matrix[SMOKER,,,,,,]
    
    # Adjust the smoker population (Equals number of people staying in smoking + Number of non-smokers becoming smokers)
    
    
    Policyholders_Current[j,SMOKER,,,,,,] <- Current_makeup[NON_SMOKER,,,,,,] * transition_matrix[NON_SMOKER,,,,,,] +
      Current_makeup[SMOKER,,,,,,] * (1-transition_matrix[SMOKER,,,,,,])
    
    # Turn the lifes alive matrix into format (number represents previous dimension)
    # Cohorts (6), Ages (3), Projection Time(1), gender(4), smoking status(2), policy type(5), Sum Assured(7)
    
    Policyholders_Current <- aperm(Policyholders_Current, c(6,3,1,4,2,5,7,8))
    
    # ---- Apply Mortality ----
    
    # Rearrange matrix (Projection Time, Age, Gender, Smoking, Policy, Cohorts, Sum Assured)
    
    Policyholders_Current <- aperm(Policyholders_Current, c(3,2,4,5,6,1,7,8))
    
    # Apply mortality assumptions (i.e. seperate assumptions for age, gender, smoking and policy)
    
    Policyholders_Mortality[j,,,,,,,] <- mortality_matrix[,,,,,,,j] * Policyholders_Current[j,,,,,,,]
    
    Policyholders_Current[j,,,,,,,] <- (1- mortality_matrix[,,,,,,,j])* Policyholders_Current[j,,,,,,,]
    
    # ---- Apply Withdraw ----
    
    # Keep track of participants who are withdrawn
    
    Policyholders_Withdrawl[j,,,,T20,,,] <- withdraw_matrix[,,,,,,j]* Policyholders_Current[j,,,,T20,,,]
    
    # Remove Participants who are withdrawn
    
    Policyholders_Current[j,,,,T20,,,] <- (1-withdraw_matrix[,,,,,,j])* Policyholders_Current[j,,,,T20,,,]
    
    
    # --- Apply Maturity Removal ----
    
    # In here we would need to loop through cohorts i.e. if the year of projection - cohort
    
    # i.e born in 2024, year of projection 21, 23 - 24 + 21 = 20
    
    # Put all the policies in the withdraw status as required
    
    if(j +2003 - 2020 > 0) {
      
      Policyholders_Withdrawl[j,,,,T20,j+2003-2020,,] <- Policyholders_Current[j,,1,1,T20,j+2003-2020,,]
    
      # Set lives in policy to 0 (at end fo the term)
      
      Policyholders_Current[j,,,,T20,j+2003-2020,, ] <- 0
      
    }
    
    # Rearrange matrix back to standard form:
    # Cohorts (6), Age (2), Projection Time (1), Gender(3), Smoking (4), Policy (5), Withdrawn (7)
    #(Projection Time, Age, Gender, Smoking, Policy, Cohorts, Sum Assured)
    
    Policyholders_Current <- aperm(Policyholders_Current, c(6,2,1,3,4,5,7,8))
    
  }
  
  # ---- Expense Calculation
  
  #--------------- List Expenses to Account for 
  
  # Fixed_Commission Expense * Policyholders Joining Term Program # Initial
  
  # Premium Commission -> Policyholders_Current * Premium * Commission Expense # Duration of Term
  
  # Operational Expense  -> Initial Expense * Policyholders Joining then resort out # Initial
  # Renewal Operational Expense - Renwal Expense * Premium * Policyholders_Current # Duration of Term
  
  # Mortality Expense # Based on Deaths
  # Number of Deaths at each time point * Mortality Cost Amount
  
  # Withdrawl/Maturity Expense -> Assume same amount for ease # Based on Withdrawls
  # Number of Withdrawls at each time point * Withdrawl Cost Amount
  
  # Capital Requirements Per Policyholder Per Death Benefit # Based on Policyholders Alive
  # Policyholder_Current * Sums_Assured Matrix * Required Capital Matrix
  
  # Reserve Amount 
  # Prospective Reserve Matrix * Policyholders_Current
  
  #----- Calculating Expenses
  
  # Initial Expenses
  
  Premium <- array(c(1), dim = dim(Policyholders_Current))
  Prospective_Reserve <- array(c(1), dim = dim(Policyholders_Current))
  
  Face_Values <- array(Sum_Assured, dim = c(SUM_ASSURED, COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                            POLICY_TYPE,DISTRIBUTION_CHANNEL))
  
  Face_Values <- aperm(Face_Values, c(2,3,4,5,6,7,1,8))
  
  Policyholders_Mortality <- aperm(Policyholders_Mortality, c(6,2,1,3,4,5,7,8))
  
  Policyholders_Withdrawl <- aperm(Policyholders_Withdrawl, c(6,2,1,3,4,5,7,8))
  
  Premium_Income <- Premium * Policyholders_Current
  
  Initial_Expenses <- (Fixed_Initial_Cost + (Commission_Initial - Commission_Renewal) * Premium[,,1,,,,,] +
                         (Initial_Rate - Renewal_Rate)*Premium[,,1,,,,,]) * Policyholders_Joining
  
  Duration_Expense <- (Commission_Renewal * Premium + Renewal_Rate * Premium) * Policyholders_Current
  
  Death_Costs <- (Face_Values + Mortality_Cost) * Policyholders_Mortality
  
  Reserve_Amount <- Reserves * Policyholders_Current
  
  Net_Inflow <- Premium_Income - Duration_Expense
  
  for (i in 2:(PROJECTION_TIME+1)) {
   
    Net_Inflow[i+2,,i-1,,,,,] <- Net_Inflow[i+2,,i-1,,,,,] - Initial_Expenses[i+2,,,,,,] + 
      Premium[i+2,,i,,,,,] * Policyholders_Joining[i+2,,,,,,]
    
  }
  
  Interest <- (Net_Inflow + Reserve_Amount) * Investment_Rate[5,1:PROJECTION_TIME+1]
  
  Reserve_Amount_Eop <- Reserves[,,(2:PROJECTION_TIME+1),,] * Policyholders_Current
  
  Profit_Array <- Reserve_Amount[,,1:(PROJECTION_TIME),,,,,] + Net_Inflow[,,1:(PROJECTION_TIME),,,,,] +
    Interest[,,1:(PROJECTION_TIME),,,,,] - Death_Costs[,,2:(PROJECTION_TIME+1),,,,,] - 
    Lapse_Costs[,,2:(PROJECTION_TIME+1),,,,,] - Prospective_Reserve[,,2:(PROJECTION_TIME+1),,,,,]
  
  Profit_Simulations[n, ]<- apply(Profit_Array, MARGIN = c(3), FUN = sum) *
    mapply(FUN = function(x) {return((1+discount_rate)^(-x))}, x = c(1:PROJECTION_TIME))

}  
  

# Compute quantiles for each column
quantiles <- apply(Profit_Simulations, 2, quantile, probs = c(0.05,0.25, 0.5, 0.75, 0.95))

average_profit <- apply(Profit_Simulations, 2, mean)

# Create dataframe for ggplot
Profit_Bandings <- data.frame(time = 1:PROJECTION_TIME,
                 Median = quantiles[3, ],
                 P25 = quantiles[2, ],
                 P75 = quantiles[4, ],
                 P5 = quantiles[1, ],
                 P95 = quantiles[5, ],
                 mean = average_profit)


# Plot
ggplot(Profit_Bandings, aes(x = time, y = mean)) +
  geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 1.05, xmax = 1.05000001, fill = "Mean"))+
  geom_ribbon(aes(ymin = P5, ymax = P25, fill = "5th - 95th Percentiles"), alpha = 0.5) +
  geom_ribbon(aes(ymin = P25, ymax = P75, fill = "25th - 75th Percentiles"), alpha = 0.5) +
  geom_ribbon(aes(ymin = P75, ymax = P95, fill = "5th - 95th Percentiles"), alpha = 0.5) +
  geom_line(col = "black", lwd = 1.5) +
  labs(title = "Superlife projected over 20 years",
       x = "Projection Time",
       y = "Profit (Crowns)") +
  scale_fill_manual(name = "Percentile Range",
                    values = c("Mean" = "black", "25th - 75th Percentiles" = "#129B99", "5th - 95th Percentiles" = "#0676C6"),
                    labels = c("25th - 75th Percentiles", "5th - 95th Percentiles","Mean")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box.just = "center",
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(color = "black")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous("Projection Time", expand = c(0,0), limits = c(1,20) )


