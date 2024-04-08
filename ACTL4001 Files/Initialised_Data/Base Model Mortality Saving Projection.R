
# Change depending if STOCHASTIC or Non-STOCHASTIC run

#Base run is STOCHASTIC 0 or off
STOCHASTIC <- 0
NUMBER_OF_SIMULATIONS <- 1


#--------------- Past Mortality Savings

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

Policyholders_Joining <- array(unlist(fread("~/ACTL4001/Initialised_Data/Starting_Policyholders.csv")), 
                               dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                       POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))



Commission_Initial <- array(unlist(fread("~/ACTL4001/Initialised_Data/Initial_Commission.csv")), 
                            dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                    POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Commission_Renewal <- array(unlist(fread("~/ACTL4001/Initialised_Data/Renewal_Commission.csv")),
                            dim = c(COHORTS, MAX_AGE, PROJECTION_TIME+1, GENDER, SMOKING_TYPES,
                                    POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

# Initial Expenses
Fixed_Initial_Cost <- array(unlist(fread("~/ACTL4001/Initialised_Data/Fixed_Initial_Cost.csv")),
                            dim = c(COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
                                    POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))


Yearly_Cost <- array(unlist(fread("~/ACTL4001/Initialised_Data/Yearly_Cost.csv")),
                     dim = c(COHORTS, MAX_AGE, PROJECTION_TIME+1, GENDER, SMOKING_TYPES,
                             POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Mortality_Cost <- array(unlist(fread("~/ACTL4001/Initialised_Data/Mortality_Cost.csv")),
                        dim = c(COHORTS, MAX_AGE, PROJECTION_TIME+1, GENDER, SMOKING_TYPES,
                                POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Reserves <- array(unlist(fread("~/ACTL4001/Initialised_Data/Current_Reserves.csv")), 
                         dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Reserves_eop <- array(unlist(fread("~/ACTL4001/Initialised_Data/Current_Reserves_eop.csv")), 
                      dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Premium <- array(unlist(fread("~/ACTL4001/Initialised_Data/Premium.csv")), 
                        dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                               POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

Deaths<- array(unlist(fread("~/ACTL4001/Initialised_Data/Deaths_Policyholders.csv")), 
                 dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                         POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))

# Rates <- fread("~/ACTL4001/Case Data/srcsc-2024-lumaria-economic-data-CLEANED.csv")
# 
# Inflation_Rate <- 1+ as.numeric(sub("%", "",Rates$Inflation[42:62] )) / 100
# 
# Investment_Rate <- as.numeric(sub("%", "",Rates$`10-yr Risk Free Annual Spot Rate`[42:62] )) / 100
# 
# Investment_Rate <- array(Investment_Rate, dim = c(PROJECTION_TIME+1, COHORTS, MAX_AGE, GENDER, SMOKING_TYPES,
#                                                   POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL) )
# 
# Investment_Rate <- aperm(Investment_Rate, c(2,3,1,4,5,6,7,8))
# 
# Mortality_Rates <- fread("~/ACTL4001/Initialised_Data/comparison_life_table_clean.csv")
# 
# Mortality_Rates <- Mortality_Rates[1:(nrow(Mortality_Rates)-1),]
# 
mortality_base <- array(c(0), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, POLICY_TYPE, COHORTS, SUM_ASSURED, 
                                       DISTRIBUTION_CHANNEL, PROJECTION_TIME + 1))

mortality_reduction <- read.csv("Intervention Mortality Reduction.csv")
cost_amount <- read.csv("Intervention Average Cost Amount.csv")

# Initialise the historical mortality rates

# Initialise the groupings

mortality_base[Mortality_Rates$x,MALE,NON_SMOKER,,,,,] <- Mortality_Rates$historical_male_NS_qx
mortality_base[Mortality_Rates$x,MALE,SMOKER,,,,,] <- Mortality_Rates$historical_male_S_qx
mortality_base[Mortality_Rates$x,FEMALE,NON_SMOKER,,,,,] <- Mortality_Rates$historical_female_NS_qx
mortality_base[Mortality_Rates$x,FEMALE,SMOKER,,,,,] <- Mortality_Rates$historical_female_S_qx

#------- Main Loop Code ----

Profit_Simulations <- matrix(c(0), nrow = NUMBER_OF_SIMULATIONS, ncol = PROJECTION_TIME)

for (n in 1:NUMBER_OF_SIMULATIONS) {
  
  Policyholders_Mortality <- array(c(0), dim = c(PROJECTION_TIME + 1, MAX_AGE, GENDER, SMOKING_TYPES,
                                                 POLICY_TYPE, COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL))
  
  Policyholders_Withdrawl <- array(c(0), dim = c(PROJECTION_TIME + 1, MAX_AGE, GENDER, SMOKING_TYPES,
                                                 POLICY_TYPE, COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL))
  
  Policyholders_Current <- array(c(0), dim = c(COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                               POLICY_TYPE, SUM_ASSURED, DISTRIBUTION_CHANNEL))
  
  
  # Initialise the current participants (at 2003)
  
  Policyholders_Current[1,3:MAX_AGE,1,,,,,] <- Policyholders_Joining[1,1:(MAX_AGE-2),,,,,]  
  Policyholders_Current[2,2:MAX_AGE,1,,,,,] <- Policyholders_Joining[1,1:(MAX_AGE-1),,,,,]  
  Policyholders_Current[3,,1,,,,,] <- Policyholders_Joining[1,,,,,,]
  
  # Withdrawing assumptions for different ages, gender etc. for term insurance
  
  reduction_matrix <- array(c(mortality_reduction$V1, mortality_reduction$V2),
                            dim = c(MAX_AGE, SMOKING_TYPES, GENDER, POLICY_TYPE, COHORTS, SUM_ASSURED, 
                                    DISTRIBUTION_CHANNEL, PROJECTION_TIME + 1))
  reduction_matrix <- aperm(reduction_matrix, c(1,3,2,4:8))                          
  
  withdraw_matrix_IND <- array(c(0.01), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL, PROJECTION_TIME + 1))
  
  mortality_matrix_IND <- mortality_base * (1-reduction_matrix)
  
  # Make the rates dependent under the UDD assumption (Withdrawls assumed at end of period)
  
  mortality_matrix <- mortality_matrix_IND
  
  withdraw_matrix <- withdraw_matrix_IND * (1 - mortality_matrix_IND[,,,T20,,,,])
  
  
  
  
  for (j in 1: (PROJECTION_TIME + 1)) {
    
    # Start by moving policy holders down their respective age for new projection
    # Move the first 1:j+21 cohorts (i.e at 2024, this would represent moving up to 2023 down)
    
    if (j != 1) {
    
      Policyholders_Current[1:(j+1),2:MAX_AGE,j,,,,,] <- Policyholders_Current[1:(j+1),1:(MAX_AGE-1),j-1,,,,,]  
    
      # Build the new cohort for example (lets just put a dummy of 1 person per cohort)
    
      Policyholders_Current[j+2,,j,,,,,] <- Policyholders_Joining[j+2,,,,,,]
    
    }
    # ---- Apply Smoking Transitions Initially ----
    
    # Turn the lifes alive matrix into format
    # Projection Time, Smoking Status, Ages, gender, Policy Type, Cohorts, Sum Assured
    
    Policyholders_Current <- aperm(Policyholders_Current, c(3,5,2,4,6,1,7,8))
    
    # Build a smoking transition assumption matrix between each category
    
    transition_matrix <- array(c(0.0001, 0.01), dim = c(SMOKING_TYPES, MAX_AGE, GENDER, POLICY_TYPE, COHORTS, SUM_ASSURED, DISTRIBUTION_CHANNEL))
    
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
  
  #----- Calculating Mortality Savings
  
  # Initial Expenses

  Face_Values <- array(Sum_Assured, dim = c(SUM_ASSURED, COHORTS, MAX_AGE, PROJECTION_TIME + 1, GENDER, SMOKING_TYPES,
                                            POLICY_TYPE,DISTRIBUTION_CHANNEL))

  Face_Values <- aperm(Face_Values, c(2,3,4,5,6,7,1,8))

  Policyholders_Mortality <- aperm(Policyholders_Mortality, c(6,2,1,3,4,5,7,8))

  Policyholders_Withdrawl <- aperm(Policyholders_Withdrawl, c(6,2,1,3,4,5,7,8))

  # All the policyholders at the start of the period
  
  # Premium_Income <- Premium * (Policyholders_Current+ Policyholders_Mortality + Policyholders_Withdrawl)
  # 
  # Initial_Expenses <- (Fixed_Initial_Cost + Commission_Initial * Premium[,,1,,,,,]) * Policyholders_Joining
  # 
  # Duration_Expense <- (Commission_Renewal * Premium + Yearly_Cost) * 
  #   (Policyholders_Current + Policyholders_Mortality + Policyholders_Withdrawl)
  #

  Death_Costs <- (Face_Values) * Policyholders_Mortality
  
  Initiative_Costs <- array(c(cost_amount$V1, cost_amount$V2), dim = c(MAX_AGE, SMOKING_TYPES,COHORTS,
                                                                       GENDER, PROJECTION_TIME+1, POLICY_TYPE,SUM_ASSURED, DISTRIBUTION_CHANNEL))
  
  Initiative_Costs <- aperm(Initiative_Costs, c(3,1,5,4,2,6:8))
  
  Initiative_Costs <- Initiative_Costs * (Policyholders_Current+Policyholders_Mortality+Policyholders_Withdrawl)
  
  # Reserve_Amount <- Reserves * (Policyholders_Current + Policyholders_Mortality + Policyholders_Withdrawl)
  # 
  # Net_Inflow <- Premium_Income - Duration_Expense
  # 
  # for (i in 1:(PROJECTION_TIME+1)) {
  #   
  #   Net_Inflow[i+2,,i,,,,,] <- Net_Inflow[i+2,,i,,,,,] - Initial_Expenses[i+2,,,,,,] +
  #     Policyholders_Joining[i+2,,,,,,] * (Commission_Renewal[i+2,,i,,,,,] * Premium[i+2, ,i,,,,,] + 
  #                                           Yearly_Cost[i+2,,i,,,,,])
  # 
  #}
  
  #Interest <- (Reserve_Amount + Net_Inflow) * Investment_Rate
  
  #Reserve_Amount_eop <- Reserves_eop * (Policyholders_Current)
  

  Profit_Array <- Death_Costs[,,2:(PROJECTION_TIME+1),,,,,] +Initiative_Costs[,,(2:(PROJECTION_TIME+1)),,,,,]
  Death_Count <- Policyholders_Mortality[,,2:(PROJECTION_TIME+1),,,,,]
  
  Profit_Array
  # Profit_Array <- Reserve_Amount[,,2:(PROJECTION_TIME+1),,,,,] + Net_Inflow[,,2:(PROJECTION_TIME+1),,,,,] +
  #   Interest[,,2:(PROJECTION_TIME+1),,,,,] - Death_Costs[,,2:(PROJECTION_TIME+1),,,,,] - Reserve_Amount_eop[,,2:(PROJECTION_TIME+1),,,,,]

  # Mortality Savings
  
  #Profit_Simulations[n, ]<- apply(Profit_Array, MARGIN = c(3), FUN = sum) #*
    #mapply(FUN = function(x) {return((1+discount_rate)^(-x))}, x = c(1:PROJECTION_TIME))

}  




sum(Reserve_Amount[,,,,,T20,,] - Reserve_Amount_eop[,,,,,T20,,] + Premium_Income[,,,,,T20,,]
    - Death_Costs[,,,,,T20,,])/10^9

sum(Death_Costs[,,,,,T20,,])/10^9
sum(Premium_Income[,,,,,T20,,])/10^9

x <- as.data.frame(Premium[1,,,,,T20,,])



x <- apply(Profit_Array[,,2:PROJECTION_TIME,,,SPWL,,], MARGIN = 3, FUN = sum)/10^9
y <- apply(Premium_Income[,,2:PROJECTION_TIME,,,SPWL,,], MARGIN = 3, FUN = sum)/10^9
apply(Death_Costs[,,2:PROJECTION_TIME,,,,,], MARGIN = 3, FUN = sum)/10^9

apply(Reserve_Amount_eop[,,2:PROJECTION_TIME,,,SPWL,,], MARGIN = 3, FUN = sum)/10^9
x/y

dies <- sum(Death_Costs)/10^9
inflow <- sum(Net_Inflow)/10^9

Inflation_Rate_forward <- rev(Inflation_Rate)
Profit_Simulations/10^9
Profit_Simulations <- Profit_Simulations * rev(cumprod(Inflation_Rate_forward)[1:PROJECTION_TIME]) / Inflation_Rate_forward[1]
Profit_Simulations[1,]/rev(cumprod(Inflation_Rate_forward)[1:PROJECTION_TIME]) * Inflation_Rate_forward[1]
# Compute quantiles for each column


base_deaths <- apply(Death_Count, 3, sum)
base_cost <- apply(Profit_Array, 3, sum) * apply(matrix(c(19:0), ncol = 20),2, function(x){(1+0.05)^x})

post_death <- apply(Death_Count, 3, sum)
base_cost


Profit_Array <- apply(Profit_Array, 3, sum) * apply(matrix(c(19:0), ncol = 20),2, function(x){(1+0.05)^x})

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



sum(post_death - deaths)

# Plot
ggplot() +
  geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 2005.01, xmax = 2005.02, fill = "Pre Initiative"))+
  geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 2005.01, xmax = 2005.02, fill = "Post Initiative"))+
  #geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 1.05, xmax = 1.05000001, fill = "Mean"))+
  # geom_ribbon(aes(ymin = P5, ymax = P25, fill = "5th - 95th Percentiles"), alpha = 0.5) +
  # geom_ribbon(aes(ymin = P25, ymax = P75, fill = "25th - 75th Percentiles"), alpha = 0.5) +
  # geom_ribbon(aes(ymin = P75, ymax = P95, fill = "5th - 95th Percentiles"), alpha = 0.5) +
  geom_line(mapping = aes(x = c(2004:2023), y = deaths), col = "#044B7F", lty ="dashed", lwd = 1.5) +
  geom_line(mapping = aes(x = c(2004:2023), y = post_death), col = "#25A0F8", lwd = 1.5) +
  labs(title = "Superlife historical mortality projections",
       x = "Projection Time",
       y = "Deaths") +
  scale_fill_manual(name = "Percentile Range",
                    values = c("Pre Initiative" = "#044B7F", "Post Initiative" = "#25A0F8"),
                    labels = c("Post Initiaitive", "Pre Initiative","Mean")) + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box.just = "center",
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(color = "black")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous("Projection Time", expand = c(0,0))

#-------------------------------------------------------------------------------------------

ggplot() +
  geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 2005.01, xmax = 2005.02, fill = "Pre Initiative"))+
  geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 2005.01, xmax = 2005.02, fill = "Post Initiative"))+
  # geom_ribbon(aes(ymin = P5, ymax = P25, fill = "5th - 95th Percentiles"), alpha = 0.5) +
  # geom_ribbon(aes(ymin = P25, ymax = P75, fill = "25th - 75th Percentiles"), alpha = 0.5) +
  # geom_ribbon(aes(ymin = P75, ymax = P95, fill = "5th - 95th Percentiles"), alpha = 0.5) +
  geom_line(mapping = aes(x = c(2004:2023), y = base_cost), col = "#044B7F", lty = 'dashed', lwd = 1.5) +
  geom_line(mapping = aes(x = c(2004:2023), y = Profit_Array), col = "#25A0F8", lwd = 1.5) +
  labs(title = "Superlife historical mortality cost savings",
       x = "Projection Time",
       y = "Mortality Costs") +
  scale_fill_manual(name = "",values = c("Pre Initiative" = "#044B7F", "Post Initiative" = "#25A0F8"),
                    labels = c("Post Initiaitive", "Pre Initiative","Mean")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box.just = "center",
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(color = "black")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous("Projection Time", expand = c(0,0)) +
  scale_y_continuous("Mortality Costs", labels = scales::unit_format(unit = "B", scale = 1e-9))

(base_cost- Profit_Array)





