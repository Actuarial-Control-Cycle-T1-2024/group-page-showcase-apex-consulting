
# Comission Rates
 
Comission_Initial <- c(0.8,0.45,0)
Comission_Renewal <- c(0.04,0.02,0)

# Initial Expenses

Fixed_Initial_Cost <- c(360,360,360)

Renewal_Cost <- 156

Mortality_Cost <- 720.82

Profit_Margin <- c(0.03, 0.04, 0.05)

discount_rate <- 0.07
reserve_rate <- 0.03
investment_rate <- 0.04

MAX_AGE <- 100
GENDER <- 2
SMOKING_TYPES <- 2
DISTRIBUTION_CHANNEL <- 3
SUM_ASSURED <- 6

inforce_raw = read.csv("~/ACTL4001/Case Data/2024-srcsc-superlife-inforce-dataset.csv")

Sum_Assured <- unique(inforce_raw$Face.amount)

setwd("~//")

mortality_table = read.csv("~/ACTL4001/Case Data/srcsc-2024-lumaria-mortality-table-CLEANED.csv")

Premium_Array <- array(c(0), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, DISTRIBUTION_CHANNEL, SUM_ASSURED))

Sum_Assured <- unique(inforce_raw$Face.amount)

x <- as.data.frame(Premium[1,,,,,T20,,])
x


# --------- Packages

library(lifecontingencies)
library(data.table)
library(dplyr)

#--------- Life Tables

mortality_rate <- fread("~/ACTL4001/Initialised_Data/weighted_qx.csv")

withdrawl_rate <- rep(0.01, 120)

#------------- Life Contingent Functions ----

Term_Assurance <- function(age, length, rate) {
  
  Term_Assurance <- case_when(length == 0 ~ 0,
                              .default = sum(apply(matrix(c(1:length), ncol = 1, nrow = length), MARGIN = 2, function(x){
                                pxt(multi_dec_table, age,x-1)*
                                qxt(life_table, age + x-1, 1) *
                                (1+rate) ^-x
  })))
  
  return(Term_Assurance)
}  

Term_Assurance(35,20,0.04)*100000 / Term_Annuity(35,20,0.04)


Term_Annuity <- function(age, length, rate) {
  
  Term_Annuity <- case_when(length == 0 ~ 0,
                            .default = sum(apply(matrix(c(1:length), ncol = 1, nrow = length), MARGIN = 2, function(x) {
                              pxt(multi_dec_table, age,x-1)*
                                (1+rate) ^-(x-1)
                            } )))
  return(Term_Annuity)  
}

# Withdrawl_Assurance <- function(age, length, rate) {
#   
#   Lapse_Decrement <- case_when(length == 0 ~ 0,
#                                .default = sum(apply(matrix(c(1:(length-1)), nrow = length - 1), MARGIN = 2, function(x) {
#                                  pxt(multi_dec_table, age,x-1)*
#                                    qxt(withdrawl_table, age+x-1, 1) *
#                                    (1+rate) ^-x
#                                })))
#                                
#   return(Lapse_Decrement)
# }


age = 35
length = 20
sums_assured = 100000
Cms_Initial = 0.8
Cms_Renewal = 0.05
Fixed_Initial_Cost = 360.412
Renewal_Cost = 156
Premium = 329
Mortality_Cost = 720

Cash_Flow_Calculation <- function(Premium, age, length, sums_assured,
                                  Cms_Initial, Cms_Renewal,Fixed_Initial_Cost,
                                  Pft_Margin) {
  
  Profit_Vector <- apply(matrix(c(1:length), ncol = 1, nrow = length), MARGIN = 1, FUN = function(i){
    
    # Calculate start of year reserves
    
    Net_Premium <- sums_assured * Term_Assurance(age,length,0.04) / Term_Annuity(age,length, 0.04)
    Net_Premium
    Prospective_Reserve_sop <- sums_assured * Term_Assurance(age+(i-1),length-(i-1), reserve_rate) - 
      Net_Premium * Term_Annuity(age+(i-1), length-(i-1), reserve_rate)
    
    
    # Calculate Premium - start of year expenses
    
    Comission_Expense <- ifelse(i == 1, Cms_Initial * Premium, 
                                Cms_Renewal * Premium)
    
    Operational_Expense <- ifelse(i == 1, Fixed_Initial_Cost, Renewal_Cost)
    
    Net_Inflow <- Premium - Comission_Expense - Operational_Expense
    
    # Calculate Investment Interest based on reserves and Net Start of year Inflow
    
    Interest <- (Net_Inflow + Prospective_Reserve_sop) * investment_rate
    
    Mortality_Expense <- (Mortality_Cost + sums_assured) * qxt(life_table, age + i-1, 1)
    
    # Lapse_Expense <- ifelse(i == length, Maturity_Cost * (1 -qxt(life_table, age+i-1, 1)),
    #                         Withdrawl_Cost * qxt(withdrawl_table, age +i - 1,1))
    
    Prospective_Reserve_eop <- ifelse(i == length, 0, (sums_assured * Term_Assurance(age+i,length-i, reserve_rate) - 
                                                         Net_Premium * Term_Annuity(age+i, length-i, reserve_rate)) * pxt(multi_dec_table, age+i-1,1))
    
    return(Prospective_Reserve_sop + Net_Inflow + Interest - 
             Mortality_Expense - Prospective_Reserve_eop) #- Lapse_Expense)
    
  })
  Profit_Vector
  Inforce_Probability <- mapply(FUN = function(x){return(pxt(multi_dec_table,age, x))}, 
                                x = c(0:(length-1)))
  
  Discount_factor <- mapply(FUN = function(x) {return((1+discount_rate)^(-x))}, 
                            x = c(1:(length)))  
  
  Profit_Signature <- Profit_Vector * Inforce_Probability * Discount_factor  
  
  EPV_Premiums <- Premium * Inforce_Probability * c(1, Discount_factor[1:(length-1)])  
  Profit_Signature/EPV_Premiums
  # Return the profit margin!
  
  return(sum(Profit_Signature) / sum(EPV_Premiums) - Pft_Margin)
  
}

Cash_Flow_Calculation(329, 35, 20, 100000, 0.8, 0.05, 360, 0)

#------------ Premium and Reserving Calculation

# Setting the premium under a specific profit margin
start_time <- Sys.time()

for (j in 1:GENDER) {
  
  for (k in 1:SMOKING_TYPES) {
    
    if (j == 1 & k == 1) {
        mortality_prob <- mortality_rate$weighted_male_NS_qx
    } else if (j == 1 & k == 2) {
      mortality_prob <- mortality_rate$weighted_male_S_qx
    } else if (j == 2 & k == 1) {
      mortality_prob <- mortality_rate$weighted_female_NS_qx
    } else {
      mortality_prob <- mortality_rate$weighted_female_S_qx
    }
      
      life_table <- probs2lifetable(c(0,mortality_prob),
                                    radix = 10000,
                                    type = "qx")
      
      # Assume Withdrawl only happens at the end of the policy year
      # Noone would pay for a policy and terminate this (unless exceptional circumstances)
      
      withdrawl_table <- probs2lifetable(c(0,withdrawl_rate * (1-mortality_prob)), 
                                         radix = 10000,
                                         type = "qx")
      
      
      # Build Multi-Decrement Table
      
      multi_dec_table <- probs2lifetable(c(0, mortality_prob + withdrawl_rate * (1-mortality_prob)),
                                         radix = 10000,
                                         type = "qx")
      
      
    for (i in 25:65) {
      
      for (l in 1:DISTRIBUTION_CHANNEL) {
        
        for (m in 1:SUM_ASSURED) {
          
          Premium_Array[i,j,k,l,m] <- uniroot(Cash_Flow_Calculation,c(100,1000000), 
                                   age = i, 
                                   length = 20, 
                                   sums_assured = Sum_Assured[m],
                                   Cms_Initial = Comission_Initial[l],
                                   Cms_Renewal = Comission_Renewal[l],
                                   Pft_Margin = Profit_Margin[l],
                                   Fixed_Initial_Cost = Fixed_Initial_Cost[l],
                                   tol = 1e-4)$root
        }
      }
    }
  }
}

fwrite(as.data.frame(Premium_Array), "~/ACTL4001/Initialised_Data/Term_Premium_new.csv")


End_time <- Sys.time()
End_time - start_time


