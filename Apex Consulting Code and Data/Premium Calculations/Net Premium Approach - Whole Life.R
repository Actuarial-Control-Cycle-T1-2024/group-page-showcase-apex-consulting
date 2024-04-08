#################################
######## Introduction ###########
#################################

# This code has been to develop the premiums for the whole life contract
# This is using a relatively simplistic pricing approach based on;
# - Scope of data
# - Efficiency of program to calculate by age level.

#---- Data Variables

Profit_Margin <- 0.08

commission_margin <- c(0.05, 0.025, 0)

# Expense margin scaled by sums assured
expense_margin <- c(0.1)
discount_rate <- 0.0475
fixed_expense <- 150
renewal_expesne <- 35
mortality_expense <- 150

Sum_Assured <- unique(inforce_raw$Face.amount)

# --------- Packages

library(lifecontingencies)
library(data.table)
library(dplyr)

#--------- Life Tables

mortality_rate <- fread("~/ACTL4001/Initialised_Data/weighted_qx.csv")

withdrawl_rate <- rep(0, 120)

Premium_Array <- array(c(0), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, DISTRIBUTION_CHANNEL, SUM_ASSURED))

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
          Premium_Array[i,j,k,l,m] <- ((Sum_Assured[m]+mortality_expense)*Term_Assurance(i, 120-i, discount_rate) + (fixed_expense-35)+
                                         renewal_expense * Term_Annuity(i, 120-i, discount_rate)) *
            (1+Profit_Margin)*(1+commission_margin[l])
        }
      }
    }
  }
}

Premium_Array
fwrite(as.data.frame(Premium_Array), "~/ACTL4001/Future_Data/Net_Premium_SPWL.csv")


