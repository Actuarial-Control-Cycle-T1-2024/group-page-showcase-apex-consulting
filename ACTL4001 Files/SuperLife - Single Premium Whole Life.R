
#---- Calculate Single Premium Whole Life

# Comission Rates

Comission_Single <- c(0.05,0.025,0)

# Initial Expenses

Fixed_Initial_Cost <- c(360,360, 360)

Renewal_Cost <- 96

Mortality_Cost <- 420.48

Profit_Margin <- c(0.04, 0.08, 0.1)

discount_rate <- 0.012
reserve_rate <- 0.03
investment_rate <- 0.035

MAX_AGE <- 100
GENDER <- 2
SMOKING_TYPES <- 2
DISTRIBUTION_CHANNEL <- 3
SUM_ASSURED <- 6



setwd("~//")

mortality_table = read.csv("srcsc-2024-lumaria-mortality-table-CLEANED.csv")

inforce_raw <- read.csv("~/ACTL4001/Case Data/2024-srcsc-superlife-inforce-dataset.csv")

Premium_Array <- array(c(0), dim = c(MAX_AGE, GENDER, SMOKING_TYPES, DISTRIBUTION_CHANNEL, SUM_ASSURED))

Sum_Assured <- unique(inforce_raw$Face.amount)

# --------- Packages

library(lifecontingencies)
library(dplyr)
library(data.table)

#--------- Life Tables

mortality_rate <- fread("~/ACTL4001/Initialised_Data/weighted_qx.csv")

withdrawl_rate <- rep(0, 120)

#------------- Life Contingent Functions ----

Term_Assurance <- function(age, length, rate) {
  if (length == 0) return(0)
  
  x <- 1:length
  life_table_values <- qxt(life_table, age + x - 1, 1)
  rate_values <- (1 + rate) ^ -x
  
  sum(pxt(multi_dec_table, age, x - 1) * life_table_values * rate_values)
}

Term_Assurance(35,95,0.03)


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


Cash_Flow_Calculation <- function(Premium, age, length, sums_assured,
                                  Cms_Single, Fixed_Initial_Cost, Pft_Margin) {
  
  Profit_Vector <- apply(matrix(c(1:length), ncol = 1, nrow = length), MARGIN = 1, FUN = function(i){
    
    
    # Calculate start of year reserves
    Net_Premium <- sums_assured * Term_Assurance(age,length,reserve_rate)
    
    Prospective_Reserve_sop <- sums_assured * Term_Assurance(age+(i-1),length-(i-1), reserve_rate) - 
      ifelse(i == 1, Net_Premium, 0)
    
    
    # Calculate Premium - start of year expenses
    
    Comission_Expense <- ifelse(i == 1, Cms_Single * Premium, 0)
    
    Operational_Expense <- ifelse(i == 1, Fixed_Initial_Cost, Renewal_Cost)
    
    Net_Inflow <- ifelse(i == 1, Premium, 0) - Comission_Expense - Operational_Expense
    
    # Calculate Investment Interest based on reserves and Net Start of year Inflow
    
    Interest <- (Net_Inflow + Prospective_Reserve_sop) * investment_rate
    
    Mortality_Expense <- (Mortality_Cost + sums_assured) * qxt(life_table, age + i-1, 1)
    
    # Lapse_Expense <- ifelse(i == length, Maturity_Cost * (1 -qxt(life_table, age+i-1, 1)),
    #                         Withdrawl_Cost * qxt(withdrawl_table, age +i - 1,1))
    
    Prospective_Reserve_eop <- ifelse(i == length, 0, sums_assured * Term_Assurance(age+i,length-i, reserve_rate))
    
    return(Prospective_Reserve_sop + Net_Inflow + Interest - 
             Mortality_Expense - Prospective_Reserve_eop) #- Lapse_Expense)
    
  })
  
  
  Inforce_Probability <- mapply(FUN = function(x){return(pxt(multi_dec_table,age, x))}, 
                                x = c(0:(length-1)))
  
  Discount_factor <- mapply(FUN = function(x) {return((1+discount_rate)^(-x))}, 
                            x = c(1:(length)))  
  
  Profit_Signature <- Profit_Vector * Inforce_Probability * Discount_factor  
  
  EPV_Premiums <- Premium 
  
  # Return the profit margin!
  
  return(sum(Profit_Signature) / sum(EPV_Premiums) - Pft_Margin)
  
}





Test_Premiums <- seq(10000, 2000000, 300)

apply(Test_Premiums, )
Cash_Flow_Calculation()
for (i in Test_Premiums) {
  if(Cash_Flow_Calculation(i, 35, 95, 100000, 0.05,360,0) > 0.03) {
    print(Cash_Flow_Calculation(i, 35, 95, 100000,0.05,360,0))
  }
}

Test_Premiums <- seq(10000, 2000000, 300)

# Apply Cash_Flow_Calculation to all premiums
profits <- sapply(Test_Premiums, function(premium) {
  Cash_Flow_Calculation(premium, 35, 95, 100000, 0.05, 360, 0)
})




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
    
    life_table <- probs2lifetable(mortality_prob,
                                    radix = 10000,
                                    type = "qx")
      
      #mortality_rate <- c(0,1 - life_table@lx[2:111]/life_table@lx[1:110])
      
      
      # Assume Withdrawl only happens at the end of the policy year
      # Noone would pay for a policy and terminate this (unless exceptional circumstances)
      
      withdrawl_table <- probs2lifetable(withdrawl_rate * (1-mortality_prob), 
                                         radix = 10000,
                                         type = "qx")
      
      
      # Build Multi-Decrement Table
      
      multi_dec_table <- probs2lifetable(mortality_prob + withdrawl_rate * (1-mortality_prob),
                                         radix = 10000,
                                         type = "qx")
      
      
      
      
    for (i in 26:65) {
       
      for (l in 1:DISTRIBUTION_CHANNEL) {
        
        for (m in 1:SUM_ASSURED) {
          
          Premium_Array[i,j,k,l,m] <- uniroot(Cash_Flow_Calculation,c(100,10000000), 
                                              age = i, 
                                              length = MAX_AGE-i, 
                                              sums_assured = Sum_Assured[m],
                                              Cms_Single = Comission_Single[l],
                                              Pft_Margin = Profit_Margin[l],
                                              Fixed_Initial_Cost = Fixed_Initial_Cost[l],
                                              tol = 1e-2)$root
        }
      }
    }
  }
}

Cash_Flow_Calculation(24000, 35, 90, 100000, 0.05,
                      360, 0)


fwrite(as.data.frame(Premium_Array), "~/ACTL4001/Initialised_Data/SP_Whole_Life.csv")

