# ---- Calculating Premiums Based on assumptions


#--------------- Assumptions -----

# Comission Rates

Comission_Initial <- 0.75
Comission_Renewal <- 0.05


# Initial Expenses
Fixed_Initial_Cost <- 0
Initial_Rate <- 0.5

Renewal_Rate <- 0.07

sums_assured <- 100000

Mortality_Cost <- 0.0002*sums_assured
Withdrawl_Cost <- 0.0001*sums_assured
Maturity_Cost <- 0.0001*sums_assured

Profit_Margin <- 0.03

discount_rate <- 0.12
reserve_rate <- 0.04
investment_rate <- 0.05

Age <- 30
mortality_table = read.csv("Case Data/srcsc-2024-lumaria-mortality-table-CLEANED.csv")

# --------- Packages

library(lifecontingencies)

#--------- Life Tables

mortality_rate <- mortality_table$Mortality.Rate

withdrawl_rate <- rep(0.01, 120)


life_table <- probs2lifetable(mortality_rate,
                              radix = 10000,
                              type = "qx")




#mortality_rate <- c(0,1 - life_table@lx[2:111]/life_table@lx[1:110])


# Assume Withdrawl only happens at the end of the policy year
# Noone would pay for a policy and terminate this (unless exceptional circumstances)

withdrawl_table <- probs2lifetable(withdrawl_rate * (1-mortality_rate), 
                                   radix = 10000,
                                   type = "qx")


# Build Multi-Decrement Table

multi_dec_table <- probs2lifetable(mortality_rate + withdrawl_rate * (1-mortality_rate),
                                   radix = 10000,
                                   type = "qx")




#------------- Life Contingent Functions ----

Term_Assurance <- function(age, length, rate) {
  
  if (length == 0) {
    return(0)
  }
  
  Term_Assurance <- 0
  
  for (i in 1:length) {
    
    Term_Assurance <- Term_Assurance + pxt(multi_dec_table, age,i-1)*
      qxt(life_table, age + i-1, 1) *
      (1+rate) ^-i
    
  }
  
  return(Term_Assurance)
}  

Term_Annuity <- function(age, length, rate) {
  
  if(length == 0) {
    return(0)
  }
  
  Term_Annuity <- 0
  
  for (i in 1:length) {
    
    Term_Annuity <- Term_Annuity + pxt(multi_dec_table, age,i-1)*
      (1+rate) ^-(i-1)
    
  }
  
  return(Term_Annuity)  
}

Withdrawl_Assurance <- function(age, length, rate) {
  Lapse_Decrement <- 0
  
  for (i in 1:(length-1)) {
    
    Lapse_Decrement <- Lapse_Decrement + pxt(multi_dec_table, age,i-1)*
      qxt(withdrawl_table, age+ i-1, 1) *
      (1+rate) ^-i
    
  }
  
  return(Lapse_Decrement)
}

#------------ Premium and Reserving Calculation

# Setting the premium under a specific profit margin


Cash_Flow_Calculation <- function(Premium, age, length) {

  Profit_Vector <- rep(0,length)
  
  for(i in 1:length) {
    
    # Calculate start of year reserves
    
    Net_Premium <- sums_assured * Term_Assurance(age,length,reserve_rate) / Term_Annuity(age,length, reserve_rate)
    
    Prospective_Reserve_sop <- sums_assured * Term_Assurance(age+(i-1),length-(i-1), reserve_rate) - 
      Net_Premium * Term_Annuity(age+(i-1), length-(i-1), reserve_rate)
    
    
    # Calculate Premium - start of year expenses
    
    Comission_Expense <- ifelse(i == 1, (Comission_Initial - Comission_Renewal) * Premium, 
                                Comission_Renewal * Premium)
    
    Operational_Expense <- ifelse(i == 1, Fixed_Initial_Cost +
                                    Initial_Rate * Premium, Renewal_Rate * Premium)
    
    Net_Inflow <- Premium - Comission_Expense - Operational_Expense
    
    # Calculate Investment Interest based on reserves and Net Start of year Inflow
    
    Interest <- (Net_Inflow + Prospective_Reserve_sop) * investment_rate
    
    Mortality_Expense <- (Mortality_Cost + sums_assured) * qxt(life_table, age+i-1, 1)
    
    Lapse_Expense <- ifelse(i == length, Maturity_Cost * (1 -qxt(life_table, age+i-1, 1)),
                        Withdrawl_Cost * qxt(withdrawl_table, age +i - 1,1))
      
    Prospective_Reserve_eop <- (sums_assured * Term_Assurance(age+i,length-i, reserve_rate) - 
      Net_Premium * Term_Annuity(age+i, length-i, reserve_rate)) * pxt(multi_dec_table, age+i-1,1)
      
    Profit_Vector[i] <- Prospective_Reserve_sop + Net_Inflow + Interest - 
        Mortality_Expense - Lapse_Expense - Prospective_Reserve_eop
    
  }
  
  
  Inforce_Probability <- mapply(FUN = function(x){return(pxt(multi_dec_table,age, x))}, 
    x = c(0:(length-1)))
  
  Discount_factor <- mapply(FUN = function(x) {return((1+discount_rate)^(-x))}, 
                            x = c(1:(length)))  
    
  Profit_Signature <- Profit_Vector * Inforce_Probability * Discount_factor  
    
  EPV_Premiums <- Premium * Inforce_Probability * c(1, Discount_factor[1:(length-1)])  
      
  # Return the profit margin!
  
  return(sum(Profit_Signature) / sum(EPV_Premiums) - Profit_Margin)
  
}


start_time <- Sys.time()
Gross_Premium <- uniroot(Cash_Flow_Calculation,c(10,1000), 
                         age = 30, 
                         length = 20, 
                         tol = 1e-4)$root

end_time <- Sys.time()
end_time - start_time
Gross_Premium

