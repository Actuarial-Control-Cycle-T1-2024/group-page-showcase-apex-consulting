#############################
####### Introduction ########
#############################

# Model to calculate the expected impacts of the App
# High level assumption based on limited specific data on creation

#--------------------------------------

# Apply Additional Cost Variations to Combined Cost Data

Profit_Simulations <- fread("~/ACTL4001/Simulations Data/Profit Sims Combined SPWL and T20.csv")

Base_set <- fread("~/ACTL4001/Simulations Data/Base Case Combined SPWL and T20.csv")


library('matrixStats')

# Research cost of healthcare app can vary
# Assuming a combination of features
# -  https://radixweb.com/blog/app-development-cost
# - https://www.linkedin.com/pulse/how-much-does-cost-develop-app-australia-spritely-app-development/
# - https://www.linkedin.com/pulse/healthcare-app-development-cost-how-get-right-estimate/
# - https://www.appschopper.com/blog/healthcare-app-development-cost/


# From the above set of articles assume for each different group
# Overall Cost of Overall Implementation (Including Reward System) 100,000 - 500,000 (USD) or 175000 - 875000
# Prevention Component 100,000 - 300,000 component or 175,000 to 500,000
# Fitness Component 15,000 - 50,000+ or rather  30,000 to 200,000
# Wellbeing Component 30,000 - 120,000 (USD) or 50,000 - 250,000
# Lifestyle and Community - 75,000 - 250,000 (USD) or 150,000 - 400,000

# Hence choose max cost as approximately $2.5m in first year
# Choose a min cost of 600,000
# Assume a conservative $2m estimate (Based on relative uncertainty)
# Costs will likely 

# Continued cost of the app
# https://gooapps.net/2024/01/31/how-much-does-it-cost-to-maintain-an-app-in-2024/#:~:text=Considering%20all%20the%20above%2C%20maintaining,type%20of%20maintenance%20service%20contracted.
# https://imaginovation.net/blog/importance-mobile-app-maintenance-cost/

# In the short to medium term most renewal costs appear to be in the vicinity of 20% of initial development
# Assume 20% level reduction over the first four implementation Years

# Build a Log-Normal Distribution of $2.5m - X
# X ~ LogNorm (mu, sig), where E[X] = $500k, and 97.5th percentile is $2.5m - 600k or $1.9m
# Place $2m as the forced limit after calculating parameters


Dist_Mean <- 500*10^3
Upper_Bound <- 1.9*10^6

Calculate_sig <- function(sig, average_cost, upper_bound) {
  
  mu = log(average_cost) - sig^2/2
  
  upper_bound = Upper_Bound
  
  return(plnorm(upper_bound, mu, sig) - 0.975)
  
  
}


Dist_sig <- uniroot(Calculate_sig, interval = c(0.5,2), average_cost = Dist_Mean,
                    upper_bound = Upper_Bound)$root

Dist_mu <- log(Dist_Mean) - Dist_sig^2/2


Trunc_lnorm <- function(){
  
  x <- 2.5*10^6

  while (x > 2*10^6) {
    x <- rlnorm(1, Dist_mu, Dist_sig)
  }
    
  return(x)
}

# Visualisation of cost of program expenditure

hist(apply(matrix(c(0), nrow = 1000, ncol = 20),c(1,2), function(x){2.5*10^6 - Trunc_lnorm()}),
     xlab = "Program Expense Assumption", ylab = "Proportion", probability = T, main= "Distribution of initiative first year expenses")




Projected_Cost_Proportion <-  matrix(c(rev(seq(0.2, 1, by = 0.2)), rep(0.2,15)), ncol = 20, nrow = 1600, byrow = T)

# Apply Initiative Infrastructure Cost

Cost_Matrix <- apply(matrix(c(0), nrow = 1600, ncol = 20),c(1,2), function(x){2.5*10^6 - Trunc_lnorm()})

Inflation_Rate <- matrix(unlist(fread("~/ACTL4001/Inflation_Simulations.csv")), ncol = 30)


Inflation_Rate <- rowCumprods(1+Inflation_Rate[,2:30])

Inflation_Rate <- Inflation_Rate[1:1600, 1:20]

Projected_Cost_Amount_Gross <- Projected_Cost_Proportion * Cost_Matrix * Inflation_Rate

# Remove this cost from the main projected simulations

Profit_Simulations <- Profit_Simulations[,2:21] - Projected_Cost_Amount_Gross

fwrite(Profit_Simulations, "~/ACTL4001/Profit Sims Combined Including Expected App Costs.csv")
















