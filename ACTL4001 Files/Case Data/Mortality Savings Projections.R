# In progress - Sam 5/3/24
# import libraries
library("dplyr")
library("ggplot2")
library("lifecontingencies")
library("reshape2")
library("tidyr")


# Import data
inforce_raw <- read.csv("Case Data/2024-srcsc-superlife-inforce-dataset.csv")
summary(inforce_raw)
n = nrow(inforce_raw)

lumaria_qx <- read.csv("srcsc-2024-lumaria-mortality-table-CLEANED.csv", header = TRUE)
life_table <- probs2lifetable(probs = lumaria_qx$Mortality.Rate, 
                              type = "qx", 
                              radix = 100000, 
                              name = "Lumaria_Life_Table")
life_table_df <- data.frame(life_table@x, life_table@lx)

inforce_clean <- inforce_raw

# Fix column name inconsistencies
colnames(inforce_clean)[colnames(inforce_clean) == "Death.indicator"] <- "Death.Indicator"

# Replace NA values with 0 for indicators
inforce_clean$Lapse.Indicator[is.na(inforce_clean$Lapse.Indicator)] <- 0
inforce_clean$Death.Indicator[is.na(inforce_clean$Death.Indicator)] <- 0

# Replace Lapse Indicator "Y" with "1" (6866 occurrences)
inforce_clean$Lapse.Indicator[inforce_clean$Lapse.Indicator == "Y"] <- "1"

inforce_clean$Lapse.Indicator <- as.numeric(inforce_clean$Lapse.Indicator)
inforce_clean$Death.Indicator <- as.numeric(inforce_clean$Death.Indicator)

inforce_clean$Underwriting.Class = factor(inforce_clean$Underwriting.Class, levels = c("very low risk", "low risk", "moderate risk", "high risk"))

# NOTE years of lapse and death have na because they have not occurred
inforce_clean$age_at_death <- inforce_clean$Issue.age + inforce_clean$Year.of.Death - inforce_clean$Issue.year
inforce_clean$curr_age <- inforce_clean$Issue.age + 2024 - inforce_clean$Issue.year
inforce_clean$birth_year <- inforce_clean$Issue.year - inforce_clean$Issue.age
inforce_clean$year_of_policy <- 2024 - inforce_clean$Issue.year


# For each year, for each policy in the dataset, we want to project whether they survive, die or lapse within the period of the next year.
# Everything below here will be repeated for each year
# Ignore this for now it is not working

# Adding decrement columns for lapse and death
inforce_clean <- merge(inforce_clean, lumaria_qx, by.x = "curr_age", by.y = "Age")
names(inforce_clean)[names(inforce_clean) == 'Mortality.Rate'] <- 'base_qx'
inforce_clean$force.m <- -log(1 - inforce_clean$base_qx)
inforce_clean$new_force.m <- inforce_clean$force.m * 0.95  # dummy value
inforce_clean$new_qx <- 1 - exp(-inforce_clean$new_force.m)
inforce_clean$wx <- 0.02  # dummy value


# Simulation of the result of that policy for that year
results <- runif(n, 0, 1)
inforce_clean$state <- rep("Survival", n)
inforce_clean$state[results < inforce_clean$new_qx] <- "Death"
inforce_clean$state[results > 1 - inforce_clean$wx] <- "Lapse"
  



# reshaping test
# all dummy vals. Later on we can figure out how to appropriately model the assumptions for gender, smoking status and mortality improvements.
lumaria_qx <- read.csv("srcsc-2024-lumaria-mortality-table-CLEANED.csv", header = TRUE)
lumaria_qx$Smoker.Mortality.Rate <- lumaria_qx$Mortality.Rate * 1.5
lumaria_qx <- pivot_longer(lumaria_qx, cols = c('Mortality.Rate', 'Smoker.Mortality.Rate'), 
             names_to = 'Smoker.Status',
             values_to = 'Mortality.Rate')
lumaria_qx$Smoker.Status[lumaria_qx$Smoker.Status == "Mortality.Rate"] <- "NS"
lumaria_qx$Smoker.Status[lumaria_qx$Smoker.Status == "Smoker.Mortality.Rate"] <- "S"

lumaria_qx$Female.Mortality.Rate <- lumaria_qx$Mortality.Rate * 0.97
lumaria_qx$Male.Mortality.Rate <- lumaria_qx$Mortality.Rate * 1.03
lumaria_qx$Mortality.Rate <- NULL
lumaria_qx <- pivot_longer(lumaria_qx, cols = c('Male.Mortality.Rate', 'Female.Mortality.Rate'), 
                           names_to = 'Sex',
                           values_to = 'Mortality.Rate')
lumaria_qx$Sex[lumaria_qx$Sex == "Male.Mortality.Rate"] <- "M"
lumaria_qx$Sex[lumaria_qx$Sex == "Female.Mortality.Rate"] <- "F"

lumaria_qx$Adj.Mortality.Rate <- lumaria_qx$Mortality.Rate * 0.9


# Condensed inforce dataset with only critical variables
inforce_condensed <- inforce_clean[c("Policy.type", "curr_age", "Sex", "Smoker.Status", "birth_year", "Face.amount", "year_of_policy", "Year.of.Death")]

projection_list <- list(inforce_condensed, inforce_condensed, inforce_condensed)
projection_yrs <- length(projection_list)

# Store values across years
for(i in 1:projection_yrs){
  projection_list[[i]]$curr_age <- projection_list[[i]]$curr_age + i
  projection_list[[i]]$year_of_policy <- projection_list[[i]]$year_of_policy + i
  projection_list[[i]] <- merge(projection_list[[i]], lumaria_qx, 
                                by.x=c("curr_age", "Sex", "Smoker.Status"), 
                                by.y=c("Age", "Sex", "Smoker.Status"))
  
}


for(i in 1:projection_yrs){
  results <- runif(n, 0, 1)
  stillAlive <- rep(TRUE, n)
  dyingThisYear <- rep(FALSE, n)
  
  if(i > 1){
    projection_list[[i]]$state <- projection_list[[i-1]]$state
    projection_list[[i]]$Year.of.Death <- projection_list[[i-1]]$Year.of.Death
    projection_list[[i]]$state <- rep("Survival", n)
    stillAlive <- projection_list[[i-1]]$state == "Survival"
  }
  
  dyingThisYear <- results < projection_list[[i]]$Mortality.Rate
  projection_list[[i]]$state[!stillAlive] <- "Dead"
  projection_list[[i]]$state[dyingThisYear & stillAlive] <- "Dead"
  projection_list[[i]]$state[!dyingThisYear & stillAlive] <- "Survival"
  projection_list[[i]]$Year.of.Death[dyingThisYear & stillAlive] <- 2023 + i
}

projected_sums <- c()
for(i in 1:projection_yrs){
  projected_sums[i] <- sum(projection_list[[i]]$Face.amount[projection_list[[i]]$Year.of.Death == (2023 + i)], na.rm = T)
}

# Include interest rates model 
combined_rates <- read.csv("combined_rates.csv")

discounted_sums <- c()
for(i in 1:projection_yrs){
  discounted_sums[i] <- projected_sums[i] * prod(1/(1 + combined_rates$Mean_RF1[1:i]))
}

# Total cost over projection period
sum(discounted_sums)


# data check and random testing
projection_list[[3]] %>% arrange(desc(Year.of.Death))
sum(projection_list[[3]]$state == "Dead")

test <- inforce_condensed %>% filter(Policy.type == "T20") %>% group_by(curr_age, year_of_policy) %>% summarise(pcount = n())
ggplot(test) + geom_tile(aes(x = curr_age, y = year_of_policy, fill = pcount))


test <- inforce_condensed %>% filter(Policy.type == "SPWL") %>% group_by(curr_age, year_of_policy) %>% summarise(pcount = n())
ggplot(test) + geom_tile(aes(x = curr_age, y = year_of_policy, fill = pcount))

