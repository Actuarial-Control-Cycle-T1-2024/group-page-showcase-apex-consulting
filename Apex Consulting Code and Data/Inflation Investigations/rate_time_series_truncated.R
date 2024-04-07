# ------------ Import Data and Define Params ------------
# Import libraries & data 
library("forecast")
library("ggplot2")
library("dplyr")

set.seed(123)

data <- read.csv("srcsc-2024-lumaria-economic-data-CLEANED.csv", header = TRUE)

# truncate from 1982 onwards
# data <- data[data$Year >= 1982, ]

# data <- data %>%
#   rename(Overnight_Rate = Government.of.Lumaria.Overnight.Rate,
#          RF_Annual_Spot_1yr = X1.yr.Risk.Free.Annual.Spot.Rate,
#          RF_Annual_Spot_10yr = X10.yr.Risk.Free.Annual.Spot.Rate)

head(data)

# Define parameters for simulations 
n_simulations <- 10000
n_periods <- 30


# ============== INFLATION RATE ============== 

# ------------ Data cleaning: Inflation ------------
historical_inflation <- data %>%
  select(Year, Inflation)

historical_inflation <- tsibble::as_tsibble(historical_inflation, index = Year)
trunc_historical_inflation <- historical_inflation[historical_inflation$Year >= 1982,]

# Convert percentages to decimal places 
historical_inflation$Inflation <- as.numeric(sub("%", "", historical_inflation$Inflation)) / 100
trunc_historical_inflation$Inflation <- as.numeric(sub("%", "", 
                                                       trunc_historical_inflation$Inflation)) / 100

# Data visualisation
ggplot(historical_inflation, aes(x = Year, y = Inflation)) + 
  geom_line(size = 0.8) + 
  ggtitle("Historical Inflation Rates")





# ------------ Inflation Random Walk ------------

inflation_sims <- data.frame(matrix(nrow = n_periods, ncol = n_simulations))

mean_inf <- mean(trunc_historical_inflation$Inflation)
sd_inf <- sd(trunc_historical_inflation$Inflation)


for (j in 1:n_simulations) {
  sim_inf_base <- numeric(n_periods)
  
  # Perform random walk for each period
  for (i in 1:n_periods) {
    while (TRUE) {
      random_walk <- cumsum(rnorm(1, mean = mean_inf, sd = sd_inf))
      
      if (random_walk > min(trunc_historical_inflation$Inflation) & 
          random_walk < max(trunc_historical_inflation$Inflation)) {
        sim_inf_base[i] <- random_walk
        break
      }
    }
  }
  
  # Store simulation results in data frame
  inflation_sims[, j] <- sim_inf_base
}

summary_inf_sims <- data.frame(
  Year = seq(from = 2024, to = 2053),
  Mean_inf = apply(inflation_sims, 1, mean),
  Min_inf = apply(inflation_sims, 1, min),
  Max_inf = apply(inflation_sims, 1, max),
  Percentile_90_inf = apply(inflation_sims, 1, function(x) quantile(x, probs = 0.9)),
  Percentile_10_inf = apply(inflation_sims, 1, function(x) quantile(x, probs = 0.1))
)

plot(historical_inflation$Year, historical_inflation$Inflation, type = "l", lwd = 2,
     xlim = c(1960, 2055),
     main = "Historical and Forecasted Inflation Rates",
     xlab = "Year",
     ylab = "Inflation Rate")
lines(summary_inf_sims$Year, summary_inf_sims$Percentile_90_inf, type = "l", col = "red2", lwd = 2)
lines(summary_inf_sims$Year, summary_inf_sims$Percentile_10_inf, type = "l", col = "springgreen3", lwd = 2)
lines(summary_inf_sims$Year, summary_inf_sims$Mean_inf, type = "l", col = "dodgerblue3", lwd = 2)
legend("topright", legend = c("Mean", "90th Percentile", "10th Percentile"), 
       col = c("dodgerblue3", "red2", "springgreen3"),
       lty = 1, lwd = 2)



# ============== OVERNIGHT RATE ============== 
# ------------ Data cleaning: Overnight Rate ------------
historical_or <- data %>%
  select(Year, Overnight_Rate)

historical_or <- tsibble::as_tsibble(historical_or, index = Year)
trunc_historical_or <- historical_or[historical_or$Year >= 1982,]

# Convert percentages to decimal places 
historical_or$Overnight_Rate <- as.numeric(sub("%", "", historical_or$Overnight_Rate)) / 100
trunc_historical_or$Overnight_Rate <- as.numeric(sub("%", "", 
                                                     trunc_historical_or$Overnight_Rate)) / 100


# Data visualisation
ggplot(historical_or, aes(x = Year, y = Overnight_Rate)) + 
  geom_line(size = 0.8) + 
  ggtitle("Historical Overnight Rates")


# ------------ Overnight Rate Random Walk ------------

mean_or <- mean(trunc_historical_or$Overnight_Rate)
sd_or <- sd(trunc_historical_or$Overnight_Rate)


or_sims <- data.frame(matrix(nrow = n_periods, ncol = n_simulations))

for (j in 1:n_simulations) {
  sim_or_base <- numeric(n_periods)
  
  # Perform random walk for each period
  for (i in 1:n_periods) {
    while (TRUE) {
      random_walk <- cumsum(rnorm(1, mean = mean_or, sd = sd_or))
      
      if (random_walk > min(trunc_historical_or$Overnight_Rate) & 
          random_walk < max(trunc_historical_or$Overnight_Rate)) {
        sim_or_base[i] <- random_walk
        break
      }
    }
  }
  
  # Store simulation results in data frame
  or_sims[, j] <- sim_or_base
}

summary_or_sims <- data.frame(
  Year = seq(from = 2024, to = 2053),
  Mean_OR = apply(or_sims, 1, mean),
  Min_OR = apply(or_sims, 1, min),
  Max_OR = apply(or_sims, 1, max),
  Percentile_90_OR = apply(or_sims, 1, function(x) quantile(x, probs = 0.9)),
  Percentile_10_OR = apply(or_sims, 1, function(x) quantile(x, probs = 0.1))
)

plot(historical_or$Year, historical_or$Overnight_Rate, type = "l", lwd = 2,
     xlim = c(1960, 2055),
     main = "Historical and Forecasted Overnight Rates",
     xlab = "Year",
     ylab = "Overnight Rate")
lines(summary_or_sims$Year, summary_or_sims$Percentile_90_OR, type = "l", col = "springgreen3", lwd = 2)
lines(summary_or_sims$Year, summary_or_sims$Percentile_10_OR, type = "l", col = "red2", lwd = 2)
lines(summary_or_sims$Year, summary_or_sims$Mean_OR, type = "l", col = "dodgerblue3", lwd = 2)
legend("bottomleft", legend = c("Mean", "90th Percentile", "10th Percentile"), 
       col = c("dodgerblue3", "springgreen3", "red2"),
       lty = 1, lwd = 2)



# ============== RF ONE YEAR RATE ============== 
# ------------ Data cleaning: RF 1yr Rate ------------
historical_1yr_rf <- data %>%
  select(Year, RF_Annual_Spot_1yr)

historical_1yr_rf <- tsibble::as_tsibble(historical_1yr_rf, index = Year)
trunc_historical_1yr_rf <- historical_1yr_rf[historical_1yr_rf$Year >= 1982,]

# Convert percentages to decimal places 
historical_1yr_rf$RF_Annual_Spot_1yr <- as.numeric(sub("%", "", 
                                                       historical_1yr_rf$RF_Annual_Spot_1yr)) / 100

trunc_historical_1yr_rf$RF_Annual_Spot_1yr <- as.numeric(sub("%", "", 
                                                             trunc_historical_1yr_rf$RF_Annual_Spot_1yr)) / 100

# Data visualisation
ggplot(historical_1yr_rf, aes(x = Year, y = RF_Annual_Spot_1yr)) + 
  geom_line(size = 0.8) + 
  ggtitle("Historical RF (1yr) Rates")


# ------------ 1yr RF Rate Random Walk ------------
mean_rf1 <- mean(trunc_historical_1yr_rf$RF_Annual_Spot_1yr)
sd_rf1 <- sd(trunc_historical_1yr_rf$RF_Annual_Spot_1yr)


rf1_sims <- data.frame(matrix(nrow = n_periods, ncol = n_simulations))

for (j in 1:n_simulations) {
  sim_rf1_base <- numeric(n_periods)
  
  # Perform random walk for each period
  for (i in 1:n_periods) {
    while (TRUE) {
      random_walk <- cumsum(rnorm(1, mean = mean_rf1, sd = sd_rf1))
      
      if (random_walk > min(trunc_historical_1yr_rf$RF_Annual_Spot_1yr) & 
          random_walk < max(trunc_historical_1yr_rf$RF_Annual_Spot_1yr)) {
        sim_rf1_base[i] <- random_walk
        break
      }
    }
  }
  
  # Store simulation results in data frame
  rf1_sims[, j] <- sim_rf1_base
}

summary_rf1_sims <- data.frame(
  Year = seq(from = 2024, to = 2053),
  Mean_RF1 = apply(rf1_sims, 1, mean),
  Min_RF1 = apply(rf1_sims, 1, min),
  Max_RF1 = apply(rf1_sims, 1, max),
  Percentile_90_RF1 = apply(rf1_sims, 1, function(x) quantile(x, probs = 0.9)),
  Percentile_10_RF1 = apply(rf1_sims, 1, function(x) quantile(x, probs = 0.1))
)

plot(historical_1yr_rf$Year, historical_1yr_rf$RF_Annual_Spot_1yr, type = "l", lwd = 2,
     xlim = c(1960, 2055),
     main = "Historical and Forecasted 1yr RF Rates",
     xlab = "Year",
     ylab = "RF Rate (1yr)")
lines(summary_rf1_sims$Year, summary_rf1_sims$Percentile_90_RF1, type = "l", col = "springgreen3", lwd = 2)
lines(summary_rf1_sims$Year, summary_rf1_sims$Percentile_10_RF1, type = "l", col = "red2", lwd = 2)
lines(summary_rf1_sims$Year, summary_rf1_sims$Mean_RF1, type = "l", col = "dodgerblue3", lwd = 2)
legend("bottomleft", legend = c("Mean", "90th Percentile", "10th Percentile"), 
       col = c("dodgerblue3", "springgreen3", "red2"),
       lty = 1, lwd = 2)



# ============== RF TEN YEAR RATE ============== 
# ------------ Data cleaning: RF 1yr Rate ------------
historical_10yr_rf <- data %>%
  select(Year, RF_Annual_Spot_10yr)

historical_10yr_rf <- tsibble::as_tsibble(historical_10yr_rf, index = Year)
trunc_historical_10yr_rf <- historical_10yr_rf[historical_10yr_rf$Year >= 1982,]

# Convert percentages to decimal places 
historical_10yr_rf$RF_Annual_Spot_10yr <- as.numeric(sub("%", "", 
                                                         historical_10yr_rf$RF_Annual_Spot_10yr)) / 100

trunc_historical_10yr_rf$RF_Annual_Spot_10yr <- as.numeric(sub("%", "", 
                                                               trunc_historical_10yr_rf$RF_Annual_Spot_10yr)) / 100

# Data visualisation
ggplot(historical_10yr_rf, aes(x = Year, y = RF_Annual_Spot_10yr)) + 
  geom_line(size = 0.8) + 
  ggtitle("Historical RF (10yr) Rates")


# ------------ 10yr RF Rate Ranndomw walk ------------
mean_rf10 <- mean(trunc_historical_10yr_rf$RF_Annual_Spot_10yr)
sd_rf10 <- sd(trunc_historical_10yr_rf$RF_Annual_Spot_10yr)


rf10_sims <- data.frame(matrix(nrow = n_periods, ncol = n_simulations))

for (j in 1:n_simulations) {
  sim_rf10_base <- numeric(n_periods)
  
  # Perform random walk for each period
  for (i in 1:n_periods) {
    while (TRUE) {
      random_walk <- cumsum(rnorm(1, mean = mean_rf10, sd = sd_rf10))
      
      if (random_walk > min(trunc_historical_10yr_rf$RF_Annual_Spot_10yr) & 
          random_walk < max(trunc_historical_10yr_rf$RF_Annual_Spot_10yr)) {
        sim_rf10_base[i] <- random_walk
        break
      }
    }
  }
  
  # Store simulation results in data frame
  rf10_sims[, j] <- sim_rf10_base
}

summary_rf10_sims <- data.frame(
  Year = seq(from = 2024, to = 2053),
  Mean_RF10 = apply(rf10_sims, 1, mean),
  Min_RF10 = apply(rf10_sims, 1, min),
  Max_RF10 = apply(rf10_sims, 1, max),
  Percentile_90_RF10 = apply(rf10_sims, 1, function(x) quantile(x, probs = 0.9)),
  Percentile_10_RF10 = apply(rf10_sims, 1, function(x) quantile(x, probs = 0.1))
)

plot(historical_10yr_rf$Year, historical_10yr_rf$RF_Annual_Spot_10yr, type = "l", lwd = 2,
     xlim = c(1960, 2055),
     main = "Historical and Forecasted 10yr RF Rates",
     xlab = "Year",
     ylab = "RF Rate (10yr)")
lines(summary_rf10_sims$Year, summary_rf10_sims$Percentile_90_RF10, type = "l", col = "springgreen3", lwd = 2)
lines(summary_rf10_sims$Year, summary_rf10_sims$Percentile_10_RF10, type = "l", col = "red2", lwd = 2)
lines(summary_rf10_sims$Year, summary_rf10_sims$Mean_RF10, type = "l", col = "dodgerblue3", lwd = 2)
legend("bottomleft", legend = c("Mean", "90th Percentile", "10th Percentile"), 
       col = c("dodgerblue3", "springgreen3", "red2"),
       lty = 1, lwd = 2)



# ============== RESULTS ============== 

# All 10000 simulations
inflation_sims #inflation
or_sims #overnight rate
rf1_sims #one-yr risk free rate
rf10_sims #ten-yr risk free rate

combined_rates <- left_join(summary_inf_sims, 
                            summary_or_sims,
                            by = "Year")
combined_rates <- left_join(combined_rates, summary_rf1_sims, by = "Year")
combined_rates <- left_join(combined_rates, summary_rf10_sims, by = "Year")

head(combined_rates)

mean(combined_rates$Mean_RF10)

file_path <- "C:/Users/grace/Desktop/Uni/Actuarial Studies/ACTL4001 (2024 Term 1)/SOA Group Assignment/02Code/combined_rates.csv"
write.csv(combined_rates, file = file_path)
