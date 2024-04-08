# setwd("C:/Users/grace/Desktop/Uni/Actuarial Studies/ACTL4001 (2024 Term 1)/SOA Group Assignment/01Data")

# Import libraries & data 
library("forecast")
library("ggplot2")
library("dplyr")

set.seed(123)

data <- read.csv("srcsc-2024-lumaria-economic-data-CLEANED.csv", header = TRUE)

data <- data %>%
  rename(Overnight_Rate = Government.of.Lumaria.Overnight.Rate,
         RF_Annual_Spot_1yr = X1.yr.Risk.Free.Annual.Spot.Rate,
         RF_Annual_Spot_10yr = X10.yr.Risk.Free.Annual.Spot.Rate)

head(data)

# ============== INFLATION RATE ============== 
# ------------ Data cleaning: Inflation ------------
historical_inflation <- data %>%
  select(Year, Inflation)

historical_inflation <- tsibble::as_tsibble(historical_inflation, index = Year)

# Convert percentages to decimal places 
historical_inflation$Inflation <- as.numeric(sub("%", "", historical_inflation$Inflation)) / 100


# Data visualisation
ggplot(historical_inflation, aes(x = Year, y = Inflation)) + 
  geom_line(size = 0.8) + 
  ggtitle("Historical Inflation Rates")


# ------------ arima ------------
# Make into a time series object
historical_inflation1 <- ts(historical_inflation$Inflation, frequency = 1)

inflation_arima <- auto.arima(historical_inflation1)
forecast_inf_arima <- forecast(inflation_arima, h = 30)


forecast_inf_arima_df <- data.frame(
  Point_Forecast = forecast_inf_arima$mean,
  Lo_80 = forecast_inf_arima$lower[, "80%"],
  Hi_80 = forecast_inf_arima$upper[, "80%"],
  Lo_95 = forecast_inf_arima$lower[, "95%"],
  Hi_95 = forecast_inf_arima$upper[, "95%"]
)

plot(forecast_inf_arima_df$Lo_80)


# Restrict range of historical data
# 1982 onwards
historical_inflation_shrunk <- historical_inflation[c(21:nrow(historical_inflation)),]
historical_inflation_shrunk <- ts(historical_inflation_shrunk$Inflation, frequency = 1)
inflation_shrunk_model <- auto.arima(historical_inflation_shrunk, seasonal = FALSE)
forecast_inflation_shrunk <- forecast(inflation_shrunk_model, h = 30)

forecast_inflation_shrunk_df <- data.frame(
  Point_Forecast = forecast_inflation_shrunk$mean,
  Lo_80 = forecast_inflation_shrunk$lower[, "80%"],
  Hi_80 = forecast_inflation_shrunk$upper[, "80%"],
  Lo_95 = forecast_inflation_shrunk$lower[, "95%"],
  Hi_95 = forecast_inflation_shrunk$upper[, "95%"]
)


# ------------ Exponential Smoothing ------------
# Perform exponential smoothing
ets_inflation <- ets(historical_inflation$Inflation)

# Forecast inflation using exp smoothing 
forecast_inf_ets <- forecast(ets_inflation, h = 30)

forecast_inf_ets_df <- data.frame(
  Point_Forecast = forecast_inf_ets$mean,
  Lo_80 = forecast_inf_ets$lower[, "80%"],
  Hi_80 = forecast_inf_ets$upper[, "80%"],
  Lo_95 = forecast_inf_ets$lower[, "95%"],
  Hi_95 = forecast_inf_ets$upper[, "95%"]
)

plot(forecast_inf_ets_df$Hi_80)



# ------------ Random Walk: Diff Case Starting Points ------------
# Starting points 
base <- forecast_inf_arima_df[1, 1]
good <- forecast_inf_arima_df[1, 2]
bad <- forecast_inf_arima_df[1, 3]

n_periods <- 30

mean_value <- mean(historical_inflation$Inflation)
sd_value <- sd(historical_inflation$Inflation)

# Constraints: base
base_low <- min(historical_inflation$Inflation)
base_high <- 0.07

# Constraints: bad (high inflation)
bad_low <- base - 0.01
bad_high <- max(historical_inflation$Inflation)

# Constraints: good (low inflation)
good_low <- min(historical_inflation$Inflation)
good_high <- base - 0.005

# Random walk around base
sim_inf_base <- numeric(n_periods)
for (i in 1:n_periods){
  while (TRUE){
    random_walk <- cumsum(rnorm(1, mean = base, sd = 0.5*sd_value))
    
    if (random_walk > base_low & random_walk < base_high){
      sim_inf_base[i] <- random_walk
      break
    }
  }
}

# Random walk around bad (high inflation)
sim_inf_bad <- numeric(n_periods)

for (i in 1:n_periods){
  while (TRUE){
    random_walk <- cumsum(rnorm(1, mean = bad, sd = 0.5*sd_value))
    
    if (random_walk > bad_low & random_walk < bad_high){
      sim_inf_bad[i] <- random_walk
      break
    }
  }
}


# Random walk around good (low inflation)
sim_inf_good <- numeric(n_periods)

for (i in 1:n_periods){
  while (TRUE){
    random_walk <- cumsum(rnorm(1, mean = good, sd = 0.5*sd_value))
    
    if (random_walk > good_low & random_walk < good_high){
      sim_inf_good[i] <- random_walk
      break
    }
  }
}

plot(sim_inf_base, type = "l", col = "dodgerblue3", lwd = 2, 
     ylim = c(0.01, 0.13),
     ylab = 'Inflation Rate',
     xlab = 'Period',
     main = "Forecasted Inflation Rates")
lines(sim_inf_bad, type = "l", col = "red2", lwd = 2)
lines(sim_inf_good, type = "l", col = "springgreen3", lwd = 2)
legend("topright", legend = c("Base", "High Inflation", "Low Inflation"), 
       col = c("dodgerblue3", "red2", "springgreen3"),
       lty = 1, lwd = 2)


inflation_forecast <- data.frame(
  Year = seq(from = 2024, to = 2053, by = 1),
  Inflation_good = sim_inf_good,
  Inflation_base = sim_inf_base,
  Inflation_bad = sim_inf_bad
)

plot(historical_inflation$Year, historical_inflation$Inflation, type = "l", lwd = 2,
     xlim = c(1960, 2055),
     main = "Historical and Forecasted Inflation Rates: Diff Starting Points",
     xlab = "Year",
     ylab = "Inflation Rate")
lines(inflation_forecast$Year, inflation_forecast$Inflation_bad, type = "l", col = "red2", lwd = 2)
lines(inflation_forecast$Year, inflation_forecast$Inflation_good, type = "l", col = "springgreen3", lwd = 2)
lines(inflation_forecast$Year, inflation_forecast$Inflation_base, type = "l", col = "dodgerblue3", lwd = 2)
legend("topright", legend = c("Base", "High Inflation", "Low Inflation"), 
       col = c("dodgerblue3", "red2", "springgreen3"),
       lty = 1, lwd = 2)


# ------------ RW: Multiple Simulations ------------

n_simulations <- 10000
inflation_sims <- data.frame(matrix(nrow = n_periods, ncol = n_simulations))

for (j in 1:n_simulations) {
  sim_inf_base <- numeric(n_periods)
  
  # Perform random walk for each period
  for (i in 1:n_periods) {
    while (TRUE) {
      random_walk <- cumsum(rnorm(1, mean = base, sd = sd_value))
      
      if (random_walk > min(historical_inflation$Inflation) & 
          random_walk < max(historical_inflation$Inflation)) {
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
lines(inflation_forecast$Year, summary_inf_sims$Percentile_10_inf, type = "l", col = "springgreen3", lwd = 2)
lines(summary_inf_sims$Year, summary_inf_sims$Mean_inf, type = "l", col = "dodgerblue3", lwd = 2)
legend("topright", legend = c("Mean", "90th Percentile", "10th Percentile"), 
       col = c("dodgerblue3", "red2", "springgreen3"),
       lty = 1, lwd = 2)



# ============== OVERNIGHT RATE ============== 
# ------------ Data cleaning: Overnight Rate ------------
historical_or <- data %>%
  select(Year, Overnight_Rate)

historical_or <- tsibble::as_tsibble(historical_or, index = Year)

# Convert percentages to decimal places 
historical_or$Overnight_Rate <- as.numeric(sub("%", "", historical_or$Overnight_Rate)) / 100


# Data visualisation
ggplot(historical_or, aes(x = Year, y = Overnight_Rate)) + 
  geom_line(size = 0.8) + 
  ggtitle("Historical Overnight Rates")


# ------------ arima ------------
# Make into a time series object
historical_or1 <- ts(historical_or$Overnight_Rate, frequency = 1)

or_arima <- auto.arima(historical_or1)
forecast_or_arima <- forecast(or_arima, h = 30)


forecast_or_arima_df <- data.frame(
  Point_Forecast = forecast_or_arima$mean,
  Lo_80 = forecast_or_arima$lower[, "80%"],
  Hi_80 = forecast_or_arima$upper[, "80%"],
  Lo_95 = forecast_or_arima$lower[, "95%"],
  Hi_95 = forecast_or_arima$upper[, "95%"]
)

# ------------ rw: overnight rate ------------
# Starting points 
base_or <- forecast_or_arima_df[1, 1]
good_or <- forecast_or_arima_df[1, 3]
bad_or <- forecast_or_arima_df[1, 2]

mean_or <- mean(historical_or$Overnight_Rate)
sd_or <- sd(historical_or$Overnight_Rate)


or_sims <- data.frame(matrix(nrow = n_periods, ncol = n_simulations))

for (j in 1:n_simulations) {
  sim_or_base <- numeric(n_periods)
  
  # Perform random walk for each period
  for (i in 1:n_periods) {
    while (TRUE) {
      random_walk <- cumsum(rnorm(1, mean = mean_or, sd = sd_or))
      
      if (random_walk > min(historical_or$Overnight_Rate) & 
          random_walk < max(historical_or$Overnight_Rate)) {
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

# Convert percentages to decimal places 
historical_1yr_rf$RF_Annual_Spot_1yr <- as.numeric(sub("%", "", 
                                                       historical_1yr_rf$RF_Annual_Spot_1yr)) / 100


# Data visualisation
ggplot(historical_1yr_rf, aes(x = Year, y = RF_Annual_Spot_1yr)) + 
  geom_line(size = 0.8) + 
  ggtitle("Historical RF (1yr) Rates")


# ------------ Sim: 1yr RF Rate ------------
mean_rf1 <- mean(historical_1yr_rf$RF_Annual_Spot_1yr)
sd_rf1 <- sd(historical_1yr_rf$RF_Annual_Spot_1yr)


rf1_sims <- data.frame(matrix(nrow = n_periods, ncol = n_simulations))

for (j in 1:n_simulations) {
  sim_rf1_base <- numeric(n_periods)
  
  # Perform random walk for each period
  for (i in 1:n_periods) {
    while (TRUE) {
      random_walk <- cumsum(rnorm(1, mean = mean_rf1, sd = sd_rf1))
      
      if (random_walk > min(historical_1yr_rf$RF_Annual_Spot_1yr) & 
          random_walk < max(historical_1yr_rf$RF_Annual_Spot_1yr)) {
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

# Convert percentages to decimal places 
historical_10yr_rf$RF_Annual_Spot_10yr <- as.numeric(sub("%", "", 
                                                         historical_10yr_rf$RF_Annual_Spot_10yr)) / 100


# Data visualisation
ggplot(historical_10yr_rf, aes(x = Year, y = RF_Annual_Spot_10yr)) + 
  geom_line(size = 0.8) + 
  ggtitle("Historical RF (10yr) Rates")


# ------------ Sim: 1yr RF Rate ------------
mean_rf10 <- mean(historical_10yr_rf$RF_Annual_Spot_10yr)
sd_rf10 <- sd(historical_10yr_rf$RF_Annual_Spot_10yr)


rf10_sims <- data.frame(matrix(nrow = n_periods, ncol = n_simulations))

for (j in 1:n_simulations) {
  sim_rf10_base <- numeric(n_periods)
  
  # Perform random walk for each period
  for (i in 1:n_periods) {
    while (TRUE) {
      random_walk <- cumsum(rnorm(1, mean = mean_rf1, sd = sd_rf1))
      
      if (random_walk > min(historical_10yr_rf$RF_Annual_Spot_10yr) & 
          random_walk < max(historical_10yr_rf$RF_Annual_Spot_10yr)) {
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


combined_rates <- left_join(summary_inf_sims, 
                            summary_or_sims,
                            by = "Year")
combined_rates <- left_join(combined_rates, summary_rf1_sims, by = "Year")
combined_rates <- left_join(combined_rates, summary_rf10_sims, by = "Year")

head(combined_rates)


file_path <- "C:/Users/grace/Desktop/Uni/Actuarial Studies/ACTL4001 (2024 Term 1)/SOA Group Assignment/02Code/combined_rates.csv"
write.csv(combined_rates, file = file_path)
