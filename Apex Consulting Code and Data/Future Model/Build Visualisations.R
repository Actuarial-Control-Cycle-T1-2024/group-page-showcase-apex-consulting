##############################
###### Introduction ##########
##############################

# Code used to build the ggplot graphs of the profits
# This is used within the report

#----------------------------------------------------

# Build Visualisations
library('data.table')
Base_set <- fread("~/ACTL4001/Simulations Data/Base Case Combined SPWL and T20.csv")
Profit_Simulations <- fread("~/ACTL4001/Simulations Data/Profit Sims Combined Including Expected App Costs.csv")

Profit_Simulations <- cbind(rep(0,nrow(Profit_Simulations)),
                            Profit_Simulations)

quantiles <- apply(Profit_Simulations, 2, quantile, probs = c(0.05,0.25, 0.5, 0.75, 0.95))

average_profit <- apply(Profit_Simulations, 2, mean)

average_profit/ Base_set- 1

# Account for the 7% Payout in benefits

expected_profit <- average_profit - (average_profit - unlist(Base_set))*0.5

# Create dataframe for ggplot
Profit_Bandings <- data.frame(time = 0:PROJECTION_TIME + 2023,
                              Median = quantiles[3, ],
                              P25 = quantiles[2, ],
                              P75 = quantiles[4, ],
                              P5 = quantiles[1, ],
                              P95 = quantiles[5, ],
                              mean = average_profit)

ggplot(Profit_Bandings, aes(x = time, y = mean)) +
  geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 1.05, xmax = 1.05000001, fill = "Baseline Profits"))+
  geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 1.05, xmax = 1.05000001, fill = "Expected Post Initiative"))+
  geom_rect(aes(ymin = 0.1, ymax = 0.10001, xmin = 1.05, xmax = 1.05000001, fill = "Expected Post Initiative (incl. Policyholder Return)"))+
  geom_ribbon(aes(ymin = P5, ymax = P25, fill = "5th - 95th Percentiles"), alpha = 0.5) +
  geom_ribbon(aes(ymin = P25, ymax = P75, fill = "25th - 75th Percentiles"), alpha = 0.5) +
  geom_ribbon(aes(ymin = P75, ymax = P95, fill = "5th - 95th Percentiles"), alpha = 0.5) +
  geom_line(col = "black", lwd = 1.5) +
  geom_line(mapping=aes(x = c(0:20)+2023, y = expected_profit), col = "red", lwd = 1.5, lty = "dashed") +
  geom_line(mapping = aes(x = c(0:20)+ 2023, y =unlist(Base_set)), col = "#420480", lwd=  1.5) +
  labs(title = "Superlife profit projection over 20 years (Combined)",
       x = "Projection Time",
       y = "Profit (Crowns)") +
  scale_fill_manual(name = "",
                    values = c("Baseline Profits" = "red", "Expected Post Initiative" =  "black","Expected Post Initiative (incl. Policyholder Return)" =  "#420480",
                               "25th - 75th Percentiles" = "#129B99", "5th - 95th Percentiles" = "#0676C6"),
                    labels = c("25th - 75th Percentiles", "5th - 95th Percentiles","Expected Post Initiative (incl. Policyholder Return)","Expected Post Initiative", "Baseline Profits")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box.just = "center",
        legend.title = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(color = "black")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous("Projection Time", expand = c(0,0), limits = c(2024,2023+PROJECTION_TIME)) +
  scale_y_continuous("Profit (Crowns)", labels = scales::unit_format(unit = "M", scale  =1e-6))
