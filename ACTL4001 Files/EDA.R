setwd("C:/Users/grace/Desktop/Uni/Actuarial Studies/ACTL4001 (2024 Term 1)/SOA Group Assignment/01Data")

library("ggplot2")
library("dplyr")
library("lifecontingencies")
library("reshape2")


# ---------------- Data Cleaning ---------------- 
# Whole dataset 
inforce <- read.csv("2024-srcsc-superlife-inforce-dataset.csv", header = TRUE)

# Subset of lives who exited the scheme (death or surrender)
inforce_exit <- inforce %>%
  filter(!is.na(inforce$Cause.of.Death))

# Subset of lives who died
inforce_died <- inforce_exit %>%
  filter(Cause.of.Death != "") %>%
  mutate(Age.of.Death = Issue.age + (Year.of.Death - Issue.year))

# Subset of lives who surrendered 
inforce_surrender <- inforce_exit %>%
  filter(Cause.of.Death == "") %>%
  mutate(Age.of.Surrender = Issue.age + (Year.of.Lapse - Issue.year))



# ---------------- EDA Plots ---------------- 

# Plot of issue age 
ggplot(data = inforce, aes(x = Issue.age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  ggtitle("Issue Age") + 
  xlab("Issue Age") + 
  ylab("Count")

# Plot of issue year
ggplot(data = inforce, aes(x = Issue.year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  ggtitle("Issue Year") + 
  xlab("Issue Year") + 
  ylab("Count")

# Plot cause of death 
ggplot(data = inforce_died, aes(x = Cause.of.Death)) +
  geom_bar(fill = "coral", width = 0.5) +
  ggtitle("Cause of Death") + 
  xlab("Cause of Death") + 
  ylab("Count")

# Cause of death and underwriting risk 
ggplot(data = inforce_died, aes(x = Cause.of.Death, fill = Underwriting.Class)) +
  geom_bar(width = 0.5) +
  ggtitle("Cause of Death") + 
  xlab("Cause of Death") + 
  ylab("Count")


# Plot issue age vs. age of death 
ggplot(data = inforce_died, aes(x = Issue.age, y = Age.of.Death)) +
  geom_point() +
  ggtitle("Issue Age vs. Age of Death") + 
  xlab("Issue Age") + 
  ylab("Age of Death")

ggplot(data = inforce_surrender, aes(x = Issue.age, y = Age.of.Surrender)) +
  geom_point() +
  ggtitle("Issue Age vs. Age of Surrender") + 
  xlab("Issue Age") + 
  ylab("Age of Surrender")



create_scatter <- function(dataset, aes_mapping, title, x_axis, y_axis){
  ggplot(data = dataset, aes(x = aes_mapping)) + 
    geom_point() + 
    ggtitle(title) + 
    xlab(x_axis) + 
    ylab(y_axis)
}


# ---------------- Survival Analysis: Population ---------------- 

lumaria_qx <- read.csv("srcsc-2024-lumaria-mortality-table-CLEANED.csv", header = TRUE)
life_table <- probs2lifetable(probs = lumaria_qx$Mortality.Rate, 
                       type = "qx", 
                       radix = 100000, 
                       name = "Lumaria_Life_Table")

summary(life_table)

# Plot Lumaria survival
plot(life_table, type="l")

# qx by age 
plot(lumaria_qx$Age, 
     lumaria_qx$Mortality.Rate, 
     type = "l", 
     xlab="Age",ylab="qx", 
     main = "qx vs x")

# log(qx) by age
plot(lumaria_qx$Age, 
     log(lumaria_qx$Mortality.Rate), 
     type = "l", 
     xlab="Age",ylab="log(qx)", 
     main = "log(qx) vs x")


# ---------------- Cost ---------------- 
interventions <- read.csv("srcsc-2024-interventions-CLEANED.csv", header = TRUE)
intervention1 <- interventions %>%
  select(-Description)


# Melt the data to long format
intervention1_long <- melt(intervention1, id.vars = c("ID", "Intervention", "Per.Unit"), 
                           measure.vars = c("Lower_Bound_Mort_Reduction", "Upper_Bound_Mort_Reduction"))

# Plot with ggparcoord
ggplot(intervention1_long, aes(x = variable, y = value, group = ID, color = as.factor(ID))) +
  geom_line(alpha = 0.5) +
  scale_color_discrete(name = "ID") +
  ggtitle("Intervention Mortality Reduction vs. Cost") +
  xlab("Mortality Reduction (%)") +
  ylab("Cost Estimate") +
  theme_minimal() +
  theme(legend.position = "right")
