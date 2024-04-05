
setwd("C:/Users/grace/Desktop/Uni/Actuarial Studies/ACTL4001 (2024 Term 1)/SOA Group Assignment/01Data")

library("ggplot2")
library("dplyr")
library("tidyr")

#Import data
inforce_raw = read.csv("2024-srcsc-superlife-inforce-dataset.csv")

policy_type <- inforce_raw %>%
  group_by(Policy.type) %>%
  summarise(count = n())

policy_counts_by_age <- inforce_raw %>%
  select(Issue.age, Policy.type) %>%
  group_by(Issue.age) %>%
  summarise(SPWL_count = sum(ifelse(Policy.type == "SPWL", 1, 0)),
            T20_count = sum(ifelse(Policy.type == "T20", 1, 0)))


policy_counts_long <- pivot_longer(policy_counts_by_age, cols = c(SPWL_count, T20_count), 
                                   names_to = "Policy_Type", values_to = "Count")

ggplot(policy_counts_long, aes(x = factor(Issue.age), y = Count, fill = Policy_Type)) +
  geom_col(position = "dodge") +
  labs(x = "Issue Age", y = "Count", fill = "Policy Type") +
  ggtitle("Count of Policies by Issue Age and Policy Type") +
  theme_minimal()


face_amt <- inforce_raw %>%
  group_by(Face.amount) %>%
  summarise(count = n())

face_amt$Face.amount = as.character(face_amt$Face.amount)
inforce_raw$Face.amount = as.character(inforce_raw$Face.amount)

face_amt_by_age <- inforce_raw %>%
  select(Issue.age, Face.amount) %>%
  group_by(Issue.age) %>%
  summarise(count_50000 = sum(ifelse(Face.amount == "50000", 1, 0)),
            count_100000 = sum(ifelse(Face.amount == "100000", 1, 0)),
            count_250000 = sum(ifelse(Face.amount == "250000", 1, 0)),
            count_500000 = sum(ifelse(Face.amount == "500000", 1, 0)),
            count_1000000 = sum(ifelse(Face.amount == "1000000", 1, 0)),
            count_2000000 = sum(ifelse(Face.amount == "2000000", 1, 0)))

face_counts_long <- pivot_longer(face_amt_by_age, cols = c(count_50000, count_100000,
                                                           count_250000, count_500000,
                                                           count_1000000, count_2000000), 
                                   names_to = "Face_Amt", values_to = "Count")

ggplot(face_counts_long, aes(x = factor(Issue.age), y = Count, fill = Face_Amt)) +
  geom_col(position = "dodge") +
  labs(x = "Issue Age", y = "Count", fill = "Face Amt") +
  ggtitle("Count of Policies by Issue Age and Face Amt") +
  theme_minimal()

