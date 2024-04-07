#############################
###### Introduction #########
#############################

# This code was used to investigate the level of mix between new policies
# specifically we want to predict the number of policies in the future.
# A selection of a range of rating factors were decided to understand future new entrants.

#----------------------------------------------------

inforce_raw <- read.csv("~/ACTL4001/Case Data/2024-srcsc-superlife-inforce-dataset.csv")

library(dplyr)
library(ggplot2)

graph4col <- c("#044B7F", "#25A0F8", "#129B99", "#420480")

# Change in the distribution of policyholders over time

inforce_current <- inforce_raw %>%
  mutate(Starting_Age = Issue.age + 2023 - Issue.year,
         grouping = paste(Issue.age, Sex, Smoker.Status, Policy.type,
                          Face.amount, Distribution.Channel)) %>%
  group_by(grouping) %>%
  summarise(proportion = n()/nrow(inforce_raw))

# Check distribution channel over time # - More recent years online picked up

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = inforce_raw$Distribution.Channel),
           position = "fill") +
  theme_classic() +
  scale_x_continuous("Issue Year") + 
  scale_y_continuous("Proportion") +
  scale_fill_manual("Distribution Channel", values = graph4col[1:3]) +
  ggtitle("Distribution Channel of Historical Policyholders")+
  theme(plot.title = element_text("Distribution of Channels of Policyholders", hjust = 0.5))

# Starting to Issue more SPWL insurances
# SPWL book is seeming to  be growing more strongly than T20 LOB


ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = inforce_raw$Policy.type),
           position = "fill") +
  theme_classic() +
  scale_x_continuous("Issue Year") + 
  scale_y_continuous("Proportion") +
  scale_fill_manual("Policy Type", values = graph4col[1:3]) +
  ggtitle("Policy Types of historical Policyholders")+
  theme(plot.title = element_text("Distribution of Policy Types", hjust = 0.5))

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = inforce_raw$Policy.type))+
  theme_classic()+
  scale_x_continuous("Issue Year") + 
  scale_y_continuous("Policies Issued") +
  scale_fill_manual("Policy Type", values = graph4col[1:3]) +
  ggtitle("Policy Types of historical Policyholders")+
  theme(plot.title = element_text("Distribution of Policy Types", hjust = 0.5))


# More Females written policies - makes sense to just have 50/50 Normally Distributed

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = inforce_raw$Sex),
           position = "fill")



ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = paste(inforce_raw$Sex,inforce_raw$Policy.type)),
           position = "fill")

# Check

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = factor(inforce_raw$Face.amount)),
           position = "fill") + 
  theme_classic()+
  scale_x_continuous("Issue Year") + 
  scale_y_continuous("Proportion") +
  scale_fill_discrete("Face Amounts")+ 
  ggtitle("Face Amounts of joining policyholders")+
  theme(plot.title = element_text("Distribution of Policy Types", hjust = 0.5))

# Can see that only 50000 policies are bought by T20 individuals
# Rest seem to follow a similar trend

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = paste(factor(inforce_raw$Face.amount),
                                                                  inforce_raw$Policy.type)),
           position = "fill") + 
  theme_classic()+
  scale_x_continuous("Issue Year") + 
  scale_y_continuous("Proportion") +
  scale_fill_discrete("Face Amounts and Policy Type")+ 
  ggtitle("Face Amounts by policy type")+
  theme(plot.title = element_text("Distribution of Policy Types", hjust = 0.5))

# Relatively Normally Distributed accross age groups issued
# Age wise slight movement up saying a potential increase in age but not too significant
# Use last 4 years issu as general weights

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = factor(inforce_raw$Issue.age)),
           position = "fill")

#Perceived better risk selection over time and that majority of portfolio is non-smoker

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = inforce_raw$Smoker.Status),
           position = "fill")

inforce_current <- inforce_raw %>%
  filter(Policy.type == "T20")

ggplot() +
  geom_bar(mapping = aes(x = inforce_current$Issue.year, fill = inforce_current$Smoker.Status),
           position = "fill") +
  scale_y_continuous(breaks = c(0:20)/20)

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = paste(inforce_raw$Smoker.Status,
                                                                      inforce_raw$Sex)),
           position = "fill") +
  scale_y_continuous(breaks = c(0:20)/20) + 
  theme_classic()+
  scale_x_continuous("Issue Year") + 
  scale_y_continuous("Proportion") +
  scale_fill_manual("Gender and Smoking Status", values = graph4col[1:4]) +
  ggtitle("Demographics of joining policyholders")+
  theme(plot.title = element_text("Distribution of Policy Types", hjust = 0.5))


?scale_y_continuous
# Smokers are a very small percentage of SPWL mostly T20

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = paste(inforce_raw$Smoker.Status,
                                                                  inforce_raw$Policy.type,
                                                                  inforce_raw$Sex)),
           position = "fill") +
  theme_classic()+
  scale_x_continuous("Issue Year") + 
  scale_y_continuous("Proportion") +
  ggtitle("Demographics of joining policyholders")+
  theme(plot.title = element_text("Distribution of Policy Types", hjust = 0.5)) +
  scale_fill_discrete(name = "Policy types by demographic") 


# Mix Assumptions - Assume the same relative mix as the last 5 years for both, as seems relatively stable.

