setwd("~//")
Inforce_Set <- read.csv("2024-srcsc-superlife-inforce-dataset.csv")
library("ggplot2")
library("dplyr")

names(Inforce_Set) <- Inforce_Set[3,]
Inforce_Set <- Inforce_Set[c(4:nrow(Inforce_Set)),]

Region_Data <- Inforce_Set %>%
  mutate(Status = paste(Urban.vs.Rural, Region, Smoker.Status)) %>%
  group_by(Status) %>%
  summarise(Number = length(Status),
            Region = unique(Region),
            Urban_Status_and_smoke = unique(paste(Urban.vs.Rural, Smoker.Status)))

Region_Data <- as.data.frame(Region_Data)



ggplot(Region_Data) +
  geom_bar(aes(x = Number, y = Region, fill = Urban_Status_and_smoke), stat = "identity", position = "dodge") +
  ggtitle("Count of Inforce Policies by Region and Urban Status") +
  coord_flip()


Risk_Data <- Inforce_Set %>%
  mutate(Status = paste(Underwriting.Class, Smoker.Status)) %>%
  group_by(Status) %>%
  summarise(Number = length(Status),
            UW_Class = unique(Underwriting.Class),
            Smoker_Status = unique(Smoker.Status))


ggplot(Risk_Data) +
  geom_col(aes(x = factor(UW_Class, levels = c("very low risk","low risk", "moderate risk", "high risk")), y = Number, fill = Smoker_Status), position = "dodge") +
  ggtitle("Count of Inforce Policies by UW Class and Smoke Status") +
  scale_x_discrete("UW Class Grouping")

Risk_Data_2 <- Inforce_Set %>%
  mutate(Status = paste(Underwriting.Class, Region)) %>%
  group_by(Status) %>%
  summarise(Number = length(Status),
            UW_Class = unique(Underwriting.Class),
            Region_Status = unique(Region))

ggplot(Risk_Data_2) +
  geom_col(aes(x = factor(UW_Class, levels = c("very low risk","low risk", "moderate risk", "high risk")), y = Number, fill = Region_Status), position = "dodge") +
  ggtitle("Count of Inforce Policies by UW Class and Region") +
  scale_x_discrete("UW Class Grouping")


Risk_Data_3 <- Inforce_Set %>%
  mutate(Status = paste(Underwriting.Class, Urban.vs.Rural)) %>%
  group_by(Status) %>%
  summarise(Number = length(Status),
            UW_Class = unique(Underwriting.Class),
            Urban_Status = unique(Urban.vs.Rural))

ggplot(Risk_Data_3) +
  geom_col(aes(x = factor(UW_Class, levels = c("very low risk","low risk", "moderate risk", "high risk")), y = Number, fill = Urban_Status), position = "dodge") +
  ggtitle("Count of Inforce Policies by UW Class and Urban Status") +
  scale_x_discrete("UW Class Grouping")


Mortality_Causes <- Inforce_Set[is.na(Inforce_Set$Cause.of.Death) == 0,]
Mortality_Causes <- Mortality_Causes %>%
  group_by(Cause.of.Death) %>%
  summarise(Number_of_deaths = length(Cause.of.Death))


Mortality_Causes <- Mortality_Causes[2:nrow(Mortality_Causes),]

ggplot(Mortality_Causes) +
  geom_col(aes(x = Cause.of.Death, y = Number_of_deaths))




Mortality_Causes
