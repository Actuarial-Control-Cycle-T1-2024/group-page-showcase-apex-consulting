<<<<<<< HEAD
setwd("~//")
Inforce_Set <- read.csv("2024-srcsc-superlife-inforce-dataset.csv")




=======
>>>>>>> 45b7dfea377d03632df9049236187d1c86a29148
library("ggplot2")
library("dplyr")

#setwd("~//")
Inforce_Set <- read.csv("Case Data/2024-srcsc-superlife-inforce-dataset.csv")

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

<<<<<<< HEAD
Mortality_Causes <- Inforce_Set[is.na(Inforce_Set$Cause.of.Death) == 0 & Inforce_Set$Cause.of.Death != "" ,]

Mortality_Causes <- Mortality_Causes %>%
  mutate(age_of_death = as.numeric(Issue.age) - as.numeric(Issue.year) + 2024,
         age_and_type = paste(age_of_death, Cause.of.Death)) %>%
  group_by(age_and_type) %>%
  summarise(Number_of_deaths = length(Cause.of.Death),
            age_of_death = unique(age_of_death),
            cause = unique(Cause.of.Death))


ggplot(Mortality_Causes) +
  geom_col(aes(x = age_of_death, y = Number_of_deaths, fill = cause))


#-----------------------------------------------------------------------------------------------

Mortality_Causes <- Inforce_Set[Inforce_Set$Cause.of.Death == "C00-D48" | Inforce_Set$Cause.of.Death == "I00-I99" |
                                  Inforce_Set$Cause.of.Death == "V01-Y89"| Inforce_Set$Cause.of.Death == "J00-J98",]

Mortality_Causes <- Mortality_Causes %>%
  mutate(age_of_death = as.numeric(Issue.age) - as.numeric(Issue.year) + 2024,
         age_and_type = paste(age_of_death, Cause.of.Death)) %>%
  group_by(age_and_type) %>%
  summarise(Number_of_deaths = length(Cause.of.Death),
            age_of_death = unique(age_of_death),
            cause = unique(Cause.of.Death))


ggplot(Mortality_Causes) +
  geom_col(aes(x = age_of_death, y = Number_of_deaths, fill = cause))


=======
Mortality_Causes
>>>>>>> 45b7dfea377d03632df9049236187d1c86a29148
