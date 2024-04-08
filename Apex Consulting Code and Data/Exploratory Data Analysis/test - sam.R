# test script

# import libraries
library("dplyr")
library("ggplot2")

# Import data
inforce_raw <- read.csv("Case Data/2024-srcsc-superlife-inforce-dataset.csv")
summary(inforce_raw)

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

summary(inforce_clean)
# Only columns with NA are Year of Death and Year of lapse

inforce_clean %>% group_by(Region) %>% summarize(count = n(), 
                                                 mean_issue_age = mean(Issue.age),
                                                 urban_prop = sum(Urban.vs.Rural == "Urban")/n(), 
                                                 smoker_prop = sum(Smoker.Status == "S")/n(),
                                                 male_prop = sum(Sex == "M")/n(),
                                                 avg_face_amt = mean(Face.amount),
                                                 very_low_risk = sum(Underwriting.Class == "very low risk")/n(),
                                                 low_risk = sum(Underwriting.Class == "low risk")/n(),
                                                 moderate_risk = sum(Underwriting.Class == "moderate risk")/n(),
                                                 high_risk = sum(Underwriting.Class == "high risk")/n(),
                                                 agent_prop = sum(Distribution.Channel == "Telemarketer")/n())


inforce_clean %>% group_by(Underwriting.Class) %>% summarize(count = n(), 
                                                 mean_issue_age = mean(Issue.age),
                                                 urban_prop = sum(Urban.vs.Rural == "Urban")/n(), 
                                                 smoker_prop = sum(Smoker.Status == "S")/n(),
                                                 male_prop = sum(Sex == "M")/n(),
                                                 avg_face_amt = mean(Face.amount),
                                                 agent_prop = sum(Distribution.Channel == "Telemarketer")/n())

ggplot(inforce_clean) + geom_histogram(aes(x = Issue.age), binwidth = 1)
ggplot(inforce_clean) + geom_histogram(aes(x = Face.amount))
