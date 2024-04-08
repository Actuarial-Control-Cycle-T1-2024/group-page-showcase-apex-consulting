setwd("~//")
Inforce_Set <- read.csv("2024-srcsc-superlife-inforce-dataset.csv")

names(Inforce_Set) <- Inforce_Set[3,]
Inforce_Set <- Inforce_Set[c(4:nrow(Inforce_Set)),]

#######################
#### Variables ########
#######################

AGE <- 3
SMOKING_STATUS <- 4
GENDER <- 5
POLICY_TYPE <- 6
WITHDRAWL_STATUS <- 7


SMOKING <- 1
NON_SMOKING <- 0

TIME_LENGTH <- 30


library("ggplot2")
library("dplyr")
library("markovchain")


# Individual Approach

# Create a List of Issued Year Cohorts

Issued_Policyholders <- Inforce_Set %>%
  group_by(Issue.year) %>%
  summarise(number = length(Issue.year))

# Have a list with a multi dimensional array which we can track specific elements
# - List consists of individual Cohorts of People (i.e Issue Year)
# - Within each issued cohort of individuals we have an array broken into 3 levels
#     1. Policy Holder Individual
#     2. Time elapsed or probability of passing

# Within the third dimension we have different slices of information
#     1. Probability of survival
#     2. Information on the persons age
#     3. Information on the persons smoking status
#     4. Withdrawl Group

#---------------------------------------------------------------------------------------------------------------------------------------

Policyholders <- list(array(c(0), dim = c(Issued_Policyholders$number[1], TIME_LENGTH, 4) ))

# Update Data Points

Issued_Policyholders_2001 <- Inforce_Set %>%
  filter(Issue.year == 2001) %>%
  mutate(Smoker_Variable = case_when(Smoker.Status == "NS" ~ 0,
                                     Smoker.Status == "S" ~ 1)) %>%
  mutate(Gender_Variable = case_when(Sex == "M" ~ 0,
                                     Sex == "F" ~ 1)) %>%
  group_by(Policy.number) %>%
  summarise(Age = as.numeric(Issue.age),
            Smoker = Smoker_Variable,
            Gender = Gender_Variable,
            Type = Policy.type)



Policyholders$`2001`[,,2] <- Issued_Policyholders_2001$Age + matrix(c(0:(TIME_LENGTH-1)), 
                                                                    ncol = TIME_LENGTH, nrow = Issued_Policyholders$number[1], byrow = T)

Policyholders$`2001`[,,3] <- Issued_Policyholders_2001$Smoker

# Create a new cohort

Policyholders <- append(Policyholders, list(array(c(0), dim = c(Issued_Policyholders$number[2], TIME_LENGTH, 4) )))


#--------------------------------------------------------------------------------------------------------------------------------------------

# 4.3% baseline are able to quit smoking in a year
# 9.2% are non-current usage
# Roughly over the course of a year 50% are in a withdrawn/failed state and 50% are in an abstinence state
# Build a discrete time matrix for yearly transition probabilities
# N (Never), U (Tobacco), W (In a failed transition state), A (Longer-term abstinence)

#https://www.aihw.gov.au/reports/alcohol/alcohol-tobacco-other-drugs-australia/contents/drug-types/tobacco
#https://qmro.qmul.ac.uk/xmlui/bitstream/handle/123456789/35923/Duffy%20What%20proportion%20of%20people%20who%20try%20one%20cigarette%202018%20Accepted.pdf?sequence=2&isAllowed=y#:~:text=The%20conversion%20rate%20from%20trying,%25%20CI%2064.8%2D65.4%25). 
#https://www.pnas.org/doi/10.1073/pnas.1011277108#t01
#https://www.icpsr.umich.edu//web/pages/NAHDAP/path-study-tables.html
#https://tobaccocontrol.bmj.com/content/31/3/424#DC1
#https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD003999.pub6/references
#https://www.who.int/data/gho/data/indicators/indicator-details/GHO/most-recent-nationally-representative-survey-reporting-prevalence-of-tobacco-use-or-smoking-among-adults
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2843145/

# Never Use yearly transition rates

N_U <- rnorm(1,0.3* 1.5/100, 0.1/100)
N_W <- rnorm(1,0.3 * 1/100, 0.1/100)
N_A <- rnorm(1,0.3 * 1/100, 0.1/100)

# Current Cigarrete Use yearly transition rates

# Based on U->A 4.3% Avg Population Transition for 6+ months - roughly survival rate of 4.3% for 6+ months, assume geometrically 50% of people last.
# Roughly 9% of people make a quit attempt in withdrawn (making it past 30 days)

U_W <- rnorm(1, 7/100,  sqrt(0.75)*1/100)
U_A <- rnorm(1, 2/100, sqrt(0.25)*1/100)

# Withdrawn Cigarrette Use yearly transition rates

W_U <- rnorm(1, 6/100, 1.2/100)
W_A <- rnorm(1, 4/100, 0.5/100)

# Abstaining Cigarette Use yearly transition rates
# Apply the W_U transition rate assuming geometric progression - if abstaining 50% * 50% - 25% to fall off to begin using.

A_W <- rnorm(1, 4.5/100, sqrt(0.75) * 1/100)
A_U <- rnorm(1, 1.5/100, sqrt(0.25) * 1/100)


# alpha <- rnorm(1, 0.003, 0)
# beta <- rnorm(1, 0.07, 0)
# lambda <- rnorm(1, 0.0139, 0)
# rho <- rnorm(1, 0.0506, 0)

# mu <- rnorm(1, 0.001/365, 0.0005/365)
# alpha <- rnorm(1, 0.004, 0.001)
# beta <- rnorm(1, 0.0575, 0.03)
# lambda <- rnorm(1, 0.014, 0.005)
# rho <- rnorm(1, 0.11, 0.02)
# 
# 
# mu <- rnorm(1, 0.001/365, 0.0005/365)
# alpha <- rnorm(1, 0.002, 0)
# beta <- rnorm(1, 0.04, 0)
# lambda <- rnorm(1, 0.013, 0)
# rho <- rnorm(1, 0.25, 0)
# 
# Smoking_Process <- new("markovchain", c("Non-Smoker", "Smoker", "Withdrawn", "Quit"),byrow = T,
#                        transitionMatrix  = matrix(c(1-mu, mu, 0,0,
#                                                   0, 1-alpha, alpha, 0,
#                                                   0, rho, 1 - rho - beta, beta,
#                                                   0, 0, lambda, 1- lambda), nrow = 4, byrow = T))
# 
# Smoking_Process <- new("markovchain", c("Smoker", "Withdrawn", "Quit"),byrow = T,
#                        transitionMatrix  = matrix(c(1-alpha, alpha, 0,
#                                                     rho, 1 - rho - beta, beta,
#                                                     0, lambda, 1- lambda), nrow = 3, byrow = T))
# 

#Hazard Ratio for changing accross demographics

Age_Mix <- c(0.18, 0.46*0.25, 0.46*0.75, 0.16)
Gender_Mix <- c(0.5, 0.5)


Age_Mix <- c(0.18, 0.46*0.25, 0.46*0.75, 0.16) * c(5.3/100, 0.126/100, )

# Sample

NS_S_Age <- c(1.83, 1.35, 1.13, 1)
NS_S_Age <- NS_S_Age/sum(NS_S_Age*Age_Mix/sum(Age_Mix))


NS_S_Gender <- c(1, 1.66)

A_S_Age <- c(13.9, 7.21, 2.65, 1)
A_S_Gender <- c(1, 0.89)


S_A_Age <- c(1.91, 1.38, 0.85, 1)
A_S_Gender <- c(1, 1.02)

Smoking_Process <- new("markovchain", c("Never-Smoker","Smoker", "Smoke Occasionally", "Quit"),byrow = T,
                       transitionMatrix  = matrix(c(1-N_U-N_W-N_A, N_U, N_W, N_A,
                                                    0, 1 -U_W - U_A, U_W, U_A,
                                                    0, W_U, 1 - W_U - W_A, W_A,
                                                    0, A_U, A_W, 1- A_U - A_W), nrow = 4, byrow = T))

Store <- matrix(0, nrow = 30, ncol = 10000)

for (i in 1:10000) {

  Store[,i] <- markovchainSequence(30, markovchain = Smoking_Process, t0 = "Never-Smoker")
  
}

Distribution_at_times <- matrix(c(1:30),ncol = 5, nrow = 30)

Distribution_at_times[,2] <- rowSums(Store == "Never-Smoker")
Distribution_at_times[,3] <- rowSums(Store == "Smoker")
Distribution_at_times[,4] <- rowSums(Store == "Smoke Occasionally")
Distribution_at_times[,5] <- rowSums(Store == "Quit")

Distribution_at_times <- as.data.frame(Distribution_at_times)
names(Distribution_at_times) <- c("Year","Never-Smoker", "Smoker", "Smoke Occasionally", "Quit")

ggplot(Distribution_at_times) +
  geom_line(mapping = aes(x = Year, y = `Never-Smoker`)) +
  geom_line(mapping = aes(x = Year, y = `Smoke Occasionally`), col = 'blue', lwd = 1) +
  geom_line(mapping = aes(x = Year, y = Quit), col = 'red', lwd = 1) +
  geom_line(mapping = aes(x = Year, y = Smoker), col = 'green', lwd = 1) +
  theme_bw()

Distribution_at_times$Smoker + Distribution_at_times$`Smoke Occasionally`
