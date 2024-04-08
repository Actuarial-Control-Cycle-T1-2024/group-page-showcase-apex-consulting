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
           position = "fill")

# Starting to Issue more SPWL insurances
# SPWL book is seeming to  be growing more strongly than T20 LOB


ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = inforce_raw$Policy.type),
           position = "fill")

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = inforce_raw$Policy.type))


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
           position = "fill")

# Can see that only 50000 policies are bought by T20 individuals
# Rest seem to follow a similar trend

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = paste(factor(inforce_raw$Face.amount),
                                                                  inforce_raw$Policy.type)),
           position = "fill")

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
  geom_bar(mapping = aes(x = inforce_current$Issue.year, fill = paste(inforce_current$Smoker.Status,
                                                                      inforce_current$Sex)),
           position = "fill") +
  scale_y_continuous(breaks = c(0:20)/20)


?scale_y_continuous
# Smokers are a very small percentage of SPWL mostly T20

ggplot() +
  geom_bar(mapping = aes(x = inforce_raw$Issue.year, fill = paste(inforce_raw$Smoker.Status,
                                                                  inforce_raw$Policy.type,
                                                                  inforce_raw$Sex)),
           position = "fill")

# Mix Assumptions - Assume the same relative mix as the last 5 years for both, as seems relatively stable.

