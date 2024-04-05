#Import libraries
library(dplyr)
library(ggplot2)

#Import data
pop_raw = read.csv("Case Data/2024-srcsc-superlife-inforce-dataset.csv")
colnames(pop_raw)[1] = "Policy.Number"

# Y.O.B
pop_raw$Year.of.Birth = pop_raw$Issue.year - pop_raw$Issue.age

#==============Withdrawal Rates Base Year 2001================================#
withdrawal_base = as.data.frame(c(2002:2023))
colnames(withdrawal_base)[1] = "Year"
withdrawal_base[1,"Withdrawals"] = 0
withdrawal_base[1,"Deaths"] = 0
withdrawal_base[1,"Population"] = nrow(pop_raw)
pop_total = nrow(pop_raw %>%
                   filter(Policy.type == "T20",
                          Issue.year == 2003))

x <- pop_raw %>%
  group_by(i)



for (i in 1:21) {
  
  withdraw = pop_raw %>%
  filter(Issue.year == 2014,
        Year.of.Lapse == 2000 + i, 
         Policy.type == "T20") %>%
  nrow()
  
  withdrawal_base[i+1,"Withdrawals"] = withdraw
  
  death = pop_raw %>%
  filter(Issue.year == 2014,
         Year.of.Death == 2000 + i) %>%
  nrow()

  withdrawal_base[i+1,"Deaths"] = death
  
  withdrawal_base[i+1,"Population"] = pop_total - sum(withdrawal_base[1:i+1,"Withdrawals"]) - sum(withdrawal_base[1:i+1, "Deaths"])
  
  withdrawal_base[i+1,"Withdrawal Rate"] = withdrawal_base[i+1,"Withdrawals"]/withdrawal_base[i,"Population"]
  
}


plot(withdrawal_base$`Withdrawal Rate`)

#======================Withdrawal Rates based on Age===========================#

withdrawal_age = as.data.frame(c(1:120))
colnames(withdrawal_age)[1] = "Age"

for( i in 1:23) {
  for (j in 1:120){
    
    withdraw = pop_raw %>%
      filter(Year.of.Lapse == 2000 + i) %>%
      mutate(Withdraw_age = 2000 + i - Year.of.Birth) %>%
      filter(Withdraw_age == j) %>%
      nrow()
    
    withdrawal_age[j,1+i] = withdraw
    colnames(withdrawal_age)[1+i] = 2000+i
  }
}

withdrawal_age = as.data.frame(t(withdrawal_age))
withdrawal_age = withdrawal_age[-1,-1:-25]
withdrawal_age = withdrawal_age[,-50:-120]

withdrawal_base[c(paste0("Age ", seq(26,74,1)))] = NA

for (i in 1:23){
  
  for (j in 1:49){
    
  withdrawal_base[i+1,paste0("Age ",25+j)] = withdrawal_age[i,j]/withdrawal_base[i,"Population"]
  }
}

withdraw <- pop_raw %>%
  filter(Policy.Type == "T20") %>%
  mutate()

plot(withdrawal_base$`Withdrawal Rate`)
#====================Withdrawal Rates based on Gender=========================#
for(i in 1:23) {

    withdraw.m = pop_raw %>%
      filter(Year.of.Lapse == 2000 + i) %>%
      filter(Sex == "M") %>%
      nrow()
    
    withdraw.f = pop_raw %>%
      filter(Year.of.Lapse == 2000 + i) %>%
      filter(Sex == "F") %>%
      nrow()
    
    withdrawal_base[1+i,"Male"] = withdraw.m/withdrawal_base[i,"Population"]
    withdrawal_base[1+i,"Female"] = withdraw.f/withdrawal_base[i,"Population"]
    
}

ggplot(data = withdrawal_base, aes(x = Year)) +
  geom_line(aes(y = Male), colour = "blue") + 
  geom_line(aes(y = Female), colour = "red") + 
  labs(title = "Withdrawals based on Gender over Time", y = "Withdrawal Rate") + 
  theme_classic()

#====================Withdrawal Rates based on Smoking=========================#
for(i in 1:23) {
  
  withdraw.s = pop_raw %>%
    filter(Year.of.Lapse == 2000 + i) %>%
    filter(Smoker.Status == "S") %>%
    nrow()
  
  withdraw.ns = pop_raw %>%
    filter(Year.of.Lapse == 2000 + i) %>%
    filter(Smoker.Status == "NS") %>%
    nrow()
  
  withdrawal_base[1+i,"Smoking"] = withdraw.s/withdrawal_base[i,"Population"]
  withdrawal_base[1+i,"NonSmoking"] = withdraw.ns/withdrawal_base[i,"Population"]
  
}

ggplot(data = withdrawal_base, aes(x = Year)) +
  geom_line(aes(y = Smoking), colour = "blue") + 
  geom_line(aes(y = NonSmoking), colour = "red") + 
  labs(title = "Withdrawals based on Smoking Status over Time", y = "Withdrawal Rate") + 
  theme_classic()

#==============Withdrawal Rates based on Distribution Channel===================#
for(i in 1:23) {
  
  withdraw.t = pop_raw %>%
    filter(Year.of.Lapse == 2000 + i) %>%
    filter(Distribution.Channel == "Telemarketer") %>%
    nrow()
  
  withdraw.a = pop_raw %>%
    filter(Year.of.Lapse == 2000 + i) %>%
    filter(Distribution.Channel == "Agent") %>%
    nrow()
  
  withdraw.o = pop_raw %>%
    filter(Year.of.Lapse == 2000 + i) %>%
    filter(Distribution.Channel == "Online") %>%
    nrow()
  
  withdrawal_base[1+i,"Telemarketer"] = withdraw.t/withdrawal_base[i,"Population"]
  withdrawal_base[1+i,"Agent"] = withdraw.a/withdrawal_base[i,"Population"]
  withdrawal_base[1+i,"Online"] = withdraw.o/withdrawal_base[i,"Population"]

  }

ggplot(data = withdrawal_base, aes(x = Year)) +
  geom_line(aes(y = Telemarketer), colour = "blue") + 
  geom_line(aes(y = Agent), colour = "red") + 
  geom_line(aes(y = Online), colour = "purple") + 
  labs(title = "Withdrawals based on Distribution Channel over Time", y = "Withdrawal Rate") + 
  theme_classic()

#==============Withdrawal Rates based on Duration===================#

pop_duration = pop_raw %>%
  mutate(Duration = Year.of.Lapse - Issue.year + 1)

withdrawal_duration = as.data.frame(c(2001:2023))
colnames(withdrawal_duration)[1] = "Year"

for(i in 1:23) {
  for(j in 1:20){
    
  withdraw = pop_duration %>%
    filter(Issue.year == 2000 + i) %>%
    filter(Policy.type == "T20") %>%
    filter(Duration == j) %>%
    nrow()
  
  
  withdrawal_duration[i,paste0("T20 Year ", j)] = withdraw/withdrawal_base[i,"Population"]
  
  }
}

withdrawal_duration = as.data.frame(t(withdrawal_duration))  
withdrawal_duration = withdrawal_duration[-1,]
colnames(withdrawal_duration)[1:23] = c(paste0("Year",seq(2001,2023,1)))
withdrawal_duration$Duration = c(1:20)

ggplot(data = withdrawal_duration, aes(x = Duration)) +
  geom_line(aes(y = Year2005), colour = "blue") +
  theme_classic()
