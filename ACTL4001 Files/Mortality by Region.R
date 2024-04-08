#Import libraries
library(dplyr)
library(ggplot2)
library(scales)

#Import data
inforce_raw = read.csv("Case Data/2024-srcsc-superlife-inforce-dataset.csv")
inforce_raw$Date.of.birth = inforce_raw$Issue.year - inforce_raw$Issue.age
inforce_raw$Age_2010 = 2010 - inforce_raw$Date.of.birth
inforce_raw[inforce_raw == "NA"] = NA

#Graph Regional Data
by_region = inforce_raw %>%
  group_by(Region) %>%
  count()

ggplot(data = by_region, aes(x = Region, y = n)) + 
  geom_bar(stat = "identity") + 
  labs(tite = "Number of policies per Region", y = "Count") + 
  scale_y_continuous(breaks = seq(0,300000,100000), labels = label_comma()) + 
  scale_x_continuous(breaks = seq(1,6,1)) +
  theme_classic()

#Mortality by region
mortality_table = read.csv("Case Data/srcsc-2024-lumaria-mortality-table-CLEANED.csv")
colnames(mortality_table)[1] = "Age"
base_inforce = subset(inforce_raw, is.na(Year.of.Death) | Year.of.Death >= 2010)
mortality_table[,"qx_1"] = NA
mortality_table[,"qx_2"] = NA
mortality_table[,"qx_3"] = NA
mortality_table[,"qx_4"] = NA
mortality_table[,"qx_5"] = NA
mortality_table[,"qx_6"] = NA

#Find proportion per region in each age group
for(i in 1:120){
  
  x = base_inforce %>%
    filter(Age_2010 == i) %>%
    summarise(Region_1 = sum(Region == "1")/n(),
              Region_2 = sum(Region == "2")/n(),
              Region_3 = sum(Region == "3")/n(),
              Region_4 = sum(Region == "4")/n(),
              Region_5 = sum(Region == "5")/n(),
              Region_6 = sum(Region == "6")/n())
  
  q1 = as.numeric(x[1,"Region_1"])
  q2 = as.numeric(x[1,"Region_2"])
  q3 = as.numeric(x[1,"Region_3"])
  q4 = as.numeric(x[1,"Region_4"])
  q5 = as.numeric(x[1,"Region_5"])
  q6 = as.numeric(x[1,"Region_6"])
  
  mortality_table[i,"qx_1"] = mortality_table[i,"Mortality.Rate"]*q1
  mortality_table[i,"qx_2"] = mortality_table[i,"Mortality.Rate"]*q2
  mortality_table[i,"qx_3"] = mortality_table[i,"Mortality.Rate"]*q3
  mortality_table[i,"qx_4"] = mortality_table[i,"Mortality.Rate"]*q4
  mortality_table[i,"qx_5"] = mortality_table[i,"Mortality.Rate"]*q5
  mortality_table[i,"qx_6"] = mortality_table[i,"Mortality.Rate"]*q6
  
}

mortality_table[,"log_qx_1"] = log(mortality_table$qx_1)
mortality_table[,"log_qx_2"] = log(mortality_table$qx_2)
mortality_table[,"log_qx_3"] = log(mortality_table$qx_3)
mortality_table[,"log_qx_4"] = log(mortality_table$qx_4)
mortality_table[,"log_qx_5"] = log(mortality_table$qx_5)
mortality_table[,"log_qx_6"] = log(mortality_table$qx_6)
mortality_table[,"log_qx"] = log(mortality_table$Mortality.Rate)

ggplot(mortality_table, aes(x = Age)) +
  geom_line(aes(y=log_qx_1, colour = "red"))+
  geom_line(aes(y=log_qx_2, colour = "blue"))+
  geom_line(aes(y=log_qx_3, colour = "pink")) +
  geom_line(aes(y=log_qx_4, colour = "purple")) +
  geom_line(aes(y=log_qx_5, colour = "darkgreen")) +
  geom_line(aes(y=log_qx_6, colour = "darkorange")) +
  geom_line(aes(y=log_qx, colour = "black")) +
  labs(title = "Mortality Rate by Region", y = "log_qx") +
  scale_color_manual(labels = c("Population","Region 2","Region 5", "Region 3", "Region 4", "Region 1", "Region 6"), values = c("black","blue","darkgreen","pink","purple","red","darkorange")) +
  theme_classic() +
  xlim(c(0,80))
              

