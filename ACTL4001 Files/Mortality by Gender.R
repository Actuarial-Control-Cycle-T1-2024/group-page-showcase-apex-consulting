#Import libraries
library(dplyr)
library(ggplot2)

#Import data
inforce_raw = read.csv("Case Data/2024-srcsc-superlife-inforce-dataset.csv")
mortality_table = read.csv("Case Data/srcsc-2024-lumaria-mortality-table-CLEANED.csv")
colnames(mortality_table)[1] = "Age"
colnames(inforce_raw)[1] = "Policy.number"
inforce_raw$Date.of.birth = inforce_raw$Issue.year - inforce_raw$Issue.age
inforce_raw$Age_2010 = 2010 - inforce_raw$Date.of.birth
inforce_raw[inforce_raw == "NA"] = NA
base_inforce = subset(inforce_raw, is.na(Year.of.Death) | Year.of.Death >= 2010)
mortality_table[,"mqx"] = NA
mortality_table[,"fqx"] = NA


#Find proportion of males in each age group
for(i in 1:120){
  
  x = base_inforce %>%
    filter(Age_2010 == i) %>%
    summarise(male.p = sum(Sex == "M")/n(),
              female.p = sum(Sex == "F")/n())
  
  m = as.numeric(x[1,"male.p"])
  f = as.numeric(x[1,"female.p"])
  
  mortality_table[i,"mqx"] = mortality_table[i,"Mortality.Rate"]*m
  mortality_table[i,"fqx"] = mortality_table[i,"Mortality.Rate"]*f

}

mortality_table[,"log_mqx"] = log(mortality_table$mqx)
mortality_table[,"log_fqx"] = log(mortality_table$fqx)
mortality_table[,"log_qx"] = log(mortality_table$Mortality.Rate)

ggplot(mortality_table) +
  geom_line(aes(x=Age, y=log_mqx, colour = "blue")) + 
  geom_line(aes(x = Age, y = log_fqx, colour = "red")) + 
  geom_line(aes(x = Age, y = log_qx, colour = "black")) +
  theme_classic() + 
  labs(title = "log(qx) vs Age by Gender", y = "log(qx)") + 
  xlim(c(0,100))+
  scale_color_manual(labels = c("Population", "Female", "Male"), values = c("black","red", "blue")) + 
  guides(colour = guide_legend("Legend"))
  