#Import libraries
library(dplyr)
library(ggplot2)

#Import data and clean
pop_raw = read.csv("Case Data/2024-srcsc-superlife-inforce-dataset.csv")
colnames(pop_raw)[1] = "Policy.Number"

mortality_table = read.csv("Case Data/srcsc-2024-lumaria-mortality-table-CLEANED.csv")
colnames(mortality_table)[1] = "Age"

# Y.O.B and Survival Time
pop_raw$Year.of.Birth = pop_raw$Issue.year - pop_raw$Issue.age
pop_raw$Base.Age = 2010 - pop_raw$Year.of.Birth

#Population Total
pop_total = nrow(pop_raw)

#=============================One Variable=====================================#
#Gender

pop_male = sum(pop_raw$Sex == "M")/pop_total
pop_female = sum(pop_raw$Sex == "F")/pop_total

for(i in 1:120){
    x = pop_raw %>%
      filter(Base.Age == i) %>%
      summarise(male.p = sum(Sex == "M")/n(),
                female.p = sum(Sex == "F")/n())
    
    m = as.numeric(x[1,"male.p"])
    f = as.numeric(x[1,"female.p"])
    
    mortality_table[i,"m.qx"] = mortality_table[i,"Mortality.Rate"]*m/pop_male
    mortality_table[i,"f.qx"] = mortality_table[i,"Mortality.Rate"]*f/pop_female
}

#Smoking Status

pop_smoke = sum(pop_raw$Smoker.Status == "S")/pop_total
pop_nonsmoke = sum(pop_raw$Smoker.Status == "NS")/pop_total

for(i in 1:120){
  x = pop_raw %>%
    filter(Base.Age == i) %>%
    summarise(smoke.p = sum(Smoker.Status == "S")/n(),
              nonsmoke.p = sum(Smoker.Status == "NS")/n())
  
  smoker = as.numeric(x[1,"smoke.p"])
  nonsmoker = as.numeric(x[1,"nonsmoke.p"])
  
  mortality_table[i,"smoke.qx"] = mortality_table[i,"Mortality.Rate"]*smoker/pop_smoke
  mortality_table[i,"nonsmoke.qx"] = mortality_table[i,"Mortality.Rate"]*nonsmoker/pop_nonsmoke
}

#Regional

pop_R1 = sum(pop_raw$Region == 1)/pop_total
pop_R2 = sum(pop_raw$Region == 2)/pop_total
pop_R3 = sum(pop_raw$Region == 3)/pop_total
pop_R4 = sum(pop_raw$Region == 4)/pop_total
pop_R5 = sum(pop_raw$Region == 5)/pop_total
pop_R6 = sum(pop_raw$Region == 6)/pop_total

for (i in 1:120){
  x = pop_raw %>%
    filter(Base.Age == i) %>%
    summarise(R1.p = sum(Region == 1)/n(),
              R2.p = sum(Region == 2)/n(),
              R3.p = sum(Region == 3)/n(),
              R4.p = sum(Region == 4)/n(),
              R5.p = sum(Region == 5)/n(),
              R6.p = sum(Region == 6)/n())
  
  R1 = as.numeric(x[1,"R1.p"])
  R2 = as.numeric(x[1,"R2.p"])
  R3 = as.numeric(x[1,"R3.p"])
  R4 = as.numeric(x[1,"R4.p"])
  R5 = as.numeric(x[1,"R5.p"])
  R6 = as.numeric(x[1,"R6.p"])
  
  mortality_table[i,"R1.qx"] = mortality_table[i,"Mortality.Rate"]*R1/pop_R1
  mortality_table[i,"R2.qx"] = mortality_table[i,"Mortality.Rate"]*R2/pop_R2
  mortality_table[i,"R3.qx"] = mortality_table[i,"Mortality.Rate"]*R3/pop_R3
  mortality_table[i,"R4.qx"] = mortality_table[i,"Mortality.Rate"]*R4/pop_R4
  mortality_table[i,"R5.qx"] = mortality_table[i,"Mortality.Rate"]*R5/pop_R5
  mortality_table[i,"R6.qx"] = mortality_table[i,"Mortality.Rate"]*R6/pop_R6
}

#Policy Type

pop_term = sum(pop_raw$Policy.Type == "T20")/pop_total
pop_whole = sum(pop_raw$Policy.Type == "SPWL")/pop_total

for(i in 1:120){
  x = pop_raw %>%
    filter(Base.Age == i) %>%
    summarise(term.p = sum(Policy.Type == "T20")/n(),
              whole.p = sum(Policy.Type == "SPWL")/n())
  
  term = as.numeric(x[1,"term.p"])
  whole = as.numeric(x[1,"whole.p"])
  
  mortality_table[i,"term.qx"] = mortality_table[i,"Mortality.Rate"]*term/pop_term
  mortality_table[i,"whole.qx"] = mortality_table[i,"Mortality.Rate"]*whole/pop_whole
}

#Face Value

pop_50K = sum(pop_raw$Face.amount == 50000)/pop_total
pop_100K = sum(pop_raw$Face.amount == 100000)/pop_total
pop_250K = sum(pop_raw$Face.amount == 250000)/pop_total
pop_500K = sum(pop_raw$Face.amount == 500000)/pop_total
pop_1mil = sum(pop_raw$Face.amount == 1000000)/pop_total
pop_2mil = sum(pop_raw$Face.amount == 2000000)/pop_total

for(i in 1:120){
  x = pop_raw %>%
    filter(Base.Age == i) %>%
    summarise(FV50K.p = sum(Face.amount == 50000)/n(),
              FV100K.p = sum(Face.amount == 100000)/n(),
              FV250K.p = sum(Face.amount == 250000)/n(),
              FV500K.p = sum(Face.amount == 500000)/n(),
              FV1mil.p = sum(Face.amount == 1000000)/n(),
              FV2mil.p = sum(Face.amount == 2000000)/n())
  
  FV50K = as.numeric(x[1,"FV50K.p"])
  FV100K = as.numeric(x[1,"FV100K.p"])
  FV250K = as.numeric(x[1,"FV250K.p"])
  FV500K = as.numeric(x[1,"FV500K.p"])
  FV1mil = as.numeric(x[1,"FV1mil.p"])
  FV2mil = as.numeric(x[1,"FV2mil.p"])
  
  mortality_table[i,"FV50K.qx"] = mortality_table[i,"Mortality.Rate"]*FV50K/pop_50K
  mortality_table[i,"FV100K.qx"] = mortality_table[i,"Mortality.Rate"]*FV100K/pop_100K
  mortality_table[i,"FV250K.qx"] = mortality_table[i,"Mortality.Rate"]*FV250K/pop_250K
  mortality_table[i,"FV500K.qx"] = mortality_table[i,"Mortality.Rate"]*FV500K/pop_500K
  mortality_table[i,"FV1mil.qx"] = mortality_table[i,"Mortality.Rate"]*FV1mil/pop_1mil
  mortality_table[i,"FV2mil.qx"] = mortality_table[i,"Mortality.Rate"]*FV2mil/pop_2mil
}

#=============================Two Variable=====================================#
#Gender and Smoking

pop_M_S = sum(pop_raw$Sex == "M" & pop_raw$Smoker.Status == "S")/pop_total
pop_M_NS = sum(pop_raw$Sex == "M" & pop_raw$Smoker.Status == "NS")/pop_total
pop_F_S =  sum(pop_raw$Sex == "F" & pop_raw$Smoker.Status == "S")/pop_total
pop_F_NS = sum(pop_raw$Sex == "F" & pop_raw$Smoker.Status == "NS")/pop_total

for(i in 1:120){
  x = pop_raw %>%
    filter(Base.Age == i) %>%
    summarise(M_S.p = sum(Sex == "M" & Smoker.Status == "S")/n(),
              M_NS.p = sum(Sex == "M" & Smoker.Status == "NS")/n(),
              F_S.p = sum(Sex == "F" & Smoker.Status == "S")/n(),
              F_NS.p = sum(Sex == "F" & Smoker.Status == "NS")/n())
  
  M_S = as.numeric(x[1,"M_S.p"])
  M_NS = as.numeric(x[1,"M_NS.p"])
  F_S = as.numeric(x[1,"F_S.p"])
  F_NS = as.numeric(x[1,"F_NS.p"])
  
  mortality_table[i,"M_S.qx"] = mortality_table[i,"Mortality.Rate"]*M_S/pop_M_S
  mortality_table[i,"M_NS.qx"] = mortality_table[i,"Mortality.Rate"]*M_NS/pop_M_NS
  mortality_table[i,"F_S.qx"] = mortality_table[i,"Mortality.Rate"]*F_S/pop_F_S
  mortality_table[i,"F_NS.qx"] = mortality_table[i,"Mortality.Rate"]*F_NS/pop_F_NS
}

#MATRIX
smokenames = c("Smoker", "Non Smoker")
sexnames = c("Male", "Female")
agenames = c(as.character(1:120))

matrix_vector = c(t(as.matrix(mortality_table[,21:24])))
array = array(matrix_vector, dim=c(2,2,120), dimnames = list(smokenames, sexnames, agenames))
array[,,13:15]
