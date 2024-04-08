setwd("C:/Users/grace/Desktop/Uni/Actuarial Studies/ACTL4001 (2024 Term 1)/SOA Group Assignment/01Data")

library("lifecontingencies")
library("dplyr")
lumaria_qx <- read.csv("srcsc-2024-lumaria-mortality-table-CLEANED.csv", header = TRUE)
lumaria_qx <- lumaria_qx %>%
  mutate(px = 1 - Mortality.Rate)

life_table <- probs2lifetable(probs = lumaria_qx$Mortality.Rate, 
                              type = "qx", 
                              radix = 100000, 
                              name = "Lumaria_Life_Table")


v <- 1/1.05

assurance <- data.frame(
  x = c(25:80)
)


calculate_sum <- function(x, v) {
  px <- lumaria_qx$px
  qx <- 1 - px
  sum_val <- 0
  for (i in 1:20) {
    prod_val <- prod(px[x:(i + x - 1)])
    sum_val <- sum_val + v^(i) * prod_val * qx[i + x]
  }
  return(sum_val)
}

face_amt <- 100000
face_amt*calculate_sum(x = 35, v = 1/1.04)

calculate_annuity <- function(x, v) {
  px <- lumaria_qx$px
  qx <- 1 - px
  sum_val <- 1
  for (i in 1:20){
    prod_val <- prod(px[x:(i + x - 1)])
    sum_val <- sum_val + v^(i) * prod_val
  }
  return(sum_val)
}

calculate_annuity(x = 35, v = 1/1.04)

(face_amt*calculate_sum(x = 35, v = 1/1.04)) / calculate_annuity(x = 35, v = 1/1.04)


# NEW
prod_px <- function(x, k){
  product <- prod(lumaria_qx$px[x:(k + x - 1)]) 
  return(product)
}

calculate_sum <- function(x, v) {
  px <- lumaria_qx$px
  qx <- 1 - px
  sum_val <- v*qx[x]
  for (i in 1:19) {
    sum_val <- sum_val + (prod_px(x, i) * qx[x+i] * v^(i+1))
  }
  return(sum_val)
}

face_amt <- 100000
face_amt*calculate_sum(x = 35, v = 1/1.04)

