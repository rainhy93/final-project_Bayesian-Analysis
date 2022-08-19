library(dplyr)
library(na.tools)

data = read.csv("STAT578 Final Project.csv", header = TRUE)
data_new = na.rm(data)

household_counts = count(data_new, HHID)

sort(household_counts$n, decreasing = T)

household_counts[which(household_counts$n > 2),]

data_new$old_agebin[data_new$age >= 65 & data_new$age < 75] <- 1
data_new$old_agebin[data_new$age >= 75 & data_new$age < 85] <- 2
data_new$old_agebin[data_new$age >= 85 & data_new$age < 100] <- 3

hist(data_new$old_agebin)

data_new$old_agebin = as.factor(data_new$old_agebin)


