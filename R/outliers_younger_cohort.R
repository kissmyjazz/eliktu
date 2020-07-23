# The following analysis is applicable to the younger cohort only

library(here)
library(tidyverse)
library(psych)
library(rJava)
library(xlsx)
library(ggthemes)
library(gridExtra)
library(OutliersO3)
options(scipen=999)
path <- here("mod_data", "AMIS_toitumine_wide.csv")
data <- read_csv(path)[, c(1:3, 125:312)]
data <- subset(data, kohort==1, -c(2, 3))
data <- data[, -grep("_9", colnames(data))]
data <- data[, -grep("_33", colnames(data))]
data <- cbind(data[, 1], data[, -1][, order(colnames(data[-1]))])
n <- length(colnames(data))

multv_outliers <- function(data, print_n = FALSE, tolBAC=0.000000001,
                           toladj=0.05, tolDDC=0.0001) {
  chunk <- na.omit(data)
  O3d <- O3prep(chunk[, -1], method=c("BAC", "adjOut", "DDC"),
                tolBAC=tolBAC, toladj=toladj, tolDDC=tolDDC)
  O3d1 <- O3plotM(O3d, caseNames = chunk$kood)
  if(print_n) {print(O3d1$nOut)}
  grid.arrange(O3d1$gO3 + theme(plot.margin = unit(c(0, 1, 0, 0), "cm")), O3d1$gpcp, ncol=1, heights=c(2,1))
}

# vitamins E and K
data_vitEK <- data[, c(1, (n-5):n)]
multv_outliers(data_vitEK)

# vitamin D everything looks OK here
# data_vitD <- data[, c(1, (n-10):(n-6))]
# multv_outliers(data_vitD)

# vitamins C & B6
data_vitC <- data[, c(1, (n-18):(n-11))]
multv_outliers(data_vitC, print_n = TRUE)

# vitamin B2
data_vitB2 <- data[, c(1, (n-22):(n-19))]
multv_outliers(data_vitB2, print_n = TRUE)

# vitamin B12
data_vitB12 <- data[, c(1, (n-26):(n-23))]
multv_outliers(data_vitB12, print_n = TRUE)

# vitamin B1
data_vitB1 <- data[, c(1, (n-30):(n-27))]
multv_outliers(data_vitB1, print_n = TRUE)

# vitamin A
data_vitA <- data[, c(1, (n-34):(n-31))]
multv_outliers(data_vitA, print_n = TRUE)

# vegetables
data_veg <- data[, c(1, (n-38):(n-35))]
multv_outliers(data_veg, print_n = TRUE)

# sweets everything looks OK here
# data_sweets <- data[, c(1, (n-44):(n-41))]
# multv_outliers(data_sweets, print_n = TRUE)

# salt and retinol looks OK here
# data_salt <- data[, c(1, (n-49), (n-46):(n-45))]
# multv_outliers(data_salt, print_n = TRUE)

# protein everything looks OK here
# data_protein <- data[, c(1, (n-53):(n-50))]
# multv_outliers(data_protein, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# poultry serious issues with the data
# data_poultry <- data[, c(1, (n-59):(n-58))]
# multv_outliers(data_poultry, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# pantothen no data for older cohort at 18 years of age, otherwise the data looks OK.
# data_panto <- data[, c(1, (n-61):(n-60))]
# multv_outliers(data_panto, print_n = TRUE)

# other drinks everything looks OK here
# data_drinks <- data[, c(1, (n-67):(n-64))]
# multv_outliers(data_drinks, print_n = TRUE)

# niacin everything looks OK here
# data_niacin <- data[, c(1, (n-73):(n-70))]
# multv_outliers(data_niacin, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# dairy
data_dairy <- data[, c(1, (n-79):(n-76))]
multv_outliers(data_dairy, print_n = TRUE)

# meat everything looks OK here
# data_meat <- data[, c(1, (n-83):(n-80))]
# multv_outliers(data_meat, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# lipids everything looks OK here
# data_lipids <- data[, c(1, (n-87):(n-84))]
# multv_outliers(data_lipids, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# LDL
data_ldl <- data[, c(1, (n-95):(n-92))]
multv_outliers(data_ldl, print_n = TRUE)

# HOMA
data_homa <- data[, c(1, (n-99):(n-96))]
multv_outliers(data_homa, print_n = TRUE)

# HDL everything looks OK here
# data_hdl <- data[, c(1, (n-103):(n-100))]
# multv_outliers(data_hdl, print_n = TRUE)

# fruits & berries
data_fruits <- data[, c(1, (n-107):(n-104))]
multv_outliers(data_fruits, print_n = TRUE)

# folate
data_folate <- data[, c(1, (n-111):(n-108))]
multv_outliers(data_folate, print_n = TRUE)

# fish data has a lot of 0 values, the multivariate test fails to converge.
# data_fish <- data[, c(1, (n-115):(n-112))]
# multv_outliers(data_fish, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# fats everything looks OK here
data_fats <- data[, c(1, (n-119):(n-116))]
multv_outliers(data_fats, print_n = TRUE)

# eggs
data_eggs <- data[, c(1, (n-123):(n-120))]
multv_outliers(data_eggs, print_n = TRUE)

# chl
data_chl <- data[, c(1, (n-131):(n-128))]
multv_outliers(data_chl, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# cereal everything looks OK here
# data_cereal <- data[, c(1, (n-135):(n-132))]
# multv_outliers(data_cereal, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# carbohydrates
data_carbo <- data[, c(1, (n-142):(n-139))]
multv_outliers(data_carbo, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# biotin there are no scores for 18 years old subjects
data_biotin <- data[, c(1, (n-148):(n-147))]
multv_outliers(data_biotin, print_n = TRUE)

# alcohol data has a lot of 0 values, the multivariate test fails to converge.
# data_alco <- data[, c(1, (n-152):(n-151))]
# multv_outliers(data_alco, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)
