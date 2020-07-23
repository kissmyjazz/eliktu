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
data_vitEK <- data[, c(1, (n-3):n)]
multv_outliers(data_vitEK)

# vitamin D
data_vitD <- data[, c(1, (n-7):(n-5))]
multv_outliers(data_vitD)

# vitamin C
data_vitC <- data[, c(1, (n-10):(n-8))]
multv_outliers(data_vitC, print_n = TRUE)

# vitamin B6
data_vitB6 <- data[, c(1, (n-13):(n-11))]
multv_outliers(data_vitB6, print_n = TRUE)

# vitamin B2
data_vitB2 <- data[, c(1, (n-16):(n-14))]
multv_outliers(data_vitB2, print_n = TRUE)

# vitamin B12
data_vitB12 <- data[, c(1, (n-19):(n-17))]
multv_outliers(data_vitB12, print_n = TRUE)

# vitamin B1
data_vitB1 <- data[, c(1, (n-22):(n-20))]
multv_outliers(data_vitB1, print_n = TRUE)

# vitamin A
data_vitA <- data[, c(1, (n-25), (n-23))]
multv_outliers(data_vitA, print_n = TRUE)

# vegetables everything looks OK here
# data_veg <- data[, c(1, (n-28):(n-26))]
# multv_outliers(data_veg, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# sweets
data_sweets <- data[, c(1, (n-33):(n-31))]
multv_outliers(data_sweets, print_n = TRUE)

# salt and retinol
data_salt <- data[, c(1, (n-37), (n-36), (n-34))]
multv_outliers(data_salt, print_n = TRUE)

# protein everything looks OK here
# data_protein <- data[, c(1, (n-40):(n-38))]
# multv_outliers(data_protein, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# pantothen and poultry
# no data for younger cohort at 15 years of age, otherwise the data looks OK.
# data_panto <- data[, c(1, (n-46):(n-44))]
# multv_outliers(data_panto, print_n = TRUE)

# other drinks
data_drinks <- data[, c(1, (n-50):(n-48))]
multv_outliers(data_drinks, print_n = TRUE)

# niacin everything looks OK here
# data_niacin <- data[, c(1, (n-55), (n-53), (n-51))]
# multv_outliers(data_niacin, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# dairy everything looks OK here
# data_dairy <- data[, c(1, (n-60):(n-58))]
# multv_outliers(data_dairy, print_n = TRUE)

# meat everything looks OK here
# data_meat <- data[, c(1, (n-63):(n-61))]
# multv_outliers(data_meat, print_n = TRUE)

# lipids everything looks OK here
# data_lipids <- data[, c(1, (n-66):(n-64))]
# multv_outliers(data_lipids, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# LDL
data_ldl <- data[, c(1, (n-72):(n-70))]
multv_outliers(data_ldl, print_n = TRUE)

# HOMA
data_homa <- data[, c(1, (n-75):(n-73))]
multv_outliers(data_homa, print_n = TRUE)

# HDL everything looks OK here
# data_hdl <- data[, c(1, (n-78):(n-76))]
# multv_outliers(data_hdl, print_n = TRUE)

# fruits & berries
data_fruits <- data[, c(1, (n-81):(n-79))]
multv_outliers(data_fruits, print_n = TRUE)

# folate everything looks OK here
# data_folate <- data[, c(1, (n-84):(n-82))]
# multv_outliers(data_folate, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# fish data has a lot of 0 values, the multivariate test fails to converge.
# data_fish <- data[, c(1, (n-87):(n-85))]
# multv_outliers(data_fish, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# fats everything looks OK here
# data_fats <- data[, c(1, (n-90):(n-88))]
# multv_outliers(data_fats, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# eggs
data_eggs <- data[, c(1, (n-93):(n-91))]
multv_outliers(data_eggs, print_n = TRUE)

# chl
data_chl <- data[, c(1, (n-100):(n-98))]
multv_outliers(data_chl, print_n = TRUE)

# cereal everything looks OK here
# data_cereal <- data[, c(1, (n-103):(n-101))]
# multv_outliers(data_cereal, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# carbohydrates everything looks OK here
# data_carbo <- data[, c(1, (n-108):(n-106))]
# multv_outliers(data_carbo, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)

# biotin there are no scores for 18 years old subjects
data_biotin <- data[, c(1, (n-113):(n-112))]
multv_outliers(data_biotin, print_n = TRUE)

# alcohol data has a lot of 0 values, the multivariate test fails to converge.
# data_alco <- data[, c(1, (n-117):(n-115))]
# multv_outliers(data_alco, print_n = TRUE, tolBAC=0.00001, toladj=0.1, tolDDC=0.01)
