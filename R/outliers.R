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
data <- read_csv(path)[, c(1, 125:312)]
data <- data[, -grep("_9", colnames(data))]
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
