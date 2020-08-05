library(here)
library(tidyverse)
library(OutlierDetection)
options(scipen=999)
path <- here("mod_data", "AMIS_toitumine_wide.csv")
data <- read_csv(path)
data <- data[, -grep("_9", colnames(data))]
col_names <- colnames(data)

# Univariate analysis of outliers
col_n <- 214
col_names[col_n]
test_data <- data[ ,col_n , drop = TRUE]
test_data <- test_data[!is.na(test_data)]
min(test_data)
# print(data[which(test_data < 0.2), 1])
g <- UnivariateOutlierDetection(test_data, cutoff=.99, dens = FALSE, depth = TRUE, dist = TRUE,
                           Method="euclidean",rnames=FALSE)
g$`Scatter plot` + labs(x = "id", y = "value", title = col_n)


