library(here)
library(tidyverse)
library(OutlierDetection)
options(scipen=999)
path <- here("mod_data", "AMIS_toitumine_wide.csv")
data <- read_csv(path)
data <- data[, -grep("_9", colnames(data))]
col_names <- colnames(data)

# Univariate analysis of outliers
col_n <- 25
col_names[col_n]
test_data <- data[ ,col_n , drop = TRUE]
test_data <- test_data[!is.na(test_data)]
min(test_data)
# print(data[which(test_data < 0.2), 1])
g <- UnivariateOutlierDetection(test_data, cutoff=.99, dens = FALSE, depth = TRUE, dist = TRUE,
                           Method="euclidean",rnames=FALSE)
g$`Scatter plot` + labs(x = "id", y = "value", title = col_n)

# plot of Nutridata against questionnaire scores
data$Veget_c_25 <- cut(data$Veget_25, 3, labels = c("1", "2", "3"),
                          ordered_result = TRUE)
veget_data_25 <- data %>% select(Veget_c_25, köögivili_25) %>% filter(!is.na(.))
ggplot(veget_data_25, aes(x = Veget_c_25, y = köögivili_25)) +
  geom_jitter(alpha = 0.5, width = 0.16, height = 0.16) +
  labs(x = "vegetables Nutridata", y = "vegetables questionnaire", title = "25 years of age")
