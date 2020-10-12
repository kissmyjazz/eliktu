library(tidyverse)
library(here)

path <- here("mod_data", "df_for_imputation.csv")
df <- read_csv(path)

df_cohort1 <- df %>% dplyr::filter(kohort == 1) %>% dplyr::select(-kohort)
df_cohort2 <- df %>% dplyr::filter(kohort == 2) %>% dplyr::select(-kohort)

# number and proportion of missing values per variable
cohort1_missing <- cbind("# NA" = sort(colSums(is.na(df_cohort1)), decreasing = TRUE),
      "% NA" = round(sort(colMeans(is.na(df_cohort1)), decreasing = TRUE) * 100, 2))

cohort2_missing <- cbind("# NA" = sort(colSums(is.na(df_cohort2)), decreasing = TRUE),
                         "% NA" = round(sort(colMeans(is.na(df_cohort2)), decreasing = TRUE) * 100, 2))

