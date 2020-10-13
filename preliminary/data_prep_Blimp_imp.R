library(tidyverse)
library(here)
library(miceadds)
path <- here("mod_data", "df_for_imputation.csv")

as_numeric_factor <- function(x) {as.numeric(levels(x))[x]}

df <- read_csv(path)
df <- df %>% dplyr::mutate(sugu = factor(sugu),
                           sugu = fct_recode(sugu, `0` = "female", `1` = "male"))

# check if value `-999` occurs in DF
any(df == -999) # No such values
df_cohort1 <- df %>% dplyr::filter(kohort == 1) %>% dplyr::select(-kohort) %>%
  dplyr::mutate(sugu = as_numeric_factor(sugu))
df_cohort2 <- df %>% dplyr::filter(kohort == 2) %>% dplyr::select(-kohort) %>%
  dplyr::mutate(sugu = as_numeric_factor(sugu))

# to employ impute by group feature in Blimp
df_joint <- df %>% dplyr::mutate(sugu = as_numeric_factor(sugu))

names <- colnames(df_cohort1)
names2 <- miceadds::VariableNames2String(names, breaks = 1000)
path_text <- here("mod_data", "colnames.txt")
file_conn<-file(path_text)
writeLines(names2, file_conn)
close(file_conn)

path1 <- here("mod_data", "df_for_imputation_blimp_c1.csv")
path2 <- here("mod_data", "df_for_imputation_blimp_c2.csv")

write_csv(df_cohort1, path1, na = "-999", col_names = FALSE)
write_csv(df_cohort2, path2, na = "-999", col_names = FALSE)

path3 <- here("mod_data", "df_for_imputation_blimp_joint.csv")
write_csv(df_joint, path3, na = "-999", col_names = FALSE)
