# I make imputation files that exclude missing data if the entire wave is missing

library(tidyverse)
library(here)
path <- here("mod_data", "df_for_imputation.csv")
df <- read_csv(path)
names <- colnames(df)[-which(colnames(df)=="kohort")]
  
path1 <- here("mod_data", "df_for_imputation_blimp_c1.csv")
path2 <- here("mod_data", "df_for_imputation_blimp_c2.csv")
df_c1 <- read_csv(path1, na = "-999", col_names = names)
df_c2 <- read_csv(path2, na = "-999", col_names = names)

# tabulate a number of missing values per row,
# the key value is 64 
rowSums(is.na(df_c1)) %>% table() 
df_c1_filt <- dplyr::filter(df_c1, !rowSums(is.na(df_c1)) >= 64) 
df_c2_filt <- dplyr::filter(df_c2, !rowSums(is.na(df_c2)) >= 64)

path_c1 <- here("mod_data", "df_for_imputation_blimp_c1f.csv")
path_c2 <- here("mod_data", "df_for_imputation_blimp_c2f.csv")

write_csv(df_c1_filt, path_c1, na = "-999", col_names = FALSE)
write_csv(df_c2_filt, path_c2, na = "-999", col_names = FALSE)

# because the imputation of the file with the full set of variables has failed, 
# I will AMIS and food data separately.
# to employ impute by group feature in Blimp
# I selected age, gender, Wmaxkg, kcal, BMI, and AMIS items for initial imputation
df_AMIS_c1 <- df_c1_filt %>% 
  dplyr::select(1:4, 15:16, 44:67) %>% 
  dplyr::mutate_at(4:6, ~(scale(.x, scale = TRUE) %>% as.vector()))
df_AMIS_c2 <- df_c2_filt %>% 
  dplyr::select(1:4, 15:16, 44:67) %>% 
  dplyr::mutate_at(4:6, ~(scale(.x, scale = TRUE) %>% as.vector()))
path_c1_AMIS <- here("mod_data", "df_for_imputation_blimp_c1f_AMIS.csv")
path_c2_AMIS <- here("mod_data", "df_for_imputation_blimp_c2f_AMIS.csv")

# the rest of the data to impute without AMIS items
df_food_c1 <- df_c1_filt %>% 
  dplyr::select(-c(44:67)) %>% 
  dplyr::mutate_at(4:43, ~(scale(.x, scale = TRUE) %>% as.vector()))
df_food_c2 <- df_c2_filt %>% 
  dplyr::select(-c(44:67)) %>% 
  dplyr::mutate_at(4:43, ~(scale(.x, scale = TRUE) %>% as.vector()))
  
path_c1_food <- here("mod_data", "df_for_imputation_blimp_c1f_food.csv")
path_c2_food <- here("mod_data", "df_for_imputation_blimp_c2f_food.csv")

write_csv(df_AMIS_c1, path_c1_AMIS, na = "-999", col_names = FALSE)
write_csv(df_AMIS_c2, path_c2_AMIS, na = "-999", col_names = FALSE)

write_csv(df_food_c1, path_c1_food, na = "-999", col_names = FALSE)
write_csv(df_food_c2, path_c2_food, na = "-999", col_names = FALSE)

