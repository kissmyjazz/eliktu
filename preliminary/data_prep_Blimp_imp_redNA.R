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

write_csv(df_c1_filt, path_c1)
write_csv(df_c2_filt, path_c2)
