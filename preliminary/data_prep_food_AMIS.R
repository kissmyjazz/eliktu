library(here)
library(tidyverse)
library(reshape2)
library(psych)
options(scipen=999)

################################################################################
# minerals
path <- here("mod_data", "df_for_lmer_minerals.csv")
df_minerals <- read_csv(path)

# delete combined AMIS scales, age_cent & observations at age 9
df_minerals <- df_minerals %>% dplyr::select(-c(Aimp, Mimp, Excite, Fast, age_cent), -ends_with("_9"))

################################################################################
# AMIS
path <- here("mod_data", "AMIS_questionnaire_raw.csv")
df_amis <- read_csv(path)

# transform into long format
df_amis_l <- df_amis %>% pivot_longer(!kood, names_to = c(".value", "age"),
                                      names_sep = "_",
                                      names_transform = list(age = as.integer),
                                      values_drop_na = FALSE)

################################################################################
# food categories
# Retinol_eqv_18 for cohort 1 is combined with VitA_18 for cohort 2

path <- here("mod_data", "toitumine_wide_reduced.csv")
path1 <- here("mod_data", "AMIS_toitumine_wide.csv")

df_food <- read_csv(path) %>% dplyr::select(-ends_with("_9"))
df_food_a <- read_csv(path1) %>% dplyr::select(kood, Retinol_eqv_18)
df_food <- df_food %>% left_join(df_food_a, by="kood") %>%
  dplyr::mutate(VitA_18 = dplyr::coalesce(VitA_18, Retinol_eqv_18),
                Retinol_eqv_18 = NULL)

# transform into long format
df_food_l <- df_food %>% pivot_longer(!kood, names_to = c(".value", "age"),
                                      names_sep = "_",
                                      names_transform = list(age = as.integer),
                                      values_drop_na = FALSE)
################################################################################
# pure alcohol
path <- here("mod_data", "ELIKTU puhas alkohol.csv")
df_alcohol <- read_csv(path) %>% dplyr::select(-ends_with("_9"), -c(sugu, kohort))

# transform into long format
df_alcohol_l <- df_alcohol %>% pivot_longer(!kood, names_to = c(".value", "age"),
                                      names_sep = "_",
                                      names_transform = list(age = as.integer),
                                      values_drop_na = FALSE)

################################################################################
# join files together to make a final long format file for imputation

df <- df_minerals %>% left_join(df_food_l, by = c("kood", "age")) %>%
  left_join(df_alcohol_l, by = c("kood", "age")) %>%
  left_join(df_amis_l, by = c("kood", "age"))

# cohort 1 does not have impulsivity scores for age 33, hence I filter data for this age out
# cohort 2 does not have impulsivity scores for age 15, hence I filter data for this age out

df <- df %>% dplyr::filter(!(kohort == 1 & age == 33)) %>% dplyr::filter(!(kohort == 2 & age == 15))

path <- here("mod_data", "df_for_imputation.csv")
write_csv(df, path)
