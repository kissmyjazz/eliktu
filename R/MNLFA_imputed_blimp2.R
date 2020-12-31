library(aMNLFA)
library(MplusAutomation)
library(tidyverse)
library(here)

data("xstudy")
glimpse(xstudy)


# load data --------------------------------------------------------------
path <- here("mod_data", "df_for_imputation.csv")
df <- read_csv(path)
names <- colnames(df)
names_amis <- c(".imp", names[1:3], names[5], names[16:17], names[45:68])
path_amis_c1 <- here("imputed_data", "blimp", 'imps_c1f_AMIS.csv')
path_amis_c2 <- here("imputed_data", "blimp", 'imps_c2f_AMIS.csv')
df_amis_c1 <- read_csv(path_amis_c1, col_names = names_amis) %>%
  dplyr::select(-c(Wmaxkg, kcal, BMI))
df_amis_c2 <- read_csv(path_amis_c2, col_names = names_amis) %>%
  dplyr::select(-c(Wmaxkg, kcal, BMI))
df_amis <- bind_rows(list(df_amis_c1, df_amis_c2))
df_amis[df_amis == -999] <- NA

# take 2nd imputation
df_amis2 <- df_amis %>% dplyr::filter(.imp == 2)

# centre the age and recode sex into contrast coding: female -1, male 1
df_amis2 <- df_amis2 %>% dplyr::mutate(sugu = ifelse(sugu == 0, -1L, 1L),
                                       age = age - 18L) %>% 
  dplyr::select(-c(.imp, kood)) 

aImp_names <- c("AMIS2", "AMIS3", "AMIS6", "AMIS7", "AMIS10", "AMIS11", "AMIS14",
                "AMIS15", "AMIS18", "AMIS19", "AMIS22", "AMIS23")
mImp_names <- c("AMIS1", "AMIS4", "AMIS5", "AMIS9", "AMIS12", "AMIS13",
                "AMIS16", "AMIS17", "AMIS20", "AMIS21", "AMIS24")

