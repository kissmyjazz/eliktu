# fitting CFA with lavaan to imputed data to extract latent factor scores 
# of impulsivity 

library(here)
library(tidyverse)
library(lavaan)
library(semTools)
library(mitml)

options(scipen=999)
old <- theme_set(theme_bw())

path <- here("mod_data", "df_for_imputation.csv")
df <- read_csv(path)
names <- colnames(df) 
names_food <- c(".imp", names[1:44])
names_amis <- c(".imp", names[1:5], names[16:17], names[45:68])

path_food <- here("imputed_data", "blimp", 'imps_food.csv')
path_amis <- here("imputed_data", "blimp", 'imps_amis.csv') 

df_food <- read_csv(path_food, col_names = names_food)
df_food[df_food == -999] <- NA

df_amis <- read_csv(path_amis, col_names = names_amis) %>% 
  dplyr::select(-c(sugu, kohort, Wmaxkg, kcal, BMI))
df_amis[df_amis == -999] <- NA

df_blimp_l <- df_food %>% dplyr::left_join(df_amis, by = c(".imp", "kood", "age"))
df_blimp_l <- df_blimp_l %>% group_by(.imp) %>% mutate(.id = row_number()) %>% 
  ungroup() %>% 
  dplyr::mutate(age_f = factor(age, levels = c(15, 18, 25, 33), 
                               labels = c("15 years", "18 years", "25 years", 
                                          "33 years"))) %>% 
  # I exclude AMIS8 because of poor factor loadings 
  dplyr::ungroup() %>% 
  dplyr::select(.imp, .id, kohort, age_f, AMIS1:AMIS7, AMIS9:AMIS24) %>% 
  dplyr::rename(T_1 = AMIS1, E_2 = AMIS2, F_3 = AMIS3, D_4 = AMIS4, T_5 = AMIS5,
                E_6p = AMIS6, F_7p = AMIS7, T_9 = AMIS9, E_10 = AMIS10, F_11 = AMIS11,
                D_12 = AMIS12, T_13 = AMIS13, E_14 = AMIS14, F_15 = AMIS15, 
                D_16 = AMIS16, T_17 = AMIS17, E_18 = AMIS18, F_19 = AMIS19,
                D_20 = AMIS20, T_21p = AMIS21, E_22 = AMIS22, F_23 = AMIS23, 
                D_24 = AMIS24) %>% dplyr::filter(.imp != 0) %>% 
  dplyr::mutate_at(5:27, ordered)

df_blimp_lavaan_c1 <- df_blimp_l %>% dplyr::filter(kohort == 1) %>% 
  dplyr::select(-kohort)
df_blimp_lavaan_c2 <- df_blimp_l %>% dplyr::filter(kohort == 2) %>% 
  dplyr::select(-kohort)

# saved the data
# path_c1 <- here("imputed_data", "blimp", "lavaan_c1_df.rds")
# path_c2 <- here("imputed_data", "blimp", "lavaan_c2_df.rds")
# saveRDS(df_blimp_lavaan_c1, path_c1)
# saveRDS(df_blimp_lavaan_c2, path_c2)
################################################################################
implist_lav_c1 <- as.mitml.list(split(df_blimp_lavaan_c1, df_blimp_lavaan_c1$.imp))
implist_lav_c2 <- as.mitml.list(split(df_blimp_lavaan_c2, df_blimp_lavaan_c2$.imp))

# model of latent impulsivity factors
lav_mod <- '
lat_aImp =~ E_2 + F_3 + E_6p + F_7p + E_10 + F_11 + E_14 + F_15 + E_18 + F_19 + 
            E_22 + F_23
lat_mImp =~ T_1 + D_4 + T_5 + T_9 + D_12 + T_13 + D_16 + T_17 + D_20 + T_21p + 
            D_24'
################################################################################
cfa_fit_c1 <- cfa.mi(lav_mod, data = implist_lav_c1, 
                  estimator="WLSMV", ordered = TRUE, 
                  parameterization = "theta", group = "age_f")

mod_thresh_c1 <- measEq.syntax(configural.model = cfa_fit_c1,
                    ID.fac = "UL", ID.cat = "millsap.tein.2004", group = "age_f",
                    group.equal=c("thresholds"))
mod_metric_c1 <- measEq.syntax(configural.model = cfa_fit_c1,
                               ID.fac = "UL", ID.cat = "millsap.tein.2004", 
                               group = "age_f",
                               group.equal=c("thresholds", "loadings"))
mod_scalar_c1 <- measEq.syntax(configural.model = cfa_fit_c1,
                               ID.fac = "UL", ID.cat = "millsap.tein.2004", 
                               group = "age_f",
                               group.equal=c("thresholds", "loadings", "intercepts"))
################################################################################
cfa_fit_c2 <- cfa.mi(lav_mod, data = implist_lav_c2, 
                  estimator="WLSMV", ordered = TRUE, 
                  parameterization = "theta", group = "age_f")

mod_thresh_c2 <- measEq.syntax(configural.model = cfa_fit_c2,
                               ID.fac = "UL", ID.cat = "millsap.tein.2004", group = "age_f",
                               group.equal=c("thresholds"))
mod_metric_c2 <- measEq.syntax(configural.model = cfa_fit_c2,
                               ID.fac = "UL", ID.cat = "millsap.tein.2004", 
                               group = "age_f",
                               group.equal=c("thresholds", "loadings"))
mod_scalar_c2 <- measEq.syntax(configural.model = cfa_fit_c2,
                               ID.fac = "UL", ID.cat = "millsap.tein.2004", 
                               group = "age_f",
                               group.equal=c("thresholds", "loadings", "intercepts"))
################################################################################