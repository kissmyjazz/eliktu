# application of mixed-effect modelling to the dataset imputed by blimp

library(here)
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
library(mice)
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

df_blimp <- df_food %>% dplyr::left_join(df_amis, by = c(".imp", "kood", "age"))
df_blimp <- df_blimp %>% group_by(.imp) %>% mutate(.id = row_number()) %>% 
  ungroup() %>% rowwise() %>% 
  dplyr::mutate(aImp = sum(c(AMIS3, (6-AMIS7), AMIS11, AMIS15, AMIS19, AMIS23, 
                             AMIS2, (6-AMIS6), AMIS10, AMIS14, AMIS18, AMIS22)),
                mImp = sum(c(AMIS1, AMIS5, AMIS9, AMIS13, AMIS17, (6-AMIS21),
                             AMIS4, AMIS12, AMIS16, AMIS20, AMIS24))) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-c(AMIS1:AMIS24)) %>% 
  dplyr::mutate(sugu = factor(sugu, levels = 0:1, labels = c("female", "male")), 
                kood = factor(kood), age_cent = (age - 18), 
                age_f = factor(age, levels = c(15, 18, 25, 33), 
                               labels = c("15 years", "18 years", "25 years", 
                                          "33 years"))) %>% 
  group_by(.imp, kohort) %>% 
  dplyr::mutate_at(c(6:45, 47:48), ~(scale(.x, scale = TRUE) %>% as.vector()))

# saved the data
# path <- here("imputed_data", "blimp", "scaled_joined_df.rds")
# saveRDS(df_blimp, path)

################################################################################
# Save as mids objects for other analysis
mids_1 <- df_blimp %>% dplyr::filter(kohort == 1) %>% 
  dplyr::select(-kohort) %>% as.mids()
mids_2 <- df_blimp %>% dplyr::filter(kohort == 2) %>% 
  dplyr::select(-kohort) %>% as.mids()

path_mids_1 <- here("imputed_data", "blimp", "mids_cohort_1.rds")
path_mids_2 <- here("imputed_data", "blimp", "mids_cohort_2.rds")

saveRDS(mids_1, path_mids_1)
saveRDS(mids_2, path_mids_2)
################################################################################
df_blimp0 <- df_blimp %>% dplyr::filter(.imp != 0) %>% dplyr::select(-.id)
df_blimp0_c1 <- df_blimp0 %>% dplyr::filter(kohort == 1) %>% 
  dplyr::select(-kohort)
df_blimp0_c2 <- df_blimp0 %>% dplyr::filter(kohort == 2) %>% 
  dplyr::select(-kohort)
implist_c1 <- as.mitml.list(split(df_blimp0_c1 , df_blimp0_c1$.imp))
implist_c2 <- as.mitml.list(split(df_blimp0_c2 , df_blimp0_c2$.imp))
names <- names(df_blimp0_c1)
# make formulas
fmla_aImp <- formula(paste("aImp ~ ", 
                           paste(names[-c(1:4, 46, 48)],collapse="+"), 
                           " + (1|kood)"))
fmla_mImp <- formula(paste("mImp ~ ", 
                           paste(names[-c(1:4, 47, 48)],collapse="+"),
                           " + (1|kood)"))
################################################################################
# cohort 1; Adaptive impulsivity
# Zink included
l_1_a_c1_Zink <- with(implist_c1, lmer(aImp ~ sugu + Wmaxkg + 
                                    Zink +  mImp + age_f + (1 | kood), REML = TRUE))
results_l_1_a_c1_Zink <- testEstimates(l_1_a_c1_Zink)

# full model
l_1_a_c1_int <- with(implist_c1, lmer(aImp ~ sugu + Wmaxkg + Calcium + Iodine + Iron + Magnesium + 
                                    Manganese + Phosphorus + Potassium + Selenium + Sodium + 
                                    Zink + kcal + BMI + Carb + Cerealprod + CHL + Eggs + Fatsg + 
                                    Fish + Folate + FruitsBerries + HDL + HOMA + LDL + Lipid + 
                                    Meat + Milk + Niacin + Protein + SugSweets + Veget + VitA + 
                                    VitB1 + VitB12 + VitB2 + VitB6 + VitC + VitD + VitE + Alco + 
                                    mImp + age_f + (1 | kood), REML = TRUE))
results_l_1_a_c1_int <- testEstimates(l_1_a_c1_int)

# VitB6 included
l_1_a_c1_VitB6 <- with(implist_c1, lmer(aImp ~ sugu + Sodium + BMI +
                                         VitB6 +  mImp + age_f + (1 | kood), REML = TRUE))
results_l_1_a_c1_VitB6 <- testEstimates(l_1_a_c1_VitB6)
################################################################################
# cohort 2; Adaptive impulsivity
# full model
l_1_a_c2_int <- with(implist_c2, lmer(aImp ~ sugu + Wmaxkg + Calcium + Iodine + Iron + Magnesium + 
                                        Manganese + Phosphorus + Potassium + Selenium + Sodium + 
                                        Zink + kcal + BMI + Carb + Cerealprod + CHL + Eggs + Fatsg + 
                                        Fish + Folate + FruitsBerries + HDL + HOMA + LDL + Lipid + 
                                        Meat + Milk + Niacin + Protein + SugSweets + Veget + VitA + 
                                        VitB1 + VitB12 + VitB2 + VitB6 + VitC + VitD + VitE + Alco + 
                                        mImp + age_f + (1 | kood), REML = TRUE))
results_l_1_a_c2_int <- testEstimates(l_1_a_c2_int)
################################################################################
# cohort 1; Maladaptive impulsivity
# full model
l_1_m_c1_int <- with(implist_c1, lmer(mImp ~ sugu + Wmaxkg + Calcium + Iodine + Iron + Magnesium + 
                                        Manganese + Phosphorus + Potassium + Selenium + Sodium + 
                                        Zink + kcal + BMI + Carb + Cerealprod + CHL + Eggs + Fatsg + 
                                        Fish + Folate + FruitsBerries + HDL + HOMA + LDL + Lipid + 
                                        Meat + Milk + Niacin + Protein + SugSweets + Veget + VitA + 
                                        VitB1 + VitB12 + VitB2 + VitB6 + VitC + VitD + VitE + Alco + 
                                        aImp + age_f + (1 | kood), REML = TRUE))
results_l_1_m_c1_int <- testEstimates(l_1_m_c1_int)
################################################################################
# cohort 2; Maladaptive impulsivity
# full model
l_1_m_c2_int <- with(implist_c2, lmer(mImp ~ sugu + Wmaxkg + Calcium + Iodine + Iron + Magnesium + 
                                        Manganese + Phosphorus + Potassium + Selenium + Sodium + 
                                        Zink + kcal + BMI + Carb + Cerealprod + CHL + Eggs + Fatsg + 
                                        Fish + Folate + FruitsBerries + HDL + HOMA + LDL + Lipid + 
                                        Meat + Milk + Niacin + Protein + SugSweets + Veget + VitA + 
                                        VitB1 + VitB12 + VitB2 + VitB6 + VitC + VitD + VitE + Alco + 
                                        aImp + age_f + (1 | kood), REML = TRUE))
results_l_1_m_c2_int <- testEstimates(l_1_m_c2_int)
################################################################################