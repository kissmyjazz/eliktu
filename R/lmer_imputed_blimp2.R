# analysis of blimp imputation data where cases with missing data for an entire 
# wave were not imputed
library(here)
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
library(mice)
library(merTools)
library(mitml)
library(lmerTest)
options(scipen=999)
old <- theme_set(theme_bw())

path <- here("mod_data", "df_for_imputation.csv")
df <- read_csv(path)
names <- colnames(df) 
names_food <- c(".imp", names[c(1:3, 5:44)])
names_amis <- c(".imp", names[1:3], names[5], names[16:17], names[45:68])

path_food_c1 <- here("imputed_data", "blimp", 'imps_c1f_food.csv')
path_food_c2 <- here("imputed_data", "blimp", 'imps_c2f_food.csv')

path_amis_c1 <- here("imputed_data", "blimp", 'imps_c1f_AMIS.csv')
path_amis_c2 <- here("imputed_data", "blimp", 'imps_c2f_AMIS.csv') 

df_food_c1 <- read_csv(path_food_c1, col_names = names_food)
df_food_c2 <- read_csv(path_food_c2, col_names = names_food)
df_food <- bind_rows(list(df_food_c1, df_food_c2), .id = "kohort")
df_food[df_food == -999] <- NA

df_amis_c1 <- read_csv(path_amis_c1, col_names = names_amis) %>% 
  dplyr::select(-c(sugu, Wmaxkg, kcal, BMI))
df_amis_c2 <- read_csv(path_amis_c2, col_names = names_amis) %>% 
  dplyr::select(-c(sugu, Wmaxkg, kcal, BMI))
df_amis <- bind_rows(list(df_amis_c1, df_amis_c2))
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
                                          "33 years")))

# saved the data
# path <- here("imputed_data", "blimp", "scaled_joined_df2.rds")
# saveRDS(df_blimp, path)

################################################################################
# Save as mids objects for other analysis
mids_1 <- df_blimp %>% dplyr::filter(kohort == 1) %>% 
  dplyr::select(-kohort) %>% as.mids()
mids_2 <- df_blimp %>% dplyr::filter(kohort == 2) %>% 
  dplyr::select(-kohort) %>% as.mids()

path_mids_1 <- here("imputed_data", "blimp", "mids_ver2_cohort_1.rds")
path_mids_2 <- here("imputed_data", "blimp", "mids_ver2_cohort_2.rds")

# saveRDS(mids_1, path_mids_1)
# saveRDS(mids_2, path_mids_2)
################################################################################
df_blimp0 <- df_blimp %>% dplyr::filter(.imp != 0) %>% dplyr::select(-.id)
df_blimp0_c1 <- df_blimp0 %>% dplyr::filter(kohort == 1) %>% 
  dplyr::select(-kohort)
df_blimp0_c2 <- df_blimp0 %>% dplyr::filter(kohort == 2) %>% 
  dplyr::select(-kohort)
implist_c1 <- as.mitml.list(split(df_blimp0_c1 , df_blimp0_c1$.imp))
implist_c2 <- as.mitml.list(split(df_blimp0_c2 , df_blimp0_c2$.imp))
names <- names(df_blimp0_c1)
implist_c_both <- as.mitml.list(split(df_blimp0 , df_blimp0$.imp))

# make formulas
fmla_aImp <- formula(paste("aImp ~ ", 
                           paste(names[-c(1:3, 16, 18, 20, 45, 48)], collapse="+"), 
                           " + (1|kood)"))
fmla_aImp_slope <- formula(paste("aImp ~ ", 
                           paste(names[-c(1:3, 16, 18, 20, 45, 48)], collapse="+"), 
                           " + (age_cent|kood)"))
fmla_aImp_slope_interac <- formula(paste("aImp ~ (", 
                                 paste(names[-c(1:3, 16, 18, 20, 45, 47, 48)],
                                 collapse="+"), 
                                 ") * age_cent + (age_cent|kood)"))
fmla_mImp <- formula(paste("mImp ~ ", 
                           paste(names[-c(1:3, 16, 18, 20, 46, 48)], collapse="+"),
                           " + (1|kood)"))
fmla_mImp_slope <- formula(paste("mImp ~ ", 
                                 paste(names[-c(1:3, 16, 18, 20, 46, 48)], collapse="+"),
                                 " + (age_cent|kood)"))
################################################################################
# cohort 1; Adaptive impulsivity
# Zink included
l_1_a_c1_Zink <- with(implist_c1, lmer(aImp ~ sugu + Wmaxkg + 
                                         Zink +  mImp + age_f + (1 | kood), REML = TRUE))
results_l_1_a_c1_Zink <- testEstimates(l_1_a_c1_Zink)

# full model
l_1_a_c1_int <- with(implist_c1, lmer(aImp ~ sugu + Wmaxkg + Calcium + Iodine + Iron + Magnesium + 
                                        Manganese + Phosphorus + Potassium + Selenium + Sodium + 
                                        Zink + BMI + Cerealprod + Eggs + Fatsg + 
                                        Fish + Folate + FruitsBerries + HDL + HOMA + LDL + Lipid + 
                                        Meat + Milk + Niacin + Protein + SugSweets + Veget + VitA + 
                                        VitB1 + VitB12 + VitB2 + VitB6 + VitC + VitD + VitE + Alco + 
                                        mImp + age_cent + (1 | kood), REML = TRUE))
results_l_1_a_c1_int <- testEstimates(l_1_a_c1_int)

# full model with age_interation
l_1_a_c1_slope_interac <- with(implist_c1, lmer(aImp ~ (sugu + Wmaxkg + Calcium + Iodine + Iron + Magnesium + 
                                        Manganese + Phosphorus + Potassium + Selenium + Sodium + 
                                        Zink + BMI + Cerealprod + Eggs + Fatsg + 
                                        Fish + Folate + FruitsBerries + HDL + HOMA + LDL + Lipid + 
                                        Meat + Milk + Niacin + Protein + SugSweets + Veget + VitA + 
                                        VitB1 + VitB12 + VitB2 + VitB6 + VitC + VitD + VitE + Alco + 
                                        mImp) * age_cent  + (age_cent | kood), REML = TRUE))
results_l_1_a_c1_slope_interac <- testEstimates(l_1_a_c1_slope_interac)

# VitB6 included
l_1_a_c1_VitB6 <- with(implist_c1, lmer(aImp ~ sugu + Sodium + BMI +
                                          VitB6 +  mImp + age_f + (1 | kood), REML = TRUE))
results_l_1_a_c1_VitB6 <- testEstimates(l_1_a_c1_VitB6)
################################################################################
# cohort 2; Adaptive impulsivity
# full model
l_1_a_c2_int <- with(implist_c2, lmer(aImp ~ sugu + Wmaxkg + Calcium + Iodine + Iron + Magnesium + 
                                        Manganese + Phosphorus + Potassium + Selenium + Sodium + 
                                        Zink + BMI + Cerealprod + Eggs + Fatsg + 
                                        Fish + Folate + FruitsBerries + HDL + HOMA + LDL + Lipid + 
                                        Meat + Milk + Niacin + Protein + SugSweets + Veget + VitA + 
                                        VitB1 + VitB12 + VitB2 + VitB6 + VitC + VitD + VitE + Alco + 
                                        mImp + age_cent + (1 | kood), REML = TRUE))
results_l_1_a_c2_int <- testEstimates(l_1_a_c2_int)
################################################################################
# cohort 1; Maladaptive impulsivity
# full model
l_1_m_c1_int <- with(implist_c1, lmer(mImp ~ sugu + Wmaxkg + Calcium + Iodine + Iron + Magnesium + 
                                        Manganese + Phosphorus + Potassium + Selenium + Sodium + 
                                        Zink + BMI + Cerealprod + Eggs + Fatsg + 
                                        Fish + Folate + FruitsBerries + HDL + HOMA + LDL + Lipid + 
                                        Meat + Milk + Niacin + Protein + SugSweets + Veget + VitA + 
                                        VitB1 + VitB12 + VitB2 + VitB6 + VitC + VitD + VitE + Alco + 
                                        aImp + age_cent + (1 | kood), REML = TRUE))
results_l_1_m_c1_int <- testEstimates(l_1_m_c1_int)
################################################################################
# cohort 2; Maladaptive impulsivity
# full model
l_1_m_c2_int <- with(implist_c2, lmer(mImp ~ sugu + Wmaxkg + Calcium + Iodine + Iron + Magnesium + 
                                        Manganese + Phosphorus + Potassium + Selenium + Sodium + 
                                        Zink + BMI + Cerealprod + Eggs + Fatsg + 
                                        Fish + Folate + FruitsBerries + HDL + HOMA + LDL + Lipid + 
                                        Meat + Milk + Niacin + Protein + SugSweets + Veget + VitA + 
                                        VitB1 + VitB12 + VitB2 + VitB6 + VitC + VitD + VitE + Alco + 
                                        aImp + age_cent + (1 | kood), REML = TRUE))
results_l_1_m_c2_int <- testEstimates(l_1_m_c2_int)
################################################################################
# using merTools
# aImp cohort 1
# full model 
lmer_1_a_c1_int <- lmerModList(fmla_aImp, data = implist_c1, REML = TRUE)
summary(lmer_1_a_c1_int)

lmer_1_a_c1_slope <- lmerModList(fmla_aImp_slope, data = implist_c1, REML = TRUE)
summary(lmer_1_a_c1_slope)

lmer_1_a_c1_slope_interac <- lmerModList(fmla_aImp_slope_interac, 
                                         data = implist_c1, REML = TRUE)
summary(lmer_1_a_c1_slope_interac)
fixef_1_a_c1_slope <- modelFixedEff(lmer_1_a_c1_slope)
fixef_1_a_c1_slope <- fixef_1_a_c1_slope[order(abs(fixef_1_a_c1_slope$statistic),
                                               decreasing = TRUE), ]
# VitA, Lipid, VitB2, Iron, Niacin, Eggs, VitB1, Fatsg, Zink, Fish, Meat, 
# Iodine, Veget, VitB12, Manganese, Sodium, Alco, Magnesium, Phosphorus, HDL,
# Potassium, VitC, Milk, Calcium, VitE, Selenium, LDL, FruitsBerries, Folate,
# Protein, Cerealprod, SugSweets, HOMA, BMI, Wmaxkg
# are excluded
fmla_aImp_slope_3 <- formula(paste("aImp ~ sugu +  
    VitB6 + mImp + age_cent + VitD +
    (age_cent | kood)"))
lmer_2_a_c1_slope <- lmerModList(fmla_aImp_slope_3, data = implist_c1, REML = TRUE)
summary(lmer_2_a_c1_slope)
fixef_2_a_c1_slope <- modelFixedEff(lmer_2_a_c1_slope)
fixef_2_a_c1_slope <- fixef_2_a_c1_slope[order(abs(fixef_2_a_c1_slope$statistic),
                                               decreasing = TRUE), ]
print(fixef_2_a_c1_slope)
# new AIC = 9040.8

# refit with the final formula to get the p values
l_2_a_c1_slope <- with(implist_c1, lmer(aImp ~ sugu + VitB6 + mImp + age_cent + VitD +
                                          (age_cent | kood), REML = TRUE))
results_l_2_a_c1_slope <- testEstimates(l_2_a_c1_slope)
df_l_2_a_c1_slope <- as.data.frame(results_l_2_a_c1_slope$estimates)
path_2_a_c1 <- here("summary_data", "lmer_results", "aImp_c1.rds")
saveRDS(df_l_2_a_c1_slope, path_2_a_c1)

################################################################################
# aImp cohort 2
# full model 
lmer_1_a_c2_int <- lmerModList(fmla_aImp, data = implist_c2, REML = TRUE)
summary(lmer_1_a_c2_int)

lmer_1_a_c2_slope <- lmerModList(fmla_aImp_slope, data = implist_c2, REML = TRUE)
summary(lmer_1_a_c2_slope)

lmer_1_a_c2_slope_interac <- lmerModList(fmla_aImp_slope_interac, data = implist_c2, REML = TRUE)
summary(lmer_1_a_c2_slope_interac)

fixef_1_a_c2_slope <- modelFixedEff(lmer_1_a_c2_slope)
fixef_1_a_c2_slope <- fixef_1_a_c2_slope[order(abs(fixef_1_a_c2_slope$statistic),
                                               decreasing = TRUE), ]

# Iron, SugSweets, LDL, VitC, HOMA, VitA, VitB12, Calcium, Folate, Lipid, HDL,
# VitB1, Niacin, Fish, Veget, Iodine, Selenium, Sodium, FruitsBerries, VitD, 
# VitE, Eggs, Potassium, Cerealprod, Fatsg, Wmaxkg, BMI, Phosphorus, VitB2, 
# Milk, Protein, Meat, Manganese, VitB6, Magnesium
# are excluded
fmla_aImp_slope_2 <- formula(paste("aImp ~ sugu + 
    Zink + Magnesium + mImp + age_cent + Alco + Alco:sugu + 
    (age_cent | kood)"))
lmer_2_a_c2_slope <- lmerModList(fmla_aImp_slope_2, data = implist_c2, REML = TRUE)
summary(lmer_2_a_c2_slope)
fixef_2_a_c2_slope <- modelFixedEff(lmer_2_a_c2_slope)
fixef_2_a_c2_slope <- fixef_2_a_c2_slope[order(abs(fixef_2_a_c2_slope$statistic),
                                               decreasing = TRUE), ]
print(fixef_2_a_c2_slope)
# new AIC = 10120.6

# refit with the final formula to get the p values
l_2_a_c2_slope <- with(implist_c2, lmer(aImp ~ sugu + Zink + Magnesium + mImp +
                                          age_cent + Alco + Alco:sugu + 
                                          (age_cent | kood), REML = TRUE))
results_l_2_a_c2_slope <- testEstimates(l_2_a_c2_slope)
df_l_2_a_c2_slope <- as.data.frame(results_l_2_a_c2_slope$estimates)
# path_2_a_c2 <- here("summary_data", "lmer_results", "aImp_c2.rds")
# saveRDS(df_l_2_a_c2_slope, path_2_a_c2)

################################################################################

# mImp cohort 1
# full model 
lmer_1_m_c1_int <- lmerModList(fmla_mImp, data = implist_c1, REML = TRUE)
summary(lmer_1_m_c1_int)

lmer_1_m_c1_slope <- lmerModList(fmla_mImp_slope, data = implist_c1, REML = TRUE)
summary(lmer_1_m_c1_slope)

fixef_1_m_c1_slope <- modelFixedEff(lmer_1_m_c1_slope)
fixef_1_m_c1_slope <- fixef_1_m_c1_slope[order(abs(fixef_1_m_c1_slope$statistic),
                                               decreasing = TRUE), ]

# HOMA, Protein, Milk, Fish, VitB6, Lipid, Fatsg, Iron, VitB12, Sodium,
# Magnesium, VitE, FruitsBerries, BMI, SugSweets, Zink, VitD, Potassium,
# Manganese, Niacin, Cerealprod, Eggs, Phosphorus, LDL, VitB1, Calcium, 
# VitB2, VitA, Folate, VitC, Alco
# are excluded
fmla_mImp_slope_3 <- formula(paste("mImp ~ sugu + Iodine + 
    Selenium +  Meat + Veget + Wmaxkg + 
    aImp + age_cent + (age_cent | kood)"))
lmer_2_m_c1_slope <- lmerModList(fmla_mImp_slope_3, data = implist_c1, REML = TRUE)
summary(lmer_2_m_c1_slope)
fixef_2_m_c1_slope <- modelFixedEff(lmer_2_m_c1_slope)
fixef_2_m_c1_slope <- fixef_2_m_c1_slope[order(abs(fixef_2_m_c1_slope$statistic),
                                               decreasing = TRUE), ]
print(fixef_2_m_c1_slope)
# new AIC = 9086.6

# refit with the final formula to get the p values
l_2_m_c1_slope <- with(implist_c1, lmer(mImp ~ sugu + Iodine + 
                                        Selenium +  Meat + Veget + Wmaxkg + 
                                        aImp + age_cent + (age_cent | kood), REML = TRUE))
results_l_2_m_c1_slope <- testEstimates(l_2_m_c1_slope)
df_l_2_m_c1_slope <- as.data.frame(results_l_2_m_c1_slope$estimates)
path_2_m_c1 <- here("summary_data", "lmer_results", "mImp_c1.rds")
saveRDS(df_l_2_m_c1_slope, path_2_m_c1)
################################################################################
# mImp cohort 2
# full model 
lmer_1_m_c2_int <- lmerModList(fmla_mImp, data = implist_c2, REML = TRUE)
summary(lmer_1_m_c2_int)

lmer_1_m_c2_slope <- lmerModList(fmla_mImp_slope, data = implist_c2, REML = TRUE)
summary(lmer_1_m_c2_slope)

fixef_1_m_c2_slope <- modelFixedEff(lmer_1_m_c2_slope)
fixef_1_m_c2_slope <- fixef_1_m_c2_slope[order(abs(fixef_1_m_c2_slope$statistic),
                                               decreasing = TRUE), ]

# lipid, VitB6, Iodine, VitB1, VitA, HDL, VitB2, Fatsg, Calcium, SugSweets,
# Potassium, Veget, LDL, Magnesium, Phosphorus, Protein, Niacin, Manganese,
# Eggs, VitD, VitB12, Iron, VitE, Meat, Milk, Cerealprod, HOMA, FruitsBerries,
# Alco, BMI, Sodium, Fish
# are excluded
fmla_mImp_slope_2 <- formula(paste("mImp ~ sugu + Wmaxkg + 
  Selenium + Folate + Zink + aImp + age_cent + sugu:Zink + 
  (age_cent | kood)"))
lmer_2_m_c2_slope <- lmerModList(fmla_mImp_slope_2, data = implist_c2, REML = TRUE)
summary(lmer_2_m_c2_slope)
fixef_2_m_c2_slope <- modelFixedEff(lmer_2_m_c2_slope)
fixef_2_m_c2_slope <- fixef_2_m_c2_slope[order(abs(fixef_2_m_c2_slope$statistic),
                                               decreasing = TRUE), ]
print(fixef_2_m_c2_slope)
# new AIC = 9962.5

# refit with the final formula to get the p values
l_2_m_c2_slope <- with(implist_c2, lmer(mImp ~ Wmaxkg + sugu +
                                          Selenium + Folate + Zink + aImp + age_cent + 
                                          (age_cent | kood), REML = TRUE))
results_l_2_m_c2_slope <- testEstimates(l_2_m_c2_slope)
df_l_2_m_c2_slope <- as.data.frame(results_l_2_m_c2_slope$estimates)
# path_2_m_c2 <- here("summary_data", "lmer_results", "mImp_c2.rds")
# saveRDS(df_l_2_m_c2_slope, path_2_m_c2)

################################################################################
# analysis of both cohorts joined together
# mImp

# Lipid, HDL, VitB6, VitB2, SugSweets, Phosphorus, VitE, VitB1, LDL, Iron, 
# VitB12, VitC, Cerealprod, Iodine, Fatsg, VitA, Meat, Milk, Magnesium,
# Calcium, Potassium, HOMA, Eggs, VitD, Protein, Niacin, FruitsBerries,
# Manganese, Folate, kohort
# are excluded
fmla_mImp_slope_both <- formula(paste("mImp ~ sugu + Wmaxkg +  
    Selenium + Sodium + Zink + Fish + Veget + 
    Alco + aImp + Sodium:sugu + age_cent + (age_cent | kood)"))
lmer_2_m_both_slope <- lmerModList(fmla_mImp_slope_both, data = implist_c_both, 
                                 REML = TRUE)
summary(lmer_2_m_both_slope)
fixef_2_m_both_slope <- modelFixedEff(lmer_2_m_both_slope)
fixef_2_m_both_slope <- fixef_2_m_both_slope[order(abs(fixef_2_m_both_slope$statistic),
                                               decreasing = TRUE), ]
print(fixef_2_m_both_slope)
# new AIC = 19043.1

# refit with the final formula to get the p values
l_m_c_both_slope <- with(implist_c_both, lmer(mImp ~ sugu + Wmaxkg +Selenium + 
                                                Sodium + Zink + Fish + Veget + 
                                                Alco + aImp + Sodium:sugu + 
                                                age_cent + (age_cent | kood)))
results_l_m_c_both_slope <- testEstimates(l_m_c_both_slope)
df_l_m_c_both_slope <- as.data.frame(results_l_m_c_both_slope$estimates)
path_m_c_both <- here("summary_data", "lmer_results", "mImp_c_both.rds")
saveRDS(df_l_m_c_both_slope, path_m_c_both) 
################################################################################
# aImp

# VitB1, VitA, VitB12, VitD, Lipid, Protein, Iron, Fatsg, Milk, Niacin, HDL, 
# Manganese, Folate, VitC, Sodium, VitB2, Eggs, Veget, Iodine, Meat, HOMA,
# Potassium, LDL, Fish, kohort, VitE, Wmaxkg, BMI, SugSweets, Alco, Selenium,
# FruitsBerries, Phosphorus, Magnesium, Calcium
# are excluded
fmla_aImp_slope_both <- formula(paste("aImp ~ sugu + 
    Zink + Cerealprod + VitB6 + mImp + age_cent + kohort +
    (age_cent | kood)"))
lmer_2_a_both_slope <- lmerModList(fmla_aImp_slope_both, data = implist_c_both, 
                                   REML = TRUE)
summary(lmer_2_a_both_slope)
fixef_2_a_both_slope <- modelFixedEff(lmer_2_a_both_slope)
fixef_2_a_both_slope <- fixef_2_a_both_slope[order(abs(fixef_2_a_both_slope$statistic),
                                                   decreasing = TRUE), ]
print(fixef_2_a_both_slope)
# new AIC = 19156

# refit with the final formula to get the p values
l_a_c_both_slope <- with(implist_c_both, lmer(aImp ~ sugu + Zink + Cerealprod + 
                                                VitB6 + mImp + age_cent + kohort +
                                                (age_cent | kood), REML = TRUE))
results_l_a_c_both_slope <- testEstimates(l_a_c_both_slope)
df_l_a_c_both_slope <- as.data.frame(results_l_a_c_both_slope$estimates)
path_a_c_both <- here("summary_data", "lmer_results", "mImp_c_both.rds")
saveRDS(df_l_a_c_both_slope, path_a_c_both) 