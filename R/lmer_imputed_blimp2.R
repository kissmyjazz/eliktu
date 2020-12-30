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
library(bbmle)
library(DHARMa)
library(glmmTMB)

options(scipen=999)
old <- theme_set(theme_bw())

# path <- here("mod_data", "df_for_imputation.csv")
# df <- read_csv(path)
# names <- colnames(df) 
# names_food <- c(".imp", names[c(1:3, 5:44)])
# names_amis <- c(".imp", names[1:3], names[5], names[16:17], names[45:68])
# 
# path_food_c1 <- here("imputed_data", "blimp", 'imps_c1f_food.csv')
# path_food_c2 <- here("imputed_data", "blimp", 'imps_c2f_food.csv')
# 
# path_amis_c1 <- here("imputed_data", "blimp", 'imps_c1f_AMIS.csv')
# path_amis_c2 <- here("imputed_data", "blimp", 'imps_c2f_AMIS.csv') 
# 
# df_food_c1 <- read_csv(path_food_c1, col_names = names_food)
# df_food_c2 <- read_csv(path_food_c2, col_names = names_food)
# df_food <- bind_rows(list(df_food_c1, df_food_c2), .id = "kohort")
# df_food[df_food == -999] <- NA
# 
# df_amis_c1 <- read_csv(path_amis_c1, col_names = names_amis) %>% 
#   dplyr::select(-c(sugu, Wmaxkg, kcal, BMI))
# df_amis_c2 <- read_csv(path_amis_c2, col_names = names_amis) %>% 
#   dplyr::select(-c(sugu, Wmaxkg, kcal, BMI))
# df_amis <- bind_rows(list(df_amis_c1, df_amis_c2))
# df_amis[df_amis == -999] <- NA
# 
# df_blimp <- df_food %>% dplyr::left_join(df_amis, by = c(".imp", "kood", "age"))
# df_blimp <- df_blimp %>% group_by(.imp) %>% mutate(.id = row_number()) %>% 
#   ungroup() %>% rowwise() %>% 
#   dplyr::mutate(aImp = sum(c(AMIS3, (6-AMIS7), AMIS11, AMIS15, AMIS19, AMIS23, 
#                              AMIS2, (6-AMIS6), AMIS10, AMIS14, AMIS18, AMIS22)),
#                 mImp = sum(c(AMIS1, AMIS5, AMIS9, AMIS13, AMIS17, (6-AMIS21),
#                              AMIS4, AMIS12, AMIS16, AMIS20, AMIS24))) %>% 
#   dplyr::ungroup() %>% 
#   dplyr::select(-c(AMIS1:AMIS24)) %>% 
#   dplyr::mutate(sugu = factor(sugu, levels = 0:1, labels = c("female", "male")), 
#                 kood = factor(kood), age_cent = (age - 18), 
#                 age_f = factor(age, levels = c(15, 18, 25, 33), 
#                                labels = c("15 years", "18 years", "25 years", 
#                                           "33 years"))) %>% 
#   group_by(.imp) %>% 
#   dplyr::mutate_at(c("aImp", "mImp"), ~(scale(.x, scale = TRUE) %>% as.vector())) %>% 
#   dplyr::ungroup()

# saved the data
# path <- here("imputed_data", "blimp", "scaled_joined_df2.rds")
# saveRDS(df_blimp, path)

################################################################################
# Save as mids objects for other analysis
# mids_1 <- df_blimp %>% dplyr::filter(kohort == 1) %>% 
#   dplyr::select(-kohort) %>% as.mids()
# mids_2 <- df_blimp %>% dplyr::filter(kohort == 2) %>% 
#   dplyr::select(-kohort) %>% as.mids()
# 
# path_mids_1 <- here("imputed_data", "blimp", "mids_ver2_cohort_1.rds")
# path_mids_2 <- here("imputed_data", "blimp", "mids_ver2_cohort_2.rds")

# saveRDS(mids_1, path_mids_1)
# saveRDS(mids_2, path_mids_2)
################################################################################
path <- here("imputed_data", "blimp", "scaled_joined_df2.rds")
df_blimp <- read_rds(path)
df_blimp0 <- df_blimp %>% dplyr::filter(.imp != 0) %>% dplyr::select(-.id) 
# apply deviation coding
df_blimp0$sex_dev <- C(df_blimp0$sugu, sum)
contrasts(df_blimp0$sex_dev) <- contrasts(df_blimp0$sex_dev) / 2

df_blimp0_c1 <- df_blimp0 %>% dplyr::filter(kohort == 1) %>% 
  dplyr::select(-kohort)
df_blimp0_c2 <- df_blimp0 %>% dplyr::filter(kohort == 2) %>% 
  dplyr::select(-kohort)
implist_c1 <- as.mitml.list(split(df_blimp0_c1 , df_blimp0_c1$.imp))
implist_c2 <- as.mitml.list(split(df_blimp0_c2 , df_blimp0_c2$.imp))
names <- names(df_blimp0_c1)
implist_c_both <- as.mitml.list(split(df_blimp0 , df_blimp0$.imp))

# filter out possible outliers for aImp modeling
# Cases 842, 870, 962, 1107, 1713, 1741, 1767, 1798, 2207, 2243
# df_blimp0_no_outl <- df_blimp0 %>% 
#   dplyr::filter(!kood %in% c(842, 870, 962, 1107, 1713, 1741, 1767, 1798, 2207,
#                              2243))
# implist_c_both_no_outl <- as.mitml.list(split(df_blimp0_no_outl, 
#                                               df_blimp0_no_outl$.imp))
################################################################################
# analysis of both cohorts joined together
# mImp

# Lipid, HDL, VitB6, VitB2, SugSweets, Phosphorus, VitE, VitB1, LDL, Iron, 
# VitB12, VitC, Cerealprod, Iodine, Fatsg, VitA, Meat, Milk, Magnesium,
# Calcium, Potassium, HOMA, Eggs, VitD, Protein, Niacin, FruitsBerries,
# Manganese, Folate, kohort
# are excluded
fmla_mImp_slope_both <- formula(paste("mImp ~ Wmaxkg +
    Selenium + aImp + Sodium * sugu +  Zink + Fish + Veget + Alco +
    age_cent + (age_cent | kood)"))
lmer_2_m_both_slope <- lmerModList(fmla_mImp_slope_both, data = implist_c_both, 
                                 REML = TRUE, control=lmerControl(optimizer="bobyqa"))
summary(lmer_2_m_both_slope)
fixef_2_m_both_slope <- modelFixedEff(lmer_2_m_both_slope)
fixef_2_m_both_slope <- fixef_2_m_both_slope[order(abs(fixef_2_m_both_slope$statistic),
                                               decreasing = TRUE), ]
print(fixef_2_m_both_slope)
# new AIC = 7337.4

# refit with the final formula to get the p values
l_m_c_both_slope <- with(implist_c_both, lmer(mImp ~ sugu + Wmaxkg + Selenium + 
                                              Sodium + Zink + Fish + Veget + 
                                              Alco + aImp + Sodium:sugu + 
                                              age_cent + (age_cent | kood),
                                              REML = TRUE, 
                                              control=lmerControl(optimizer="bobyqa")))
# save for further visualisation with `emmeans`
# path_m_c_both <- here("mira_objects", "mitml_m_c_both_slope.rds")
# saveRDS(l_m_c_both_slope, path_m_c_both)

results_l_m_c_both_slope <- testEstimates(l_m_c_both_slope)
# save pooled estimates for easy export into MS Word 
df_l_m_c_both_slope <- as.data.frame(results_l_m_c_both_slope$estimates) %>% 
  round(3) %>% rownames_to_column("term")
path_m <- here("summary_data", "lmer_results", "mImp_estimates_both_c.txt")
write.table(df_l_m_c_both_slope, file = path_m, sep = ",", quote = FALSE, 
            row.names = FALSE)

# path_m_c_both <- here("summary_data", "lmer_results", "mImp_c_both.rds")
# saveRDS(df_l_m_c_both_slope, path_m_c_both) 
################################################################################
# aImp

# VitB1, VitA, VitB12, VitD, Lipid, Protein, Iron, Fatsg, Milk, Niacin, HDL, 
# Manganese, Folate, VitC, Sodium, VitB2, Eggs, Veget, Iodine, Meat, HOMA,
# Potassium, LDL, Fish, kohort, VitE, Wmaxkg, BMI, SugSweets, Alco, Selenium,
# FruitsBerries, Phosphorus, Magnesium, Calcium, kohort
# are excluded
fmla_aImp_slope_both <- formula(paste("aImp ~ Zink + Cerealprod + VitB6 + mImp +
age_cent + sugu + age_cent:sugu + (age_cent | kood)"))
lmer_2_a_both_slope <- lmerModList(fmla_aImp_slope_both, data = implist_c_both, 
                                   REML = TRUE, 
                                   control=lmerControl(optimizer="bobyqa"))
summary(lmer_2_a_both_slope)
fixef_2_a_both_slope <- modelFixedEff(lmer_2_a_both_slope)
fixef_2_a_both_slope <- fixef_2_a_both_slope[order(abs(fixef_2_a_both_slope$statistic),
                                                   decreasing = TRUE), ]
print(fixef_2_a_both_slope)
# new AIC = 7085

# refit with the final formula to get the p values
l_a_c_both_slope <- with(implist_c_both, lmer(aImp ~ Zink + Cerealprod + VitB6 + mImp +
                                                age_cent + sugu + age_cent:sugu +
                                                (age_cent | kood), REML = TRUE,
                                              control=lmerControl(optimizer="bobyqa")))

# save for further visualisation with `emmeans`
# path_a_c_both <- here("mira_objects", "mitml_a_c_both_slope.rds")
# saveRDS(l_a_c_both_slope, path_a_c_both)

results_l_a_c_both_slope <- testEstimates(l_a_c_both_slope)
# save pooled estimates for easy export into MS Word 
df_l_a_c_both_slope <- as.data.frame(results_l_a_c_both_slope$estimates) %>% 
  round(3) %>% rownames_to_column("term")
path_a <- here("summary_data", "lmer_results", "aImp_estimates_both_c.txt")
write.table(df_l_a_c_both_slope, file = path_a, sep = ",", quote = FALSE, 
            row.names = FALSE)
# path_a_c_both <- here("summary_data", "lmer_results", "aImp_c_both.rds")
# saveRDS(df_l_a_c_both_slope, path_a_c_both) 

################################################################################
# additional model selection to account for autocorrelation among the residuals
# mImp
# lme_1_m_both_slope <- lme(mImp ~ Wmaxkg +  
#   Selenium + aImp + Sodium * sugu + Zink + Fish + Veget + Alco +
#   age_cent, random = list(kood = ~ 1), data = implist_c_both[[2]], 
#   method = "REML", correlation = corSymm())
# 
# lme_m_c_both_slope <- with(implist_c_both, lme(mImp ~ Wmaxkg +  
#                          Selenium + aImp + Sodium * sugu + Zink + Fish + 
#                          Veget + Alco + age_cent, random = list(kood = ~ 1),
#                          method = "REML", correlation = corSymm()))
# results_lme_m_c_both_slope <- testEstimates(lme_m_c_both_slope)

# path_m_c_both_lme <- here("mira_objects", "mitml_m_c_both_slope_lme.rds")
# saveRDS(lme_m_c_both_slope, path_m_c_both_lme)
################################################################################
# additional model selection to account for autocorrelation among the residuals
# aImp 
# Accoding to AIC scores, no adjustments concerning the correlation structure 
# of the residuals were necessary 
# lmer_a_c_both_slope_no_outl <- with(implist_c_both_no_outl, 
#                            lmer(aImp ~ Zink + Cerealprod + VitB6 + mImp +
#                                age_cent + sugu + age_cent:sugu + (age_cent|kood),
#                                REML = TRUE, 
#                                control=lmerControl(optimizer="bobyqa")))
# results_a_c_both_slope_no_outl <- testEstimates(lmer_a_c_both_slope_no_outl)
# save for further visualisation with `emmeans`
# path_a_c_both_no_outl <- here("mira_objects", "mitml_a_c_both_slope_no_outl.rds")
# saveRDS(lmer_a_c_both_slope_no_outl, path_a_c_both_no_outl)

# analysis redone with deviation coding of the sex ------------------------
# mImp
# refit with the final formula to get the p values
l_m_c_both_slope_dev <- with(implist_c_both, lmer(mImp ~ sex_dev + Wmaxkg + Selenium + 
                                                Sodium + Zink + Fish + Veget + 
                                                Alco + aImp + Sodium:sex_dev + 
                                                age_cent + (age_cent | kood),
                                              REML = TRUE, 
                                              control=lmerControl(optimizer="bobyqa")))
# save for further visualisation with `emmeans`
path_m_c_both <- here("mira_objects", "mitml_m_c_both_slope_dev.rds")
saveRDS(l_m_c_both_slope_dev, path_m_c_both)

################################################################################
# aImp
# refit with the final formula to get the p values
l_a_c_both_slope_dev <- with(implist_c_both, lmer(aImp ~ Zink + Cerealprod + 
                                                  VitB6 + mImp +
                                                age_cent + sex_dev + 
                                                  age_cent:sex_dev +
                                                (age_cent | kood), REML = TRUE,
                                              control=lmerControl(optimizer="bobyqa")))

# save for further visualisation with `emmeans`
path_a_c_both <- here("mira_objects", "mitml_a_c_both_slope_dev.rds")
saveRDS(l_a_c_both_slope_dev, path_a_c_both)




