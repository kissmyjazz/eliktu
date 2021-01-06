
# analysis of blimp imputation data where cases with missing data for an entire 
# wave were not imputed
# univariate analysis
# analysis of the effect of mother's education
# deviation coding is used for sex factor

library(here)
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
library(mice)
library(merTools)
library(mitml)

options(scipen=999)
old <- theme_set(theme_bw())

# load data ---------------------------------------------------------------
# mother's education
path <- here("mod_data", "ema_haridus.csv")
df_educ <- read_csv(path) %>% dplyr::select(kood, m_educ = K24_uus) %>% 
  mutate(m_educ = factor(m_educ, levels = 1:3, labels = c("basic", 
  "secondary", "tertiary")), kood = factor(kood)) 

# imputed data
path <- here("imputed_data", "blimp", "scaled_joined_df2.rds")
df_blimp <- read_rds(path)
# some housekeeping tasks due to the misplaced original data
# kood 1162, age 33, the results were misplaces to kood 1157, age 33 
vec_blimp_1157 <- df_blimp %>% dplyr::filter(kood == 1157, age == 33, 
                                             .imp == 0) %>%
  dplyr::select(Calcium:kcal, Carb:Cerealprod, Eggs:FruitsBerries,
                Lipid:Alco) %>% unlist(., use.names = TRUE)
df_blimp_1157 <- df_blimp %>% dplyr::filter(kood == 1157, age == 33, 
                                            .imp == 0) %>%
  dplyr::select(Calcium:kcal, Carb:Cerealprod, Eggs:FruitsBerries,
                Lipid:Alco) 
df_blimp[which((df_blimp$kood == 1162) & (df_blimp$age == 33)), 
         names(df_blimp_1157)] <- matrix(rep(vec_blimp_1157, 101), nrow = 101, 
                                         byrow = TRUE)

# kood 1825, age 18, the results were misplaces to kood 1824, age 18
vec_blimp_1824 <- df_blimp %>% 
  dplyr::filter(kood == 1824, age == 18, .imp == 0) %>%
  dplyr::select(CHL, HDL:LDL) %>% unlist(., use.names = TRUE) 
df_blimp_1824 <- df_blimp %>% 
  dplyr::filter(kood == 1824, age == 18, .imp == 0) %>%
  dplyr::select(CHL, HDL:LDL)
df_blimp[which((df_blimp$kood == 1825) & (df_blimp$age == 18)), 
         names(df_blimp_1824)] <- matrix(rep(vec_blimp_1824, 101), nrow = 101, 
                                         byrow = TRUE)

# join mother's education data to main file -------------------------------
df_blimp0 <- df_blimp %>% 
  dplyr::filter(!(kood == 1824 & age == 18)) %>% 
  dplyr::filter(!(kood == 1157 & age == 33)) %>% 
  dplyr::filter(!(kood == 1307 & age == 18)) %>% 
  dplyr::filter(.imp != 0) %>% dplyr::select(-.id) %>% 
  left_join(df_educ, by = "kood")
# apply deviation coding
df_blimp0$sex_dev <- C(df_blimp0$sugu, sum)
contrasts(df_blimp0$sex_dev) <- contrasts(df_blimp0$sex_dev) / 2

implist_c_both <- as.mitml.list(split(df_blimp0 , df_blimp0$.imp))

df_origin <- df_blimp %>% dplyr::filter(.imp == 0)

# fitting models with mother's education ----------------------------------
# Mother's education was not an important factor for either mImp or aImp
# # mImp
# fmla_mImp_educ <- formula(paste("mImp ~ Wmaxkg +
#     Selenium + aImp + Sodium * sex_dev +  Zink + Fish + Veget + Alco +
#     age_cent + m_educ + (age_cent | kood)"))
# lmer_mImp_educ <- lmerModList(fmla_mImp_educ, data = implist_c_both,
#                                    REML = TRUE, control=lmerControl(optimizer="bobyqa"))
# summary(lmer_mImp_educ)
# fixef_lmer_mImp_educ <- modelFixedEff(lmer_mImp_educ)
# fixef_lmer_mImp_educ <- fixef_lmer_mImp_educ[order(abs(fixef_lmer_mImp_educ$statistic),
#                                                    decreasing = TRUE), ]
# print(fixef_lmer_mImp_educ)
# 
# # aImp
# fmla_aImp_educ <- formula(paste("aImp ~ Zink + Cerealprod + VitB6 + mImp +
# age_cent + sex_dev + age_cent:sex_dev + m_educ + (age_cent | kood)"))
# lmer_aImp_educ <- lmerModList(fmla_aImp_educ, data = implist_c_both,
#                               REML = TRUE, control=lmerControl(optimizer="bobyqa"))
# summary(lmer_aImp_educ)
# fixef_lmer_aImp_educ <- modelFixedEff(lmer_aImp_educ)
# fixef_lmer_aImp_educ <- fixef_lmer_aImp_educ[order(abs(fixef_lmer_aImp_educ$statistic),
#                                                    decreasing = TRUE), ]
# print(fixef_lmer_aImp_educ)

# univariate models -------------------------------------------------------
# all models include sex. other impulsivity subscale, and centred age
# mImp
# Sex
lmer_mImp_sex <- with(implist_c_both, lmer(mImp ~ sex_dev + age_cent + 
                                          (age_cent | kood), REML = TRUE,
                                          control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_mImp_sex))
path <- here("mira_objects", "mitml_lmer_univ_mImp_sex.rds")
saveRDS(lmer_mImp_sex, path)

# Wmaxkg
lmer_mImp_Wmaxkg <- with(implist_c_both, lmer(mImp ~ Wmaxkg + sex_dev + age_cent + 
                                             (age_cent | kood), REML = TRUE,
                                           control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_mImp_Wmaxkg))
path <- here("mira_objects", "mitml_lmer_univ_mImp_Wmaxkg.rds")
saveRDS(lmer_mImp_Wmaxkg, path)

# Selenium
lmer_mImp_Selenium <- with(implist_c_both, lmer(mImp ~ Selenium + sex_dev + age_cent + 
                                               (age_cent | kood), REML = TRUE,
                                              control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_mImp_Selenium))
path <- here("mira_objects", "mitml_lmer_univ_mImp_Selenium.rds")
saveRDS(lmer_mImp_Selenium, path)

# Zink
lmer_mImp_Zink <- with(implist_c_both, lmer(mImp ~ Zink + sex_dev + age_cent + 
                                                (age_cent | kood), REML = TRUE,
                                              control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_mImp_Zink))
path <- here("mira_objects", "mitml_lmer_univ_mImp_Zink.rds")
saveRDS(lmer_mImp_Zink, path)

# Fish
lmer_mImp_Fish <- with(implist_c_both, lmer(mImp ~ Fish + sex_dev + age_cent + 
                                                (age_cent | kood), REML = TRUE,
                                              control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_mImp_Fish))
path <- here("mira_objects", "mitml_lmer_univ_mImp_Fish.rds")
saveRDS(lmer_mImp_Fish, path)

# Veget
lmer_mImp_Veget <- with(implist_c_both, lmer(mImp ~ Veget + sex_dev + age_cent + 
                                                (age_cent | kood), REML = TRUE,
                                              control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_mImp_Veget))
path <- here("mira_objects", "mitml_lmer_univ_mImp_Veget.rds")
saveRDS(lmer_mImp_Veget, path)

# Alco
lmer_mImp_Alco <- with(implist_c_both, lmer(mImp ~ Alco + sex_dev + age_cent + 
                                                (age_cent | kood), REML = TRUE,
                                              control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_mImp_Alco))
path <- here("mira_objects", "mitml_lmer_univ_mImp_Alco.rds")
saveRDS(lmer_mImp_Alco, path)

# aImp
lmer_mImp_aImp <- with(implist_c_both, lmer(mImp ~ aImp + sex_dev + age_cent + 
                                                (age_cent | kood), REML = TRUE,
                                              control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_mImp_aImp))
path <- here("mira_objects", "mitml_lmer_univ_mImp_aImp.rds")
saveRDS(lmer_mImp_aImp, path)

# Sodium
lmer_mImp_Sodium <- with(implist_c_both, lmer(mImp ~ Sodium * sex_dev + age_cent + 
                                                (age_cent | kood), REML = TRUE,
                                              control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_mImp_Sodium))
path <- here("mira_objects", "mitml_lmer_univ_mImp_Sodium.rds")
saveRDS(lmer_mImp_Sodium, path)
################################################################################
# aImp
# Sex
lmer_aImp_Sex <- with(implist_c_both, lmer(aImp ~ age_cent * sex_dev + 
                                              (age_cent | kood), REML = TRUE,
                                            control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_aImp_Sex))
path <- here("mira_objects", "mitml_lmer_univ_aImp_Sex.rds")
saveRDS(lmer_aImp_Sex, path)

# Zink
lmer_aImp_Zink <- with(implist_c_both, lmer(aImp ~ age_cent * sex_dev + Zink +
                                              (age_cent | kood), REML = TRUE,
                                              control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_aImp_Zink))
path <- here("mira_objects", "mitml_lmer_univ_aImp_Zink.rds")
saveRDS(lmer_aImp_Zink, path)

# Cerealprod
lmer_aImp_Cerealprod <- with(implist_c_both, lmer(aImp ~ age_cent * sex_dev + Cerealprod +
                                              (age_cent | kood), REML = TRUE,
                                            control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_aImp_Cerealprod))
path <- here("mira_objects", "mitml_lmer_univ_aImp_Cerealprod.rds")
saveRDS(lmer_aImp_Cerealprod, path)

# VitB6
lmer_aImp_VitB6 <- with(implist_c_both, lmer(aImp ~ age_cent * sex_dev + VitB6 +
                                              (age_cent | kood), REML = TRUE,
                                            control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_aImp_VitB6))
path <- here("mira_objects", "mitml_lmer_univ_aImp_VitB6.rds")
saveRDS(lmer_aImp_VitB6, path)

# mImp
lmer_aImp_mImp <- with(implist_c_both, lmer(aImp ~ age_cent * sex_dev + mImp +
                                              (age_cent | kood), REML = TRUE,
                                            control=lmerControl(optimizer="bobyqa")))

summary(pool(lmer_aImp_mImp))
path <- here("mira_objects", "mitml_lmer_univ_aImp_mImp.rds")
saveRDS(lmer_aImp_mImp, path)

# fully adjusted model on original data (with missing) --------------------
# mImp
# lmer_mImp_orig <- lmer(mImp ~ sex_dev + Wmaxkg + Selenium + Sodium + Zink + Fish + 
#                          Veget + Alco + aImp + Sodium:sex_dev + age_cent + 
#                          (1 | kood), REML = TRUE, data = df_origin,
#                           control=lmerControl(optimizer="bobyqa"))
# path <- here("lmer_objects", "lmer_mImp_orig_data.rds")
# saveRDS(lmer_mImp_orig, path)
# 
# # aImp
# lmer_aImp_orig <- lmer(aImp ~ Zink + Cerealprod + VitB6 + mImp +
#                         age_cent + sex_dev + age_cent:sex_dev +
#                         (1 | kood), REML = TRUE, data = df_origin,
#                        control=lmerControl(optimizer="bobyqa"))
# path <- here("lmer_objects", "lmer_aImp_orig_data.rds")
# saveRDS(lmer_aImp_orig, path)



