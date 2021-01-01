
# lmer analysis of latent rather than composite scores as in 
# `lmer_imputed_blimp2.R`

library(here)
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
library(mice)
library(merTools)
library(mitml)
options(scipen = 999)

# load data ---------------------------------------------------------------
# load latent scores
path <- here("mod_data", "lavaan_fits.rds")
cfa_results <- read_rds(path)
params_df <- purrr::transpose(cfa_results$params) %>% 
  data.table::as.data.table()
scores_df <- dplyr::bind_rows(cfa_results$scores, .id = ".imp") %>% 
  dplyr::rename(aImp_lscore = F2, mImp_lscore = F1) %>% 
  dplyr::mutate(.imp = as.numeric(.imp), kood = factor(kood))

# load imputed data
path <- here("imputed_data", "blimp", "scaled_joined_df2.rds")
df_blimp <- read_rds(path)
df_blimp0 <- df_blimp %>% 
  dplyr::filter(!(kood == 1824 & age == 18)) %>% 
  dplyr::filter(!(kood == 1157 & age == 33)) %>% 
  dplyr::filter(!(kood == 1307 & age == 18)) %>% 
  dplyr::filter(.imp != 0) %>% dplyr::select(-.id) 
# apply deviation coding
df_blimp0$sex_dev <- C(df_blimp0$sugu, sum)
contrasts(df_blimp0$sex_dev) <- contrasts(df_blimp0$sex_dev) / 2

# join datasets
df_full <- df_blimp0 %>% left_join(scores_df, by = c(".imp", "kood", "age"))

implist_lscores <- as.mitml.list(split(df_full , df_full$.imp))

# lmer with deviation coding for sex --------------------------------------
# analysis redone with deviation coding of the sex ------------------------
# mImp
# refit with the final formula to get the p values
l_m_c_both_slope_dev_lscores <- with(implist_lscores, 
                                     lmer(mImp_lscore ~ sex_dev + Wmaxkg + Selenium + 
                                          Sodium + Zink + Fish + Veget + 
                                          Alco + aImp_lscore + Sodium:sex_dev + 
                                          age_cent + (1 | kood),
                                          REML = TRUE, 
                                          control=lmerControl(optimizer="bobyqa")))
# save for further visualisation with `emmeans`
path_m_c_lscores <- here("mira_objects", "mitml_m_c_both_slope_dev_lscores.rds")
saveRDS(l_m_c_both_slope_dev_lscores, path_m_c_lscores)

################################################################################
# aImp
# refit with the final formula to get the p values
l_a_c_both_slope_dev_lscores <- with(implist_lscores, 
                                     lmer(aImp_lscore ~ Zink + Cerealprod + VitB6 + 
                                          mImp_lscore + age_cent + sex_dev + 
                                          age_cent:sex_dev +
                                          (1 | kood), REML = TRUE,
                                          control=lmerControl(optimizer="bobyqa")))

# save for further visualisation with `emmeans`
path_a_c_lscores <- here("mira_objects", "mitml_a_c_both_slope_dev_lscores.rds")
saveRDS(l_a_c_both_slope_dev_lscores, path_a_c_lscores)

'''
Wmaxkg + Calcium + Iodine + Iron + Magnesium + Manganese + Phosphorus + 
  Potassium + Selenium + Sodium + Zink + kcal + BMI + Carb + Cerealprod + CHL + 
  Eggs + Fatsg + Fish + Folate + FruitsBerries + HDL + HOMA + LDL + Lipid +
  Meat + Milk + Niacin + Protein + SugSweets + Veget + VitA + VitB1 + VitB12 +
  VitB2 + VitB6 + VitC + VitD + VitE + Alco + age_cent + 
  sex_dev + mImp_lscore + aImp_lscore
'''

