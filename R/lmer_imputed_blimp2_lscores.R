
# lmer analysis of latent rather than composite scores as in 
# `lmer_imputed_blimp2.R

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

# load data ---------------------------------------------------------------
# load latent scores
path <- here("mod_data", "lavaan_fits.rds")
saveRDS(cfa_results, path)
params_df <- purrr::transpose(cfa_results$params) %>% 
  data.table::as.data.table()
scores_df <- dplyr::bind_rows(cfa_results$scores, .id = ".imp") %>% 
  dplyr::rename(aImp_lscore = F2, mImp_lscore = F2)


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
implist_c_both <- as.mitml.list(split(df_blimp0 , df_blimp0$.imp))