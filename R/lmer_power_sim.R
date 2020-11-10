# power analysis of lmer results

library(here)
library(tidyverse)
library(lme4)
library(mice)
library(mitml)
library(ggsci)
library(papaja)
library(easystats)
library(simr)
options(scipen=999)
theme_set(theme_apa(base_size = 15) + 
            theme(legend.position = "bottom"))
################################################################################
path <- here("imputed_data", "blimp", "scaled_joined_df2.rds")
df_blimp_2 <- read_rds(path) %>% dplyr::filter(.imp == 2)

lmer_mImp <- lmer(mImp ~ Wmaxkg + Selenium + aImp + Sodium * sugu + Zink + Fish +
                    Veget + Alco + age_cent + (age_cent | kood), data = df_blimp_2, 
              REML = TRUE, control=lmerControl(optimizer="bobyqa"))

lmer_aImp <- lmer(aImp ~ Zink + Cerealprod + VitB6 + mImp + age_cent + sugu + 
                    age_cent:sugu + (age_cent | kood), data = df_blimp_2, 
                  REML = TRUE, control=lmerControl(optimizer="bobyqa"))

################################################################################
# power analysis
# Alco has the lowest coefficent for mImp
# Zink has the lowest coefficent for aImp
# mImp
power_mImp_AlCo <- powerSim(lmer_mImp, test = fixed("Alco", "pb"), nsim = 1000)
power_mImp_Sodium_sugu <- powerSim(lmer_mImp, 
                                   test = fcompare(~ Wmaxkg + Selenium + aImp + 
                                                     Sodium + sugu + Zink + Fish +
                                                     Veget + Alco +
                                                     age_cent, "pb"), nsim = 1000)
# aImp
power_aImp_Zink <- powerSim(lmer_aImp, test = fixed("Zink", "pb"), nsim = 1000)
power_aImp_age_sugu <- powerSim(lmer_aImp, 
                                   test = fcompare(~ Zink + Cerealprod + VitB6 +
                                                     mImp + age_cent + 
                                                     sugu , "pb"), nsim = 1000)

################################################################################
