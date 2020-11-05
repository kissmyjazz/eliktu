library(emmeans)
library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(mice)
library(merTools)
library(mitml)
library(lmerTest)
library(papaja)
library(ggsci)
options(scipen=999)
theme_set(theme_apa(base_size = 14)) 

path_mImp <- here("mira_objects", "mitml_m_c_both_slope.rds")
path_aImp <- here("mira_objects", "mitml_a_c_both_slope.rds")

mitml_mImp <- read_rds(path_mImp)
mitml_aImp <- read_rds(path_aImp)

mira_mImp <- as.mira(mitml_mImp)
mira_aImp <- as.mira(mitml_aImp)

age_names <- c(`0` = "18 years", `15` = "33 years")
################################################################################
# mImp
################################################################################
# Wmaxkg by sex conditioned on age
g_m_sex_Wmaxkg_age <- emmip(mira_mImp, sugu ~ Wmaxkg|age_cent, 
                            at = list(age_cent = c(0, 15),
                                      Wmaxkg = c(-2, 2))) 
g_m_sex_Wmaxkg_age <- g_m_sex_Wmaxkg_age + 
  labs(y = "standardised maladaptive impulsivity score", x = "standardised Wmaxkg", 
       title = "Estimated marginal means of standardised\nmaladaptive impulsivity score") + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_m_sex_Wmaxkg_age
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_mImp_sex_Wmaxkg_age.rds")
saveRDS(g_m_sex_Wmaxkg_age, path)

path <- here("figures", "lmer", "g_mImp_sex_Wmaxkg_age.pdf")
ggsave(path, g_m_sex_Wmaxkg_age, width = 6, height = 4.5, dpi = 300)
################################################################################
# Zink by sex conditioned on age
g_m_sex_Zink_age <- emmip(mira_mImp, sugu ~ Zink|age_cent, 
                            at = list(age_cent = c(0, 15),
                                      Zink = c(-2, 2))) 
g_m_sex_Zink_age <- g_m_sex_Zink_age + 
  labs(y = "standardised maladaptive impulsivity score", x = "standardised Zink", 
       title = "Estimated marginal means of standardised\nmaladaptive impulsivity score") + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_m_sex_Zink_age
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_mImp_sex_Zink_age.rds")
saveRDS(g_m_sex_Zink_age, path)

path <- here("figures", "lmer", "g_mImp_sex_Zink_age.pdf")
ggsave(path, g_m_sex_Zink_age, width = 6, height = 4.5, dpi = 300)
################################################################################
# Sodium by sex conditioned on age
g_m_sex_Sodium_age <- emmip(mira_mImp, sugu ~ Sodium|age_cent, 
                            at = list(age_cent = c(0, 15), 
                                      Sodium = c(-2, 2))) 
g_m_sex_Sodium_age <- g_m_sex_Sodium_age + 
  scale_size_manual(values = rep(1.5, 2)) +
  labs(y = "standardised maladaptive impulsivity score", x = "standardised Sodium", 
       title = "Estimated marginal means of standardised\nmaladaptive impulsivity score") + scale_color_lancet() + 
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_m_sex_Sodium_age
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_mImp_sex_Sodium_age.rds")
saveRDS(g_m_sex_Sodium_age, path)

path <- here("figures", "lmer", "g_mImp_sex_Sodium_age.pdf")
ggsave(path, g_m_sex_Sodium_age, width = 6, height = 4.5, dpi = 300)
################################################################################
# aImp
################################################################################
# Zink by sex conditioned on age
g_a_sex_Zink_age <- emmip(mira_aImp, sugu ~ Zink|age_cent, 
                          at = list(age_cent = c(0, 15),
                                    Zink = c(-2, 2))) 
g_a_sex_Zink_age <- g_a_sex_Zink_age + 
  labs(y = "standardised adaptive impulsivity score", x = "standardised Zink", 
       title = "Estimated marginal means of standardised\nadaptive impulsivity score") + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_a_sex_Zink_age
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_aImp_sex_Zink_age.rds")
saveRDS(g_a_sex_Zink_age, path)

path <- here("figures", "lmer", "g_aImp_sex_Zink_age.pdf")
ggsave(path, g_a_sex_Zink_age, width = 6, height = 4.5, dpi = 300)
################################################################################
# Vitamin B6 by sex conditioned on age
g_a_sex_VitB6_age <- emmip(mira_aImp, sugu ~ VitB6|age_cent, 
                          at = list(age_cent = c(0, 15),
                                    VitB6 = c(-2, 2))) 
g_a_sex_VitB6_age <- g_a_sex_VitB6_age + 
  labs(y = "standardised adaptive impulsivity score", x = "standardised Vitamin B6", 
       title = "Estimated marginal means of standardised\nadaptive impulsivity score") + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_a_sex_VitB6_age
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_aImp_sex_VitB6_age.rds")
saveRDS(g_a_sex_VitB6_age, path)

path <- here("figures", "lmer", "g_aImp_sex_VitB6_age.pdf")
ggsave(path, g_a_sex_VitB6_age, width = 6, height = 4.5, dpi = 300)
################################################################################
# Cereal products by sex conditioned on age
g_a_sex_Cerealprod_age <- emmip(mira_aImp, sugu ~ Cerealprod|age_cent, 
                           at = list(age_cent = c(0, 15),
                                     Cerealprod = c(-2, 2))) 
g_a_sex_Cerealprod_age <- g_a_sex_Cerealprod_age + 
  labs(y = "standardised adaptive impulsivity score", 
       x = "standardised Cereal products", 
       title = "Estimated marginal means of standardised\nadaptive impulsivity score") + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_a_sex_Cerealprod_age
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_aImp_sex_Cerealprod_age.rds")
saveRDS(g_a_sex_Cerealprod_age, path)

path <- here("figures", "lmer", "g_aImp_sex_Cerealprod_age.pdf")
ggsave(path, g_a_sex_Cerealprod_age, width = 6, height = 4.5, dpi = 300)