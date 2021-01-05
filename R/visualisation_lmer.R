library(emmeans)
library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(mice)
library(merTools)
library(mitml)
library(papaja)
library(ggsci)
library(cowplot)
library(extrafont)
options(scipen=999)
theme_set(theme_apa(base_size = 12) + 
  theme(legend.position = "none",
        legend.background = element_blank(),
        legend.title = element_blank()
))

path_mImp <- here("mira_objects", "mitml_m_c_both_slope.rds")
path_aImp <- here("mira_objects", "mitml_a_c_both_slope.rds")

mitml_mImp <- read_rds(path_mImp)
mitml_aImp <- read_rds(path_aImp)

mira_mImp <- as.mira(mitml_mImp)
mira_aImp <- as.mira(mitml_aImp)

age_names <- c(`-3` = "15 years", `0` = "18 years", `7` = "25 years", 
               `15` = "33 years")
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

################################################################################
################################################################################
# 18 and 25, and 33 years
# mImp
################################################################################
# Wmaxkg by sex conditioned on age
g_m_sex_Wmaxkg_age_25 <- emmip(mira_mImp, sugu ~ Wmaxkg|age_cent, 
                            at = list(age_cent = c(0, 7, 15),
                                      Wmaxkg = c(-2, 2))) 
g_m_sex_Wmaxkg_age_25 <- g_m_sex_Wmaxkg_age_25 + 
  labs(y = "standardised maladaptive impulsivity score", x = "standardised Wmaxkg", 
       title = "Estimated marginal means of standardised\nmaladaptive impulsivity score") + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_m_sex_Wmaxkg_age_25
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_mImp_sex_Wmaxkg_age_25.rds")
saveRDS(g_m_sex_Wmaxkg_age_25, path)

path <- here("figures", "lmer", "g_mImp_sex_Wmaxkg_age_25.pdf")
ggsave(path, g_m_sex_Wmaxkg_age_25, width = 6, height = 4.5, dpi = 300)
################################################################################
# Zink by sex conditioned on age
g_m_sex_Zink_age_25 <- emmip(mira_mImp, sugu ~ Zink|age_cent, 
                          at = list(age_cent = c(0, 7, 15),
                                    Zink = c(-2, 2))) 
g_m_sex_Zink_age_25 <- g_m_sex_Zink_age_25 + 
  labs(y = "standardised maladaptive impulsivity score", x = "standardised Zink", 
       title = "Estimated marginal means of standardised\nmaladaptive impulsivity score") + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_m_sex_Zink_age_25
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_mImp_sex_Zink_age_25.rds")
saveRDS(g_m_sex_Zink_age_25, path)

path <- here("figures", "lmer", "g_mImp_sex_Zink_age_25.pdf")
ggsave(path, g_m_sex_Zink_age_25, width = 6, height = 4.5, dpi = 300)
################################################################################
# Sodium by sex conditioned on age
g_m_sex_Sodium_age_25 <- emmip(mira_mImp, sugu ~ Sodium|age_cent, 
                            at = list(age_cent = c(0, 7, 15), 
                                      Sodium = c(-2, 2))) 
g_m_sex_Sodium_age_25 <- g_m_sex_Sodium_age_25 + 
  scale_size_manual(values = rep(1.5, 2)) +
  labs(y = "standardised maladaptive impulsivity score", x = "standardised Sodium", 
       title = "Estimated marginal means of standardised\nmaladaptive impulsivity score") + scale_color_lancet() + 
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_m_sex_Sodium_age_25
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_mImp_sex_Sodium_age_25.rds")
saveRDS(g_m_sex_Sodium_age_25, path)

path <- here("figures", "lmer", "g_mImp_sex_Sodium_age_25.pdf")
ggsave(path, g_m_sex_Sodium_age_25, width = 6, height = 4.5, dpi = 300)
################################################################################
# Fish by sex conditioned on age
g_m_sex_Fish_age_25 <- emmip(mira_mImp, sugu ~ Fish|age_cent, 
                               at = list(age_cent = c(0, 7, 15), 
                                         Fish = c(-2, 2))) 
g_m_sex_Fish_age_25 <- g_m_sex_Fish_age_25 + 
  scale_size_manual(values = rep(1.5, 2)) +
  labs(y = "standardised maladaptive impulsivity score", x = "standardised Fish", 
       title = "Estimated marginal means of standardised\nmaladaptive impulsivity score") + scale_color_lancet() + 
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_m_sex_Fish_age_25
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_mImp_sex_Fish_age_25.rds")
saveRDS(g_m_sex_Fish_age_25, path)

path <- here("figures", "lmer", "g_mImp_sex_Fish_age_25.pdf")
ggsave(path, g_m_sex_Fish_age_25, width = 6, height = 4.5, dpi = 300)
#################################################################################
# Vegetables by sex conditioned on age
g_m_sex_Veget_age_25 <- emmip(mira_mImp, sugu ~ Veget|age_cent, 
                               at = list(age_cent = c(0, 7, 15), 
                                         Veget = c(-2, 2))) 
g_m_sex_Veget_age_25 <- g_m_sex_Veget_age_25 + 
  scale_size_manual(values = rep(1.5, 2)) +
  labs(y = "standardised maladaptive impulsivity score", x = "standardised Vegetables", 
       title = "Estimated marginal means of standardised\nmaladaptive impulsivity score") + scale_color_lancet() + 
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_m_sex_Veget_age_25
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_mImp_sex_Veget_age_25.rds")
saveRDS(g_m_sex_Veget_age_25, path)

path <- here("figures", "lmer", "g_mImp_sex_Veget_age_25.pdf")
ggsave(path, g_m_sex_Veget_age_25, width = 6, height = 4.5, dpi = 300)
################################################################################# 
# Alcohol by sex conditioned on age
g_m_sex_Alco_age_25 <- emmip(mira_mImp, sugu ~ Alco|age_cent, 
                               at = list(age_cent = c(0, 7, 15), 
                                         Alco = c(-2, 2))) 
g_m_sex_Alco_age_25 <- g_m_sex_Alco_age_25 + 
  scale_size_manual(values = rep(1.5, 2)) +
  labs(y = "standardised maladaptive impulsivity score", x = "standardised Alcohol", 
       title = "Estimated marginal means of standardised\nmaladaptive impulsivity score") + scale_color_lancet() + 
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_m_sex_Alco_age_25
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_mImp_sex_Alco_age_25.rds")
saveRDS(g_m_sex_Alco_age_25, path)

path <- here("figures", "lmer", "g_mImp_sex_Alco_age_25.pdf")
ggsave(path, g_m_sex_Alco_age_25, width = 6, height = 4.5, dpi = 300)
################################################################################
# aImp
################################################################################
# Zink by sex conditioned on age
g_a_sex_Zink_age_25 <- emmip(mira_aImp, sugu ~ Zink|age_cent, 
                          at = list(age_cent = c(0, 7, 15),
                                    Zink = c(-2, 2))) 
g_a_sex_Zink_age_25 <- g_a_sex_Zink_age_25 + 
  labs(y = "standardised adaptive impulsivity score", x = "standardised Zink", 
       title = "Estimated marginal means of standardised\nadaptive impulsivity score") + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_a_sex_Zink_age_25
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_aImp_sex_Zink_age_25.rds")
saveRDS(g_a_sex_Zink_age_25, path)

path <- here("figures", "lmer", "g_aImp_sex_Zink_age_25.pdf")
ggsave(path, g_a_sex_Zink_age_25, width = 6, height = 4.5, dpi = 300)
################################################################################
# Vitamin B6 by sex conditioned on age
g_a_sex_VitB6_age_25 <- emmip(mira_aImp, sugu ~ VitB6|age_cent, 
                           at = list(age_cent = c(0, 7, 15),
                                     VitB6 = c(-2, 2))) 
g_a_sex_VitB6_age_25 <- g_a_sex_VitB6_age_25 + 
  labs(y = "standardised adaptive impulsivity score", x = "standardised Vitamin B6", 
       title = "Estimated marginal means of standardised\nadaptive impulsivity score") + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_a_sex_VitB6_age_25
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_aImp_sex_VitB6_age_25.rds")
saveRDS(g_a_sex_VitB6_age_25, path)

path <- here("figures", "lmer", "g_aImp_sex_VitB6_age_25.pdf")
ggsave(path, g_a_sex_VitB6_age_25, width = 6, height = 4.5, dpi = 300)
################################################################################
# Cereal products by sex conditioned on age
g_a_sex_Cerealprod_age_25 <- emmip(mira_aImp, sugu ~ Cerealprod|age_cent, 
                                at = list(age_cent = c(0, 7, 15),
                                          Cerealprod = c(-2, 2))) 
g_a_sex_Cerealprod_age_25 <- g_a_sex_Cerealprod_age_25 + 
  labs(y = "standardised adaptive impulsivity score", 
       x = "standardised Cereal products", 
       title = "Estimated marginal means of standardised\nadaptive impulsivity score") + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) +
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
g_a_sex_Cerealprod_age_25
# save ggplot object in case I need to further edit it
path <- here("figures", "ggplot_objects", "g_aImp_sex_Cerealprod_age_25.rds")
saveRDS(g_a_sex_Cerealprod_age_25, path)

path <- here("figures", "lmer", "g_aImp_sex_Cerealprod_age_25.pdf")
ggsave(path, g_a_sex_Cerealprod_age_25, width = 6, height = 4.5, dpi = 300)

# cowplot prep --------------------------------------------------------------
g_m_sex_Sodium_age_t <- emmip(mira_mImp, sugu ~ Sodium|age_cent, 
                              CIs = TRUE, CIarg = list(lwd = 1, alpha = 1,
                                                       show.legend = FALSE),
                              dodge = 0.3,
                            at = list(age_cent = c(0, 7, 15),
                                      Sodium = c(-2, 2))) 
g_m_sex_Sodium_age_t <- g_m_sex_Sodium_age_t + 
  labs(y = NULL,
       x = "standardised sodium intake", 
       title = NULL) + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) +
  coord_cartesian(y = c(-0.6, 0.6)) + 
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names)) + 
  theme(legend.position = c(0.84, 0.90))
g_m_sex_Sodium_age_t
path <- here("figures", "ggplot_objects", "g_mImp_sex_Sodium_CI.rds")
saveRDS(g_m_sex_Sodium_age_t, path)

g_a_sex_VitB6_age_t <- emmip(mira_aImp, sugu ~ VitB6|age_cent, 
                              CIs = TRUE, CIarg = list(lwd = 1, alpha = 1,
                                                       show.legend = FALSE),
                              dodge = 0.3,
                              at = list(age_cent = c(0, 7, 15),
                                        VitB6 = c(-2, 2))) 
g_a_sex_VitB6_age_t <- g_a_sex_VitB6_age_t + 
  labs(y = NULL,
       x = "standardised Vitamin B6 intake", 
       title = NULL) + scale_color_lancet() + 
  scale_size_manual(values = rep(1.5, 2)) +
  aes(size = sugu, color = sugu) + 
  coord_cartesian(y = c(-0.6, 0.6)) + 
  facet_wrap(vars(age_cent), labeller = as_labeller(age_names))
g_a_sex_VitB6_age_t
path <- here("figures", "ggplot_objects", "g_aImp_sex_VitB6_CI.rds")
saveRDS(g_a_sex_VitB6_age_t, path)

# cowplot grids
grid_Sodium_VitB6 <- plot_grid(g_m_sex_Sodium_age_t, g_a_sex_VitB6_age_t,
                      ncol = 2, labels = "AUTO", scale = 1,
                      label_fontfamily = "Times New Roman",
                      label_size = 12, rel_widths = c(1, 1))
grid_Sodium_VitB6
path <- here("figures", "lmer_plot.pdf")
save_plot(path, grid_Sodium_VitB6, base_width = 7.5, base_height = 4, dpi = 600)