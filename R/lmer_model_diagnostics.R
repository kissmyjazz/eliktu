# diagnostics of lmer models and further selection

library(here)
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
library(mice)
library(mitml)
library(ggsci)
library(papaja)
library(easystats)
library(bbmle)
library(DHARMa)
options(scipen=999)
theme_set(theme_apa(base_size = 15) + 
            theme(legend.position = "bottom"))

################################################################################
# visualisation of Vitamine B6 distribution
path <- here("mod_data", "df_for_imputation.csv")
df <- read_csv(path) %>% 
  dplyr::mutate(sugu = factor(sugu, levels = 0:1, labels = c("female", "male")), 
                              kood = factor(kood), age_cent = (age - 18), 
                              age_f = factor(age, levels = c(15, 18, 25, 33), 
                              labels = c("15 years", "18 years", "25 years", 
                                         "33 years")))

df_stats <- df %>% group_by(kohort) %>% summarise(median = median(VitB6, 
                                                                  na.rm = TRUE))

labels = c(`1` = "cohort: 1", `2` = "cohort: 2")

g_hist_VitB6 <- ggplot(df, aes(x = VitB6)) + 
  geom_histogram(aes(y =..density..), binwidth = 0.1) + 
  facet_grid(rows = vars(kohort), labeller = labeller(kohort = labels)) +
  geom_vline(data = df_stats, aes(xintercept = median), color = "orange") +
  coord_cartesian(x = c(0, 7))

g_hist_VitB6 
# ggsave("figures/g_hist_VitB6.pdf", width = 7, height = 5, dpi = 300)
################################################################################
# load data files and convert to `mira`
# main model
path_mImp <- here("mira_objects", "mitml_m_c_both_slope_dev.rds")
path_aImp <- here("mira_objects", "mitml_a_c_both_slope_dev.rds")
mitml_mImp <- read_rds(path_mImp)
mitml_aImp <- read_rds(path_aImp)
mira_mImp <- as.mira(mitml_mImp)
mira_aImp <- as.mira(mitml_aImp)

# latent scores model
path_mImp_lscores <- here("mira_objects", "mitml_m_c_both_slope_dev_lscores.rds")
path_aImp_lscores <- here("mira_objects", "mitml_a_c_both_slope_dev_lscores.rds")
mitml_mImp_lscores <- read_rds(path_mImp_lscores)
mitml_aImp_lscores <- read_rds(path_aImp_lscores)
mira_mImp_lscores <- as.mira(mitml_mImp_lscores)
mira_aImp_lscores <- as.mira(mitml_aImp_lscores)
################################################################################
# initial diagnostics
plot_mImp <- check_model(mira_mImp$analyses[[2]], check = c("qq", "homogeneity"),
                         panel = TRUE)
plot_mImp
plot_aImp <- check_model(mira_aImp$analyses[[2]], check = c("qq", "homogeneity"),
                         panel = TRUE)
plot_aImp
model_performance(mira_mImp$analyses[[2]])
model_performance(mira_aImp$analyses[[2]])

check_collinearity(mira_mImp$analyses[[2]])
check_collinearity(mira_aImp$analyses[[2]])

check_convergence(mira_mImp$analyses[[2]])
check_convergence(mira_aImp$analyses[[2]])

check_autocorrelation(mira_mImp$analyses[[2]])
check_autocorrelation(mira_aImp$analyses[[2]])

check_heterogeneity(mira_mImp$analyses[[2]])
check_heterogeneity(mira_aImp$analyses[[2]])

check_distribution(mira_mImp$analyses[[2]])
check_distribution(mira_aImp$analyses[[2]])

check_heteroscedasticity(mira_mImp$analyses[[2]])
check_heteroscedasticity(mira_aImp$analyses[[2]])

check_outliers(mira_mImp$analyses[[2]])
check_outliers(mira_aImp$analyses[[2]])
# Warning: 10 outliers detected (cases 842, 870, 962, 1107, 1713, 1741, 1767, 
# 1798, 2207, 2243)
# 
# r squared ---------------------------------------------------------------
mImp_r2_df <- map(mira_mImp$analyses, r2) %>% purrr::transpose() %>% 
  data.table::as.data.table() %>% mutate_all(as.numeric) %>% 
  summarise_all(mean)
mImp_r2_df

aImp_r2_df <- map(mira_aImp$analyses, r2) %>% purrr::transpose() %>% 
  data.table::as.data.table() %>% mutate_all(as.numeric) %>% 
  summarise_all(mean)
aImp_r2_df

mImp_r2_lscores_df <- map(mira_mImp_lscores$analyses, r2) %>% 
  purrr::transpose() %>% 
  data.table::as.data.table() %>% mutate_all(as.numeric) %>% 
  summarise_all(mean)
mImp_r2_lscores_df

aImp_r2_lscores_df <- map(mira_aImp_lscores$analyses, r2) %>% 
  purrr::transpose() %>% 
  data.table::as.data.table() %>% mutate_all(as.numeric) %>% 
  summarise_all(mean)
aImp_r2_lscores_df
