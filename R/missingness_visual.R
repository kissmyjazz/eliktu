library(tidyverse)
library(here)
library(JointAI)
library(naniar)
library(huxtable)

tab_NA <- function(df) {
  cbind.data.frame(count = sort(colSums(is.na(df)), decreasing = TRUE),
        percent = round(sort(colMeans(is.na(df)), decreasing = TRUE) * 100, 2)) %>%
    tibble::rownames_to_column(var = "variable")
 }

print_console <- function(df) {
  names(df)[[2]] <- c("average % missing")
  hux(df) %>% set_bold(row = 1, col = everywhere, value = TRUE) %>%
    set_all_borders(TRUE) %>% print_screen()
}

path <- here("mod_data", "df_for_imputation.csv")
df <- read_csv(path)

df_cohort1 <- df %>% dplyr::filter(kohort == 1) %>% dplyr::select(-kohort)
df_cohort2 <- df %>% dplyr::filter(kohort == 2) %>% dplyr::select(-kohort)

# number and proportion of missing values per variable
cohort1_missing <- tab_NA(df_cohort1)

cohort2_missing <- tab_NA(df_cohort2)

# tabulate by age and missing values
df_cohort1_age <- df_cohort1 %>% dplyr::group_by(age) %>%
  dplyr::group_modify(~ tab_NA(.x)) %>% ungroup() %>% group_by(age) %>%
  summarise(`mean_%_missing` = mean(percent))

df_cohort2_age <- df_cohort2 %>% dplyr::group_by(age) %>%
  dplyr::group_modify(~ tab_NA(.x)) %>% dplyr::ungroup() %>% group_by(age) %>%
  summarise(`mean_%_missing` = mean(percent))

# pretty print results to console
print_console(df_cohort1_age)
print_console(df_cohort2_age)

df_cohort1_AMIS <- df_cohort1[, grep("AMIS", colnames(df_cohort1))] %>%
  mutate_at(vars(matches("AMIS")), ordered)

df_cohort2_AMIS <- df_cohort2[, grep("AMIS", colnames(df_cohort2))] %>%
  mutate_at(vars(matches("AMIS")), ordered)

################################################################################
# JointAI visualisation of AMIS questionnaire
# cohort 1
par(mgp = c(3,1,0), mar = c(4.8, 3.1, 3.1, 1))
g_cohort1_AMIS_1_12 <- JointAI::plot_all(as.data.frame(df_cohort1_AMIS[, 1:12]),
                                         allNA = FALSE,
                  fill = c(rep('#e30f41', 5), grey(0.7)),
                  border ='#34111b', ncol = 4)
g_cohort1_AMIS_13_24 <- JointAI::plot_all(as.data.frame(df_cohort1_AMIS[, 13:24]),
                                         allNA = FALSE,
                                         fill = c(rep('#e30f41', 5), grey(0.7)),
                                         border ='#34111b', ncol = 4)
################################################################################
# cohort 2
g_cohort2_AMIS_1_12 <- JointAI::plot_all(as.data.frame(df_cohort2_AMIS[, 1:12]),
                                         allNA = FALSE,
                                         fill = c(rep('#e30f41', 5), grey(0.7)),
                                         border ='#34111b', ncol = 4)
g_cohort2_AMIS_13_24 <- JointAI::plot_all(as.data.frame(df_cohort2_AMIS[, 13:24]),
                                          allNA = FALSE,
                                          fill = c(rep('#e30f41', 5), grey(0.7)),
                                          border ='#34111b', ncol = 4)
################################################################################
# JointAI visualisation of other items
df_cohort1_other <- df_cohort1[, -c(1:3, 44:67)]
df_cohort2_other <- df_cohort2[, -c(1:3, 44:67)]
par(mgp = c(3,1,0), mar = c(3, 4.5, 3.1, 4))
# cohort 1
g_cohort1_other_1_20 <- JointAI::plot_all(as.data.frame(df_cohort1_other[, 1:20]),
                                         allNA = FALSE,
                                         fill = '#e30f41', breaks = 50,
                                         border ='#e30f41', ncol = 5)
g_cohort1_other_21_40 <- JointAI::plot_all(as.data.frame(df_cohort1_other[, 21:40]),
                                          allNA = FALSE,
                                          fill = '#e30f41', breaks = 50,
                                          border ='#e30f41', ncol = 5)
# cohort 2
g_cohort2_other_1_20 <- JointAI::plot_all(as.data.frame(df_cohort2_other[, 1:20]),
                                          allNA = FALSE,
                                          fill = '#e30f41', breaks = 50,
                                          border ='#e30f41', ncol = 5)
g_cohort2_other_21_40 <- JointAI::plot_all(as.data.frame(df_cohort2_other[, 21:40]),
                                           allNA = FALSE,
                                           fill = '#e30f41', breaks = 50,
                                           border ='#e30f41', ncol = 5)
################################################################################
