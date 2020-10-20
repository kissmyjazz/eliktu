################################################################################
# In this file I run multievel lasso data mining on original data of cohort 1 
# with missing observations to select the candidate variables for in depth 
# analysis on imputed data

library(tidyverse)
library(here)
library(glmmLasso)
options(scipen = 6)

path <- here("mod_data", "df_for_imputation.csv")
df <- read_csv(path)

as_numeric_factor <- function(x) {as.numeric(levels(x))[x]}
################################################################################
# score items for adaptive and maladaptive impusivity composite scales
# because of the poor factor loadings I leave out item 8 from the composite
# maladaptive impulsivity score
df$sugu <- factor(df$sugu, levels = c("male", "female"), labels = c("male", "female"))
# centre age at age 18
df$age_cent <- df$age - 18

################################################################################
# cohort 1
# cohort 1 does not have impulsivity scores for age 33, hence I filter data for 
# this age out
cohort_1_df <- df %>% filter(kohort == 1, age != 33) %>% select(-kohort)
cohort_1_df$age_cent_f <- factor(cohort_1_df$age_cent, levels = c(-3, 0, 7),
                               labels = c("15 years", "18 years", "25 years"))
cohort_1_df <- cohort_1_df %>% rowwise() %>% 
  dplyr::mutate(aImp = sum(c(AMIS3, (6-AMIS7), AMIS11, AMIS15, AMIS19, AMIS23, 
                      AMIS2, (6-AMIS6), AMIS10, AMIS14, AMIS18, AMIS22)),
         mImp = sum(c(AMIS1, AMIS5, AMIS9, AMIS13, AMIS17, (6-AMIS21),
                      AMIS4, AMIS12, AMIS16, AMIS20, AMIS24))) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-c(AMIS1:AMIS24)) %>% 
  dplyr::mutate(sugu_n = recode_factor(sugu, "female" = 1, "male" = 0), 
                kood = factor(kood)) %>% 
  dplyr::mutate_at(c(4:43, 46:47), ~(scale(.x, scale = TRUE) %>% as.vector()))

# cohort 2
# cohort 2 does not have impulsivity scores for age 15, hence I filter data for 
# this age out
cohort_2_df <- df %>% filter(kohort == 2, age != 15) %>% select(-kohort)
cohort_2_df$age_cent_f <- factor(cohort_2_df$age_cent, levels = c(0, 7, 15),
                                 labels = c("18 years", "25 years", "33 years"))
cohort_2_df <- cohort_2_df %>% rowwise() %>% 
  dplyr::mutate(aImp = sum(c(AMIS3, (6-AMIS7), AMIS11, AMIS15, AMIS19, AMIS23, 
                             AMIS2, (6-AMIS6), AMIS10, AMIS14, AMIS18, AMIS22)),
                mImp = sum(c(AMIS1, AMIS5, AMIS9, AMIS13, AMIS17, (6-AMIS21),
                             AMIS4, AMIS12, AMIS16, AMIS20, AMIS24))) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-c(AMIS1:AMIS24)) %>% 
  dplyr::mutate(sugu_n = recode_factor(sugu, "female" = 1, "male" = 0), 
                kood = factor(kood)) %>% 
  dplyr::mutate_at(c(4:43, 46:47), ~(scale(.x, scale = TRUE) %>% as.vector()))

################################################################################
names <- colnames(cohort_1_df)
fmla_aImp <- formula(paste("aImp ~ ", "as.factor(age_cent_f) + ", 
                           "as.factor(sugu) + ",
                           paste(names[-c(1:3, 44:46, 48)],collapse="+")))
fmla_mImp <- formula(paste("mImp ~ ", "as.factor(age_cent_f) + ", 
                            "as.factor(sugu) + ",
                            paste(names[-c(1:3, 44:45, 47:48)],collapse="+")))
################################################################################
# glmm lasso for aImp
# cohort 1
cohort_1_df_noNA <- na.omit(cohort_1_df)
# lambda == 10
l1_10 <- glmmLasso(fmla_aImp, rnd = list(kood=~1),
          lambda = 10, data = cohort_1_df_noNA, switch.NR = TRUE,
          control = list(center = FALSE, standardize = FALSE, method = "EM",
                         print.iter=TRUE))
coef_1_10 <- sort(abs(coef(l1_10)), decreasing = TRUE)

# lambda == 50
l1_50 <- glmmLasso(fmla_aImp, rnd = list(kood=~1),
                   lambda = 50, data = cohort_1_df_noNA, switch.NR = TRUE,
                   control = list(center = FALSE, standardize = FALSE, method = "EM",
                                  print.iter=TRUE))
coef_1_50 <- sort(abs(coef(l1_50)), decreasing = TRUE)

# cohort 2
cohort_2_df_noNA <- na.omit(cohort_2_df)
# lambda == 10
l2_10 <- glmmLasso(fmla_aImp, rnd = list(kood=~1),
                   lambda = 10, data = cohort_2_df_noNA, switch.NR = TRUE,
                   control = list(center = FALSE, standardize = FALSE, method = "REML",
                                  print.iter=TRUE))
coef_2_10 <- sort(abs(coef(l2_10)), decreasing = TRUE)

# lambda == 50
l2_50 <- glmmLasso(fmla_aImp, rnd = list(kood=~1),
                   lambda = 50, data = cohort_2_df_noNA, switch.NR = TRUE,
                   control = list(center = FALSE, standardize = FALSE, method = "REML",
                                  print.iter=TRUE))
coef_2_50 <- sort(abs(coef(l2_50)), decreasing = TRUE)
################################################################################
################################################################################
# glmm lasso for mImp
# cohort 1
# lambda == 10
l1_10_m <- glmmLasso(fmla_mImp, rnd = list(kood=~1),
                   lambda = 10, data = cohort_1_df_noNA, switch.NR = TRUE,
                   control = list(center = FALSE, standardize = FALSE, method = "EM",
                                  print.iter=TRUE))
coef_1_10_m <- sort(abs(coef(l1_10_m)), decreasing = TRUE)

# lambda == 50
l1_50_m <- glmmLasso(fmla_mImp, rnd = list(kood=~1),
                   lambda = 50, data = cohort_1_df_noNA, switch.NR = TRUE,
                   control = list(center = FALSE, standardize = FALSE, method = "EM",
                                  print.iter=TRUE))
coef_1_50_m <- sort(abs(coef(l1_50_m)), decreasing = TRUE)

# cohort 2
cohort_2_df_noNA <- na.omit(cohort_2_df)
# lambda == 10
l2_10_m <- glmmLasso(fmla_mImp, rnd = list(kood=~1),
                   lambda = 10, data = cohort_2_df_noNA, switch.NR = TRUE,
                   control = list(center = FALSE, standardize = FALSE, method = "REML",
                                  print.iter=TRUE))
coef_2_10_m <- sort(abs(coef(l2_10_m)), decreasing = TRUE)

# lambda == 50
l2_50_m <- glmmLasso(fmla_mImp, rnd = list(kood=~1),
                   lambda = 50, data = cohort_2_df_noNA, switch.NR = TRUE,
                   control = list(center = FALSE, standardize = FALSE, method = "REML",
                                  print.iter=TRUE))
coef_2_50_m <- sort(abs(coef(l2_50_m)), decreasing = TRUE)
################################################################################