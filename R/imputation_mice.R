library(tidyverse)
library(lme4)
library(here)
library(mice)
library(miceadds)
################################################################################
path <- here("mod_data", "df_for_imputation.csv")
df <- read_csv(path)
df <- df %>% dplyr::mutate(sugu = factor(sugu))
df_cohort1 <- df %>% dplyr::filter(kohort == 1) %>% dplyr::select(-kohort)
df_cohort2 <- df %>% dplyr::filter(kohort == 2) %>% dplyr::select(-kohort)
################################################################################
# make a separate block for AMIS questionnaire items
block_AMIS <- mice::make.blocks(df_cohort1[, c(1:3, 44:67)], partition = "collect")
block_food <- mice::make.blocks(df_cohort1[, -c(44:67)], partition = "collect")
blocks = c(block_AMIS, block_food)
################################################################################
# imputation
# cohort 1
path_imp <- here("imputed_data", "mice_imp_cohort1.rds")
imp_cohort1 <- mice(df_cohort1, blocks = blocks, method = "2l.pmm", maxit = 0)
pred_matrix <- imp_cohort1$predictorMatrix
pred_matrix[, c("kood")] <- pred_matrix[, c("kood")] * -2
imp_cohort1 <- mice(df_cohort1, blocks = blocks, method = "2l.pmm", m = 100,
                    maxit = 100,
                    predictorMatrix = pred_matrix, printFlag = TRUE)
saveRDS(imp_cohort1, path_imp)
################################################################################
# cohort 2
path_imp <- here("imputed_data", "mice_imp_cohort2.rds")
imp_cohort2 <- mice(df_cohort2, blocks = blocks, method = "2l.pmm", maxit = 0)
pred_matrix <- imp_cohort2$predictorMatrix
pred_matrix[, c("kood")] <- pred_matrix[, c("kood")] * -2
imp_cohort2 <- mice(df_cohort2, blocks = blocks, method = "2l.pmm", m = 100,
                    maxit = 100,
                    predictorMatrix = pred_matrix, printFlag = TRUE)
saveRDS(imp_cohort2, path_imp)
################################################################################
