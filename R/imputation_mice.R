library(tidyverse)
library(lme4)
library(blme)
library(here)
library(mice)
library(miceadds)
library(micemd)
################################################################################
path <- here("mod_data", "df_for_imputation.csv")
df <- read_csv(path)
df <- df %>% dplyr::mutate(sugu = factor(sugu))
# The continuous data is centered to improve the stability of the estimates
# df_cohort1 <- df %>% dplyr::filter(kohort == 1) %>% dplyr::select(-kohort) %>% 
#   mutate_at(4:43, ~(scale(.x, scale = TRUE) %>% as.vector())) 
# df_cohort2 <- df %>% dplyr::filter(kohort == 2) %>% dplyr::select(-kohort) %>% 
#   mutate_at(4:43, ~(scale(.x, scale = TRUE) %>% as.vector()))

# filter out the row where data is missing for the entire wave
df <- dplyr::filter(df, !rowSums(is.na(df)) >= 64) 
df_cohort1 <- df %>% dplyr::filter(kohort == 1) %>% dplyr::select(-kohort) %>% 
  mutate_at(4:43, ~(scale(.x, scale = TRUE) %>% as.vector())) 
df_cohort2 <- df %>% dplyr::filter(kohort == 2) %>% dplyr::select(-kohort) %>% 
  mutate_at(4:43, ~(scale(.x, scale = TRUE) %>% as.vector()))
################################################################################
# make a separate block for AMIS questionnaire items
block_AMIS <- mice::make.blocks(df_cohort1[, c(1:3, 44:67)], partition = "collect")
block_food <- mice::make.blocks(df_cohort1[, -c(44:67)], partition = "scatter")
blocks = c(block_food, block_AMIS)
################################################################################
# imputation
# cohort 1
path_imp <- here("imputed_data", "mice", "mice_imp_cohort1_filt.rds")
imp_cohort1 <- mice(df_cohort1, blocks = blocks, method = "2l.pmm", maxit = 0)
pred_matrix <- imp_cohort1$predictorMatrix
pred_matrix[, c("kood")] <- pred_matrix[, c("kood")] * -2

# write predictor matrix to hard drive to modify it in spreadsheet software
path <- here("imputed_data", "mice", "mice_pred_matrix.csv")
#  write_csv(as.data.frame(pred_matrix), path)
mod_matrix <- read_csv2(path) %>% as.matrix()
dimnames(mod_matrix) <- dimnames(pred_matrix) 
meth <- imp_cohort1$method
# use `2l.pan` method for continuous values and `2l.pmm` for AMIS scale items
meth[4:43] <- "2l.pan"
imp_cohort1 <- mice(df_cohort1, blocks = blocks, method = meth, m = 100,
                    maxit = 40,
                    predictorMatrix = mod_matrix, printFlag = TRUE, seed = 1984)
plot(imp_cohort1)
saveRDS(imp_cohort1, path_imp)
################################################################################
# cohort 2
path_imp <- here("imputed_data", "mice", "mice_imp_cohort2_filt.rds")
imp_cohort2 <- mice(df_cohort2, blocks = blocks, method = meth, m = 100,
                    maxit = 40, 
                    predictorMatrix = mod_matrix, printFlag = TRUE, seed = 1984)
plot(imp_cohort2)
saveRDS(imp_cohort2, path_imp)
################################################################################
