# application of mixed-effect modelling to ELIKTU data minerals data

library(here)
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
options(scipen=999)
old <- theme_set(theme_bw())

path <- here("mod_data", "df_for_lmer_minerals.csv")
data <- read_csv(path)

################################################################################
# divide data by cohort because cohort 2 does not have CHL_9 scores
# cohort 1 does not have impulsivity scores for age 33, hence I filter data for this age out
kohort_1_df <- data %>% filter(kohort == 1, age != 33) %>% select(-kohort)
kohort_1_df$age_cent <- factor(kohort_1_df$age_cent, levels = c(0, 3, 10),
                               labels = c("15 years", "18 years", "25 years"))

# cohort 2 does not have impulsivity scores for age 15, hence I filter data for this age out
kohort_2_df <- data %>% filter(kohort == 2, age != 15) %>% select(-c(kohort, grep("_9", colnames(data))))
kohort_2_df$age_cent <- factor(kohort_2_df$age_cent, levels = c(3, 10, 18),
                               labels = c("18 years", "25 years", "33 years"))

################################################################################
# Calcium & Mimp
# model 1 for cohort 1
model_c1_calcium_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Calcium_9 + Calcium +
                     (1 | kood),
                   REML = TRUE, data = kohort_1_df)
model_calcium_c1_summary_1 <- summary(model_c1_calcium_mimp, ddf = "Kenward-Roger")
model_calcium_c1_summary_1

# model for cohort 2
model_c2_calcium_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Calcium +
                     (1 | kood),
                   REML = TRUE, data = kohort_2_df)
model_calcium_c2_summary_1 <- summary(model_c2_calcium_mimp, ddf = "Kenward-Roger")
model_calcium_c2_summary_1
################################################################################
# Calcium & Aimp
# model 1 for cohort 1
model_c1_calcium_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Calcium_9 + Calcium +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_calcium_c1_summary_2 <- summary(model_c1_calcium_aimp, ddf = "Kenward-Roger")
model_calcium_c1_summary_2

# model for cohort 2
model_c2_calcium_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Calcium +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_calcium_c2_summary_2 <- summary(model_c2_calcium_aimp, ddf = "Kenward-Roger")
model_calcium_c2_summary_2
################################################################################
# Iodine & Mimp
# model 1 for cohort 1
model_c1_Iodine_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Iodine_9 + Iodine +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Iodine_c1_summary_1 <- summary(model_c1_Iodine_mimp, ddf = "Kenward-Roger")
model_Iodine_c1_summary_1

# model for cohort 2
model_c2_Iodine_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Iodine +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Iodine_c2_summary_1 <- summary(model_c2_Iodine_mimp, ddf = "Kenward-Roger")
model_Iodine_c2_summary_1
################################################################################
# Iodine & Aimp
# model 1 for cohort 1
model_c1_Iodine_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Iodine_9 + Iodine +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Iodine_c1_summary_2 <- summary(model_c1_Iodine_aimp, ddf = "Kenward-Roger")
model_Iodine_c1_summary_2

# model for cohort 2
model_c2_Iodine_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Iodine +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Iodine_c2_summary_2 <- summary(model_c2_Iodine_aimp, ddf = "Kenward-Roger")
model_Iodine_c2_summary_2
################################################################################
# Iron & Mimp
# model 1 for cohort 1
model_c1_Iron_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Iron_9 + Iron +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Iron_c1_summary_1 <- summary(model_c1_Iron_mimp, ddf = "Kenward-Roger")
model_Iron_c1_summary_1

# model for cohort 2
model_c2_Iron_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Iron +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Iron_c2_summary_1 <- summary(model_c2_Iron_mimp, ddf = "Kenward-Roger")
model_Iron_c2_summary_1
################################################################################
# Iron & Aimp
# model 1 for cohort 1
model_c1_Iron_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Iron_9 + Iron +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Iron_c1_summary_2 <- summary(model_c1_Iron_aimp, ddf = "Kenward-Roger")
model_Iron_c1_summary_2

# model for cohort 2
model_c2_Iron_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Iron +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Iron_c2_summary_2 <- summary(model_c2_Iron_aimp, ddf = "Kenward-Roger")
model_Iron_c2_summary_2
################################################################################
# Magnesium & Mimp
# model 1 for cohort 1
model_c1_Magnesium_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Magnesium_9 + Magnesium +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Magnesium_c1_summary_1 <- summary(model_c1_Magnesium_mimp, ddf = "Kenward-Roger")
model_Magnesium_c1_summary_1

# model for cohort 2
model_c2_Magnesium_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Magnesium +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Magnesium_c2_summary_1 <- summary(model_c2_Magnesium_mimp, ddf = "Kenward-Roger")
model_Magnesium_c2_summary_1
################################################################################
# Magnesium & Aimp
# model 1 for cohort 1
model_c1_Magnesium_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Magnesium_9 + Magnesium +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Magnesium_c1_summary_2 <- summary(model_c1_Magnesium_aimp, ddf = "Kenward-Roger")
model_Magnesium_c1_summary_2

# model for cohort 2
model_c2_Magnesium_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Magnesium +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Magnesium_c2_summary_2 <- summary(model_c2_Magnesium_aimp, ddf = "Kenward-Roger")
model_Magnesium_c2_summary_2
################################################################################
# Manganese & Mimp
# model 1 for cohort 1
model_c1_Manganese_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Mangane_9 + Manganese +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Manganese_c1_summary_1 <- summary(model_c1_Manganese_mimp, ddf = "Kenward-Roger")
model_Manganese_c1_summary_1

# model for cohort 2
model_c2_Manganese_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Manganese +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Manganese_c2_summary_1 <- summary(model_c2_Manganese_mimp, ddf = "Kenward-Roger")
model_Manganese_c2_summary_1
################################################################################
# Manganese & Aimp
# model 1 for cohort 1
model_c1_Manganese_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Mangane_9 + Manganese +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Manganese_c1_summary_2 <- summary(model_c1_Manganese_aimp, ddf = "Kenward-Roger")
model_Manganese_c1_summary_2

# model for cohort 2
model_c2_Manganese_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Manganese +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Manganese_c2_summary_2 <- summary(model_c2_Manganese_aimp, ddf = "Kenward-Roger")
model_Manganese_c2_summary_2
################################################################################
# Phosphorus & Mimp
# model 1 for cohort 1
model_c1_Phosphorus_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Phosphor_9 + Phosphorus +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Phosphorus_c1_summary_1 <- summary(model_c1_Phosphorus_mimp, ddf = "Kenward-Roger")
model_Phosphorus_c1_summary_1

# model for cohort 2
model_c2_Phosphorus_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Phosphorus +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Phosphorus_c2_summary_1 <- summary(model_c2_Phosphorus_mimp, ddf = "Kenward-Roger")
model_Phosphorus_c2_summary_1
################################################################################
# Phosphorus & Aimp
# model 1 for cohort 1
model_c1_Phosphorus_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Phosphor_9 + Phosphorus +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Phosphorus_c1_summary_2 <- summary(model_c1_Phosphorus_aimp, ddf = "Kenward-Roger")
model_Phosphorus_c1_summary_2

# model for cohort 2
model_c2_Phosphorus_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Phosphorus +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Phosphorus_c2_summary_2 <- summary(model_c2_Phosphorus_aimp, ddf = "Kenward-Roger")
model_Phosphorus_c2_summary_2
################################################################################
# Potassium & Mimp
# model 1 for cohort 1
model_c1_Potassium_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Potassium_9 + Potassium +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Potassium_c1_summary_1 <- summary(model_c1_Potassium_mimp, ddf = "Kenward-Roger")
model_Potassium_c1_summary_1

# model for cohort 2
model_c2_Potassium_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Potassium +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Potassium_c2_summary_1 <- summary(model_c2_Potassium_mimp, ddf = "Kenward-Roger")
model_Potassium_c2_summary_1
################################################################################
# Potassium & Aimp
# model 1 for cohort 1
model_c1_Potassium_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Potassium_9 + Potassium +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Potassium_c1_summary_2 <- summary(model_c1_Potassium_aimp, ddf = "Kenward-Roger")
model_Potassium_c1_summary_2

# model for cohort 2
model_c2_Potassium_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Potassium +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Potassium_c2_summary_2 <- summary(model_c2_Potassium_aimp, ddf = "Kenward-Roger")
model_Potassium_c2_summary_2
################################################################################
# Selenium & Mimp
# model 1 for cohort 1
model_c1_Selenium_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Selenium_9 + Selenium +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Selenium_c1_summary_1 <- summary(model_c1_Selenium_mimp, ddf = "Kenward-Roger")
model_Selenium_c1_summary_1

# model for cohort 2
model_c2_Selenium_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Selenium +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Selenium_c2_summary_1 <- summary(model_c2_Selenium_mimp, ddf = "Kenward-Roger")
model_Selenium_c2_summary_1
################################################################################
# Selenium & Aimp
# model 1 for cohort 1
model_c1_Selenium_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Selenium_9 + Selenium +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Selenium_c1_summary_2 <- summary(model_c1_Selenium_aimp, ddf = "Kenward-Roger")
model_Selenium_c1_summary_2

# model for cohort 2
model_c2_Selenium_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Selenium +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Selenium_c2_summary_2 <- summary(model_c2_Selenium_aimp, ddf = "Kenward-Roger")
model_Selenium_c2_summary_2
################################################################################
# Sodium & Mimp
# model 1 for cohort 1
model_c1_Sodium_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Sodium_9 + Sodium +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Sodium_c1_summary_1 <- summary(model_c1_Sodium_mimp, ddf = "Kenward-Roger")
model_Sodium_c1_summary_1

# model for cohort 2
model_c2_Sodium_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Sodium +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Sodium_c2_summary_1 <- summary(model_c2_Sodium_mimp, ddf = "Kenward-Roger")
model_Sodium_c2_summary_1
################################################################################
# Sodium & Aimp
# model 1 for cohort 1
model_c1_Sodium_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Sodium_9 + Sodium +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Sodium_c1_summary_2 <- summary(model_c1_Sodium_aimp, ddf = "Kenward-Roger")
model_Sodium_c1_summary_2

# model for cohort 2
model_c2_Sodium_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Sodium +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Sodium_c2_summary_2 <- summary(model_c2_Sodium_aimp, ddf = "Kenward-Roger")
model_Sodium_c2_summary_2
################################################################################
# Zink & Mimp
# model 1 for cohort 1
model_c1_Zink_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Zink +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Zink_c1_summary_1 <- summary(model_c1_Zink_mimp, ddf = "Kenward-Roger")
model_Zink_c1_summary_1

# model for cohort 2
model_c2_Zink_mimp <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + Zink +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Zink_c2_summary_1 <- summary(model_c2_Zink_mimp, ddf = "Kenward-Roger")
model_Zink_c2_summary_1
################################################################################
# Zink & Aimp
# model 1 for cohort 1
model_c1_Zink_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Zink_9 + Zink +
                                (1 | kood),
                              REML = TRUE, data = kohort_1_df)
model_Zink_c1_summary_2 <- summary(model_c1_Zink_aimp, ddf = "Kenward-Roger")
model_Zink_c1_summary_2

# model for cohort 2
model_c2_Zink_aimp <- lmer(Aimp ~ Mimp + age_cent + Wmaxkg + sugu + Zink +
                                (1 | kood),
                              REML = TRUE, data = kohort_2_df)
model_Zink_c2_summary_2 <- summary(model_c2_Zink_aimp, ddf = "Kenward-Roger")
model_Zink_c2_summary_2
################################################################################
# Zink & Aimp ver 2
# model 1 for cohort 1
model_c1_Zink_aimp_2 <- lmer(Aimp ~ Mimp + age_cent + sugu + Zink +
                             (1 | kood),
                           REML = TRUE, data = kohort_1_df)
model_Zink_c1_summary_3 <- summary(model_c1_Zink_aimp_2, ddf = "Kenward-Roger")
model_Zink_c1_summary_3

# model for cohort 2
model_c2_Zink_aimp_2 <- lmer(Aimp ~ Mimp + age_cent + sugu + Zink +
                             (1 | kood),
                           REML = TRUE, data = kohort_2_df)
model_Zink_c2_summary_3 <- summary(model_c2_Zink_aimp_2, ddf = "Kenward-Roger")
model_Zink_c2_summary_3
################################################################################
# Zink & Fast ver 2
# model 1 for cohort 1
model_c1_Zink_Fast_2 <- lmer(Fast ~ Mimp + age_cent + sugu + Zink +
                               (1 | kood),
                             REML = TRUE, data = kohort_1_df)
model_Zink_c1_summary_4 <- summary(model_c1_Zink_Fast_2, ddf = "Kenward-Roger")
model_Zink_c1_summary_4

# model for cohort 2
model_c2_Zink_Fast_2 <- lmer(Fast ~ Mimp + age_cent + sugu + Zink +
                               (1 | kood),
                             REML = TRUE, data = kohort_2_df)
model_Zink_c2_summary_5 <- summary(model_c2_Zink_Fast_2, ddf = "Kenward-Roger")
model_Zink_c2_summary_5
################################################################################
# Zink & Excite ver 2
# model 1 for cohort 1
model_c1_Zink_Excite_2 <- lmer(Excite ~ Mimp + age_cent + sugu + Zink +
                               (1 | kood),
                             REML = TRUE, data = kohort_1_df)
model_Zink_c1_summary_4 <- summary(model_c1_Zink_Excite_2, ddf = "Kenward-Roger")
model_Zink_c1_summary_4

# model for cohort 2
model_c2_Zink_Excite_2 <- lmer(Excite ~ Mimp + age_cent + sugu + Zink +
                               (1 | kood),
                             REML = TRUE, data = kohort_2_df)
model_Zink_c2_summary_4 <- summary(model_c2_Zink_Excite_2, ddf = "Kenward-Roger")
model_Zink_c2_summary_4
################################################################################
################################################################################
