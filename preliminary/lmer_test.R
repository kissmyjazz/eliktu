# application of mixed-effect modelling to ELIKTU data

library(here)
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
options(scipen=999)
old <- theme_set(theme_bw())

path <- here("mod_data", "df_for_lmer_test.csv")
data <- read_csv(path)
data$sugu <- factor(data$sugu, levels = c(1, 2), labels = c("man", "woman"))

# divide data by cohort because cohort 2 does not have CHL_9 scores
# cohort 1 does not have impulsivity scores for age 33, hence I filter data for this age out
kohort_1_df <- data %>% filter(kohort == 1, age != 33) %>% select(-kohort)
kohort_1_df$age_cent <- factor(kohort_1_df$age_cent, levels = c(0, 3, 10),
          labels = c("15 years", "18 years", "25 years"))

# cohort 2 does not have impulsivity scores for age 15, hence I filter data for this age out
kohort_2_df <- data %>% filter(kohort == 2, age != 15) %>% select(-c(kohort, CHL_9))
kohort_2_df$age_cent <- factor(kohort_2_df$age_cent, levels = c(3, 10, 18),
                               labels = c("18 years", "25 years", "33 years"))

# summary graphs
# Cohort 1
g1 <- ggplot(kohort_1_df, aes(x = age, y = Mimp)) + geom_line(alpha = 0.1, aes(group = kood)) +
  stat_summary(fun.data = 'mean_sdl',fun.args = list(mult = 1), geom = 'smooth', se = TRUE,
              colour = "coral", size = 1.8, fill = "palegreen") +
  scale_x_continuous(breaks = c(15, 18, 25)) +
  labs(y = "Maladaptive impulsivity score",
       title = "Maldaptive impulsivity declined with age in cohort 1") +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
g1

# Cohort 2
g2 <- ggplot(kohort_2_df, aes(x = age, y = Mimp)) + geom_line(alpha = 0.1, aes(group = kood)) +
  stat_summary(fun.data = 'mean_sdl',fun.args = list(mult = 1), geom = 'smooth', se = TRUE,
               colour = "coral", size = 1.8, fill = "palegreen") +
  scale_x_continuous(breaks = c(18, 25, 33)) +
  labs(y = "Maladaptive impulsivity score",
       title = "Maldaptive impulsivity declined with age in cohort 2") +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
g2

# model 1 for cohort 1
model_c1_1 <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + CHL_9 + CHL +
                     (1 | kood),
                          REML = TRUE, data = kohort_1_df)
model_c1_summary_1 <- summary(model_c1_1, ddf = "Kenward-Roger")
model_c1_summary_1


# model for cohort 2
model_c2_1 <- lmer(Mimp ~ Aimp + age_cent + Wmaxkg + sugu + CHL +
                     (1 | kood),
                   REML = TRUE, data = kohort_2_df)
model_c2_summary_1 <- summary(model_c2_1, ddf = "Kenward-Roger")
model_c2_summary_1
