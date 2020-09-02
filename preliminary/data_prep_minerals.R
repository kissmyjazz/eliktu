library(here)
library(tidyverse)
library(reshape2)
library(psych)
options(scipen=999)
path <- here("mod_data", "ELIKTU AMIS_cal_BMI_miner.csv")
data <- read_csv(path)
data_tr <- data[, 77:159]
desc <- psych::describe(data_tr)
desc <- cbind(indicator=row.names(desc), desc)
desc <- desc[order(desc$indicator), ]
write_csv(desc, here("summary_data", "minerals_summary.csv"))

# function to reshape the data into long format
reshape_l <- function(df, value.name = NULL, id.vars = "kood") {
  if (is.null(value.name)) {stop("please provide a name for value variable")}
  df_l <- df %>% melt(id.vars = id.vars,
                      value.name = value.name, variable.name = "age") %>%
    mutate(age = as.numeric(gsub(".*?([0-9]+).*", "\\1", age)))
  df_l
}

################################################################################
# impulsivity
path <- here("mod_data", "AMIS_toitumine_wide.csv")
data_food <- read_csv(path)

# pull out single indicators
df_amis_adapt <- data_food[, c(1:3, grep("Aimp_", colnames(data_food)))]
df_amis_maladapt <- data_food[, c(1, grep("Mimp_", colnames(data_food)))]
# subscales of impulsivity
df_amis_fast <- data_food[, c(1, grep("Fast_", colnames(data_food)))]
df_amis_excite <- data_food[, c(1, grep("Excite_", colnames(data_food)))]

################################################################################
# minerals
# pull out single indicators, age 9 measurements are only in cohort 1
df_calcium <- data[, c(1, grep("Calcium_", colnames(data)))]
df_iodine <- data[, c(1, grep("Iodine_", colnames(data)))]
df_iron <- data[, c(1, grep("Iron_", colnames(data)))]
df_magnesium <- data[, c(1, grep("Magnesium_", colnames(data)))]
df_manganese <- data[, c(1, grep("Mangane_", colnames(data)))]
df_phosphorus <- data[, c(1, grep("Phosphor_", colnames(data)))]
df_potassium <- data[, c(1, grep("Potassium_", colnames(data)))]
df_selenium <- data[, c(1, grep("Selenium_", colnames(data)))]
df_sodium <- data[, c(1, grep("Sodium_", colnames(data)))]
df_zink <- data[, c(1, grep("Zink_", colnames(data)))]

# energy and body weight
df_kcal <- data[, c(1, grep("kcalday_", colnames(data)))]
df_BMI <- data[, c(1, grep("BMI_", colnames(data)))]

# pull out single indicators, measurements are only for ages 18-33
df_chrome <- data[, c(1, grep("Chrome_", colnames(data)))]
df_copper <- data[, c(1, grep("Copper_", colnames(data)))]

################################################################################
# cycling data
path <- here("mod_data", "ELIKTU AM_velo.csv")
data_velo <- read_csv(path)
df_wmaxkg <- data_velo[, c(1, grep("Wmaxkg_", colnames(data_velo)))]

################################################################################
# reshape wide data into long
df_amis_adapt_l <- df_amis_adapt %>% reshape_l(id.vars = c("kood", "sugu", "kohort"),
                                               value.name = "Aimp")
df_amis_maladapt_l <- df_amis_maladapt %>% reshape_l(id.vars = c("kood"),
                                                     value.name = "Mimp")
df_amis_fast_l <- df_amis_fast %>% reshape_l(id.vars = c("kood"),
                                                     value.name = "Fast")
df_amis_excite_l <- df_amis_excite %>% reshape_l(id.vars = c("kood"),
                                             value.name = "Excite")
df_wmaxkg_l <- df_wmaxkg %>% reshape_l(id.vars = c("kood"),
                                       value.name = "Wmaxkg")
df_calcium_l <- df_calcium %>% reshape_l(id.vars = c("kood", "Calcium_9"),
                                        value.name = "Calcium")
df_iodine_l <- df_iodine %>% reshape_l(id.vars = c("kood", "Iodine_9"),
                                       value.name = "Iodine")
df_iron_l <- df_iron %>% reshape_l(id.vars = c("kood", "Iron_9"),
                                     value.name = "Iron")
df_magnesium_l <- df_magnesium %>% reshape_l(id.vars = c("kood", "Magnesium_9"),
                                          value.name = "Magnesium")
df_manganese_l <- df_manganese %>% reshape_l(id.vars = c("kood", "Mangane_9"),
                                          value.name = "Manganese")
df_phosphorus_l <- df_phosphorus %>% reshape_l(id.vars = c("kood", "Phosphor_9"),
                                           value.name = "Phosphorus")
df_potassium_l <- df_potassium %>% reshape_l(id.vars = c("kood", "Potassium_9"),
                                               value.name = "Potassium")
df_selenium_l <- df_selenium %>% reshape_l(id.vars = c("kood", "Selenium_9"),
                                         value.name = "Selenium")
df_sodium_l <- df_sodium %>% reshape_l(id.vars = c("kood", "Sodium_9"),
                                       value.name = "Sodium")
df_zink_l <- df_zink %>% reshape_l(id.vars = c("kood", "Zink_9"),
                                     value.name = "Zink")
df_kcal_l <- df_kcal %>% reshape_l(id.vars = c("kood"),
                                       value.name = "kcal")
df_BMI_l <- df_BMI %>% reshape_l(id.vars = c("kood"),
                                   value.name = "BMI")
################################################################################
# merge the separate long df-s together

lst <- list(df_amis_adapt_l, df_amis_maladapt_l,df_wmaxkg_l, df_calcium_l,
            df_iodine_l, df_iron_l, df_magnesium_l, df_manganese_l, df_phosphorus_l,
            df_potassium_l, df_selenium_l, df_sodium_l, df_zink_l, df_kcal_l, df_BMI_l,
            df_amis_excite_l, df_amis_fast_l)

df_long <- Reduce(function(...) merge(..., by = c("kood", "age"), all = FALSE),
                  lst
)


################################################################################
# center age at 15 years
df_long$age_cent <- (df_long$age - 15)
df_long$sugu <- factor(df_long$sugu, levels = c(1, 2), labels = c("man", "woman"))

################################################################################
path <- here("mod_data", "df_for_lmer_minerals.csv")
write_csv(df_long, path)

################################################################################
