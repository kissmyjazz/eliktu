library(here)
library(tidyverse)
library(reshape2)
options(scipen=999)
path <- here("mod_data", "AMIS_toitumine_wide.csv")
data <- read_csv(path)

# function to reshape the data into long format
reshape_l <- function(df, value.name = NULL, id.vars = "kood") {
  if (is.null(value.name)) { stop("please provide a name for value variable")}
  df_l <- df %>% melt(id.vars = id.vars,
                      value.name = value.name, variable.name = "age") %>%
  mutate(age = as.numeric(gsub(".*?([0-9]+).*", "\\1", age)))
  df_l
}

# pull out single indicators
df_amis_adapt <- data[, c(1:3, grep("Aimp_", colnames(data)))]
df_amis_maladapt <- data[, c(1, grep("Mimp_", colnames(data)))]
df_amis_CHL <- data[, c(1, grep("CHL_", colnames(data)))]

# cycling data
path <- here("mod_data", "ELIKTU AM_velo.csv")
data_velo <- read_csv(path)
df_wmaxkg <- data_velo[, c(1, grep("Wmaxkg_", colnames(data_velo)))]

# reshape wide data into long
df_amis_adapt_l <- df_amis_adapt %>% reshape_l(id.vars = c("kood", "sugu", "kohort"),
                                          value.name = "Aimp")
df_amis_maladapt_l <- df_amis_maladapt %>% reshape_l(id.vars = c("kood"),
                                               value.name = "Mimp")
df_amis_CHL_l <- df_amis_CHL %>% reshape_l(id.vars = c("kood", "CHL_9"),
                                                     value.name = "CHL")
df_wmaxkg_l <- df_wmaxkg %>% reshape_l(id.vars = c("kood"),
                                                     value.name = "Wmaxkg")
# merge the separate long df-s together
df_long <- Reduce(function(...) merge(..., by = c("kood", "age"), all = FALSE),
                  list(df_amis_adapt_l, df_amis_maladapt_l, df_amis_CHL_l, df_wmaxkg_l)
                  )

path <- here("mod_data", "df_for_lmer_test.csv")
write_csv(df_long, path)
