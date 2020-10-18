library(tidyverse)
library(here)
path <- here("mod_data", "df_for_imputation.csv")
as_numeric_factor <- function(x) {as.numeric(levels(x))[x]}
df <- read_csv(path)
df <- df %>% dplyr::mutate(sugu = factor(sugu),
                           sugu = fct_recode(sugu, `0` = "female", `1` = "male"))
################################################################################
# the data is prepared for EFA by FACTOR program in Windows
# make 6 DF-s of AMIS questionnaire items only separated for each cohort and age

df_cohort1_age15 <- df %>% dplyr::filter(kohort == 1, age == 15) %>% 
  dplyr::select(AMIS1:AMIS24)
df_cohort1_age18 <- df %>% dplyr::filter(kohort == 1, age == 18) %>% 
  dplyr::select(AMIS1:AMIS24)
df_cohort1_age25 <- df %>% dplyr::filter(kohort == 1, age == 25) %>% 
  dplyr::select(AMIS1:AMIS24)

df_cohort2_age18 <- df %>% dplyr::filter(kohort == 2, age == 18) %>% 
  dplyr::select(AMIS1:AMIS24)
df_cohort2_age25 <- df %>% dplyr::filter(kohort == 2, age == 25) %>% 
  dplyr::select(AMIS1:AMIS24)
df_cohort2_age33 <- df %>% dplyr::filter(kohort == 2, age == 33) %>% 
  dplyr::select(AMIS1:AMIS24)

path_cohort1_age15 <- here("mod_data", "df_for_factor_c1_a15.dat")
path_cohort1_age18 <- here("mod_data", "df_for_factor_c1_a18.dat")
path_cohort1_age25 <- here("mod_data", "df_for_factor_c1_a25.dat")

path_cohort2_age18 <- here("mod_data", "df_for_factor_c2_a18.dat")
path_cohort2_age25 <- here("mod_data", "df_for_factor_c2_a25.dat")
path_cohort2_age33 <- here("mod_data", "df_for_factor_c2_a33.dat")

write_delim(df_cohort1_age15, path_cohort1_age15, na = "999", col_names = FALSE)
write_delim(df_cohort1_age18, path_cohort1_age18, na = "999", col_names = FALSE)
write_delim(df_cohort1_age25, path_cohort1_age25, na = "999", col_names = FALSE)

write_delim(df_cohort2_age18, path_cohort2_age18, na = "999", col_names = FALSE)
write_delim(df_cohort2_age25, path_cohort2_age25, na = "999", col_names = FALSE)
write_delim(df_cohort2_age33, path_cohort2_age33, na = "999", col_names = FALSE)

################################################################################
# cohorts 1 & 2
df_cohort1 <- df %>% dplyr::filter(kohort == 1) %>% 
  dplyr::select(AMIS1:AMIS24)
df_cohort2 <- df %>% dplyr::filter(kohort == 2) %>% 
  dplyr::select(AMIS1:AMIS24)

path_cohort1 <- here("mod_data", "df_for_factor_c1.dat")
path_cohort2 <- here("mod_data", "df_for_factor_c2.dat")

write_delim(df_cohort1, path_cohort1, na = "999", col_names = FALSE)
write_delim(df_cohort2, path_cohort2, na = "999", col_names = FALSE)
################################################################################
# cohorts together
df_both_cohorts <- df %>% dplyr::select(AMIS1:AMIS24)

path_both_cohorts <- here("mod_data", "df_for_factor_all.dat")

write_delim(df_both_cohorts, path_both_cohorts, na = "999", col_names = FALSE)

