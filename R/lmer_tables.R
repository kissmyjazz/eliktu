library(here)
library(tidyverse)
library(mitml)
library(mice)
library(papaja)
library(ggsci)
library(modelsummary)
library(broom)
library(broom.mixed)
library(easystats)
library(broomExtra)
library(gtsummary)
library(huxtable)

source("R/utils/tidy_melded.R")

options(scipen=999)
theme_set(theme_apa(base_size = 14)) 

# load objects ------------------------------------------------------------
path_mImp <- here("mira_objects", "mitml_m_c_both_slope.rds")
path_aImp <- here("mira_objects", "mitml_a_c_both_slope.rds")

mitml_mImp <- read_rds(path_mImp)
mitml_aImp <- read_rds(path_aImp)

mira_mImp <- as.mira(mitml_mImp)
mira_aImp <- as.mira(mitml_aImp)


# make custom classed objects `melded` to use with custom tidy functions----
mira_mImp_melded <- mira_mImp
class(mira_mImp_melded) <- "melded"

mira_aImp_melded <- mira_aImp
class(mira_aImp_melded) <- "melded"

# make tables -------------------------------------------------------------
# adj_mImp_model <- parameters(mira_mImp) %>% 
#   select(Parameter, Coefficient, CI_low, CI_high, p) %>%
#   parameters_table(stars = TRUE)

# `gtsummary` does not work, use `huxtable` 

adj_mImp_model_tbl <- mira_mImp_melded %>% 
  huxreg(statistics = "AIC")
