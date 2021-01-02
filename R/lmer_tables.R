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
library(flextable)

source("R/utils/tidy_melded.R")

options(scipen=999)
theme_set(theme_apa(base_size = 14)) 

# load objects ------------------------------------------------------------
# fully adjusted lmer models
path_mImp <- here("mira_objects", "mitml_m_c_both_slope_dev.rds")
path_aImp <- here("mira_objects", "mitml_a_c_both_slope_dev.rds")
mitml_mImp <- read_rds(path_mImp)
mitml_aImp <- read_rds(path_aImp)
mira_mImp <- as.mira(mitml_mImp)
mira_aImp <- as.mira(mitml_aImp)

# univariate lmer models adjusted for sex and age
# mImp
path_mImp_Sex <- here("mira_objects", "mitml_lmer_univ_mImp_sex.rds")
path_mImp_Wmaxkg <- here("mira_objects", "mitml_lmer_univ_mImp_Wmaxkg.rds")
path_mImp_Selenium <- here("mira_objects", "mitml_lmer_univ_mImp_Selenium.rds")
path_mImp_Zink  <- here("mira_objects", "mitml_lmer_univ_mImp_Zink.rds")
path_mImp_Fish <- here("mira_objects", "mitml_lmer_univ_mImp_Fish.rds")
path_mImp_Veget <- here("mira_objects", "mitml_lmer_univ_mImp_Veget.rds")
path_mImp_Alco <- here("mira_objects", "mitml_lmer_univ_mImp_Alco.rds")
path_mImp_aImp <- here("mira_objects", "mitml_lmer_univ_mImp_aImp.rds")
path_mImp_Sodium <- here("mira_objects", "mitml_lmer_univ_mImp_Sodium.rds")
# aImp
path_aImp_Sex <- here("mira_objects", "mitml_lmer_univ_aImp_Sex.rds")
path_aImp_Zink <- here("mira_objects", "mitml_lmer_univ_aImp_Zink.rds")
path_aImp_Cerealprod <- here("mira_objects", "mitml_lmer_univ_aImp_Cerealprod.rds")
path_aImp_VitB6 <- here("mira_objects", "mitml_lmer_univ_aImp_VitB6.rds")
path_aImp_mImp <- here("mira_objects", "mitml_lmer_univ_aImp_mImp.rds")
# read files
mitml_mImp_Sex <- read_rds(path_mImp_Sex)
mitml_mImp_Wmaxkg <- read_rds(path_mImp_Wmaxkg)
mitml_mImp_Selenium <- read_rds(path_mImp_Selenium)
mitml_mImp_Zink  <- read_rds(path_mImp_Zink)
mitml_mImp_Fish <- read_rds(path_mImp_Fish)
mitml_mImp_Veget <- read_rds(path_mImp_Veget)
mitml_mImp_Alco <- read_rds(path_mImp_Alco)
mitml_mImp_aImp <- read_rds(path_mImp_aImp)
mitml_mImp_Sodium <- read_rds(path_mImp_Sodium)

mitml_aImp_Sex <- read_rds(path_aImp_Sex)
mitml_aImp_Zink <- read_rds(path_aImp_Zink)
mitml_aImp_Cerealprod <- read_rds(path_aImp_Cerealprod)
mitml_aImp_VitB6 <- read_rds(path_aImp_VitB6)
mitml_aImp_mImp <- read_rds(path_aImp_mImp)

# convert to `mira` objects
mira_mImp_Sex <- as.mira(mitml_mImp_Sex)
mira_mImp_Wmaxkg <- as.mira(mitml_mImp_Wmaxkg)
mira_mImp_Selenium <- as.mira(mitml_mImp_Selenium)
mira_mImp_Zink  <- as.mira(mitml_mImp_Zink)
mira_mImp_Fish <- as.mira(mitml_mImp_Fish)
mira_mImp_Veget <- as.mira(mitml_mImp_Veget)
mira_mImp_Alco <- as.mira(mitml_mImp_Alco)
mira_mImp_aImp <- as.mira(mitml_mImp_aImp)
mira_mImp_Sodium <- as.mira(mitml_mImp_Sodium)

mira_aImp_Sex <- as.mira(mitml_aImp_Sex)
mira_aImp_Zink <- as.mira(mitml_aImp_Zink)
mira_aImp_Cerealprod <- as.mira(mitml_aImp_Cerealprod)
mira_aImp_VitB6 <- as.mira(mitml_aImp_VitB6)
mira_aImp_mImp <- as.mira(mitml_aImp_mImp)

# fully adjusted lmer models with latent factor scores
path_mImp_lscores <- here("mira_objects", "mitml_m_c_both_slope_dev_lscores.rds")
path_aImp_lscores <- here("mira_objects", "mitml_a_c_both_slope_dev_lscores.rds")
mitml_mImp_lscores <- read_rds(path_mImp_lscores)
mitml_aImp_lscores <- read_rds(path_aImp_lscores)
mira_mImp_lscores <- as.mira(mitml_mImp_lscores)
mira_aImp_lscores <- as.mira(mitml_aImp_lscores)

# make custom classed objects `melded` to use with custom tidy functions----
# fully adjusted lmer models
mira_mImp_melded <- mira_mImp
class(mira_mImp_melded) <- "melded"
mira_aImp_melded <- mira_aImp
class(mira_aImp_melded) <- "melded"

# univariate lmer models adjusted for sex and age
mira_mImp_Sex_melded <- mitml_mImp_Sex
class(mira_mImp_Sex_melded) <- "melded"
mira_mImp_Wmaxkg_melded <- mitml_mImp_Wmaxkg
class(mira_mImp_Wmaxkg_melded) <- "melded"
mira_mImp_Selenium_melded <- mitml_mImp_Selenium
class(mira_mImp_Selenium_melded) <- "melded"
mira_mImp_Zink_melded <- mitml_mImp_Zink
class(mira_mImp_Zink_melded) <- "melded"
mira_mImp_Fish_melded <- mitml_mImp_Fish
class(mira_mImp_Fish_melded) <- "melded"
mira_mImp_Veget_melded <- mitml_mImp_Veget
class(mira_mImp_Veget_melded) <- "melded"
mira_mImp_Alco_melded <- mitml_mImp_Alco
class(mira_mImp_Alco_melded) <- "melded"
mira_mImp_aImp_melded <- mitml_mImp_aImp
class(mira_mImp_aImp_melded) <- "melded"
mira_mImp_Sodium_melded <- mitml_mImp_Sodium
class(mira_mImp_Sodium_melded) <- "melded"

mira_aImp_Sex_melded <- mitml_aImp_Sex
class(mira_aImp_Sex_melded) <- "melded"
mira_aImp_Zink_melded <- mitml_aImp_Zink
class(mira_aImp_Zink_melded) <- "melded"
mira_aImp_Cerealprod_melded <- mitml_aImp_Cerealprod
class(mira_aImp_Cerealprod_melded) <- "melded"
mira_aImp_VitB6_melded <- mitml_aImp_VitB6
class(mira_aImp_VitB6_melded) <- "melded"
mira_aImp_mImp_melded <- mitml_aImp_mImp
class(mira_aImp_mImp_melded) <- "melded"

# fully adjusted lmer models with latent factor scores
mira_mImp_lscores_melded <- mitml_mImp_lscores
class(mira_mImp_lscores_melded) <- "melded"
mira_aImp_lscores_melded <- mitml_aImp_lscores
class(mira_aImp_lscores_melded) <- "melded"

# make tables -------------------------------------------------------------
# adj_mImp_model <- parameters(mira_mImp) %>% 
#   select(Parameter, Coefficient, CI_low, CI_high, p) %>%
#   parameters_table(stars = TRUE)
# mImp
mImp_coefs = c("Age centered at 18 years" = "age_cent",
  "Sex: (female - male)" = "sex_dev1",
  "Adaptive impulsivity score" = "aImp",
  "Adaptive impulsivity score" = "aImp_lscore",  
  "Maximum power output" = "Wmaxkg",
  "Zinc" = "Zink",
  "Selenium" = "Selenium",
  "Sodium" = "Sodium",
  "Fish" = "Fish",
  "Vegetables" = "Veget",
  "Alcohol" = "Alco",
  "Sodium \U2715 Sex: (female - male)" = "sex_dev1:Sodium"
  )

# # `gtsummary` does not work, use `huxtable` 
# adj_mImp_model_tbl <- huxreg("Unweighted composite scores:\nbest fitting model"=
#   mira_mImp_melded,
#   "Unweighted composite scores:\nmodel adjusted for sex and age" =
#   mira_mImp_Sex_melded,
#   "Latent factor scores:\nadjusted model" =
#   mira_mImp_lscores_melded, statistics = c(N = "nobs"),
#   error_format = "({conf.low}; {conf.high})", omit_coefs = "(Intercept)",
#   note = "{stars}\nRegression coefficients are standardised\n95% CI are given in parentheses",
#   coefs = mImp_coefs, borders = 0.4)
# adj_mImp_model_tbl
# ft_mImp <- adj_mImp_model_tbl %>% huxtable::as_flextable()
# # saving table as flextable
# path_mImp <- here("..", "Manuscript", "tables", "mImp_regress_tbl.docx")
# ft_mImp <- font(ft_mImp, fontname = "Times New Roman", part = c("all"))
# ft_mImp <- fontsize(ft_mImp, size = 12)
# ft_mImp <- fontsize(ft_mImp, size = 12, part = c("header"))
# ft_mImp <- fontsize(ft_mImp, size = 12, part = c("footer"))
# ft_mImp <- line_spacing(ft_mImp, space = 1) %>% autofit() %>% 
#   fit_to_width(max_width = 7.5)
# save_as_docx(ft_mImp, path = path_mImp)

# aImp
aImp_coefs = c("Age centered at 18 years" = "age_cent",
               "Sex: (female - male)" = "sex_dev1",
               "Maladaptive impulsivity score" = "mImp",
               "Maladaptive impulsivity score" = "mImp_lscore",  
               "Zinc" = "Zink",
               "Vitamin B6" = "VitB6",
               "Cereal products" = "Cerealprod",
               "Age \U2715 Sex: (female - male)" = "age_cent:sex_dev1"
)

adj_aImp_model_tbl <- huxreg("Unweighted composite scores:\nbest fitting model"=
  mira_aImp_melded,
  "Unweighted composite scores:\nmodel adjusted for sex and age" =
  mira_aImp_Sex_melded,
  "Latent factor scores:\nadjusted model" =
  mira_aImp_lscores_melded, statistics = c(N = "nobs"),
  error_format = "({conf.low}; {conf.high})", omit_coefs = "(Intercept)",
  note = "{stars}\nRegression coefficients are standardised\n95% CI are given in parentheses",
  coefs = aImp_coefs, borders = 0.4)
adj_aImp_model_tbl
ft_aImp <- adj_aImp_model_tbl %>% huxtable::as_flextable()

# saving table as flextable
path_aImp <- here("..", "Manuscript", "tables", "aImp_regress_tbl.docx")
ft_aImp <- font(ft_aImp, fontname = "Times New Roman", part = c("all"))
ft_aImp <- fontsize(ft_aImp, size = 12)
ft_aImp <- fontsize(ft_aImp, size = 12, part = c("header"))
ft_aImp <- fontsize(ft_aImp, size = 12, part = c("footer"))
ft_aImp <- line_spacing(ft_aImp, space = 1) %>% autofit() %>%
  fit_to_width(max_width = 7.5)
save_as_docx(ft_aImp, path = path_aImp)

# temp table to find out missing coefs ------------------------------------
temp_tbl <- huxreg(mira_aImp_Cerealprod_melded, statistics = character(0),
                   error_format = "({conf.low}; {conf.high})")
temp_tbl