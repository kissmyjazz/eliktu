# summary tables of for the paper
library(here)
library(gtsummary)
library(gt)
library(labelled)
library(tidyverse)
library(huxtable)
library(flextable)


old <- theme_set(theme_bw())
theme_gtsummary_journal(journal = "lancet")
# some custom tweaks to Lancet theme
my_theme <- list("style_number-arg:decimal.mark" = ".",
                 "style_number-arg:big.mark" = ",",
                 "tbl_summary-str:categorical_stat" = "{n} ({p}%)",
                 "tbl_summary-arg:missing_text" = "Missing")
set_gtsummary_theme(my_theme)

# load files --------------------------------------------------------------
# original data only
path <- here("mod_data", "df_for_imputation.csv") 
df_blimp_orig <- read_csv(path) %>% dplyr::filter(!rowSums(is.na(.)) >= 64) %>% 
  mutate(kood = factor(kood)) %>% rowwise() %>% 
  dplyr::mutate(aImp = sum(c(AMIS3, (6-AMIS7), AMIS11, AMIS15, AMIS19, AMIS23,
                             AMIS2, (6-AMIS6), AMIS10, AMIS14, AMIS18, AMIS22)),
                mImp = sum(c(AMIS1, AMIS5, AMIS9, AMIS13, AMIS17, (6-AMIS21),
                             AMIS4, AMIS12, AMIS16, AMIS20, AMIS24))) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(AMIS1:AMIS24))


# actual age
path <- here("mod_data", "vanus.csv")
df_vanus <- read_csv(path) %>% 
  dplyr::select(kood, preciseage_15:preciseage_33) %>% 
  pivot_longer(!kood, names_to = c(".value", "age"),
             names_sep = "_",
             names_transform = list(age = as.integer),
             values_drop_na = FALSE) %>% 
  mutate(kood = factor(kood))
# mother's education
path <- here("mod_data", "ema_haridus.csv")
df_educ <- read_csv(path) %>% 
  dplyr::select(kood, m_educ = K24_uus) %>% 
  mutate(m_educ = factor(m_educ, levels = 1:3, labels = c("basic", 
                        "secondary", "tertiary")), kood = factor(kood)) 
df_full <- df_blimp_orig %>% left_join(df_vanus, by = c("kood", "age")) %>% 
  left_join(df_educ, by = "kood") %>% 
  mutate(kohort = factor(kohort, labels = c("Younger", "Older")), 
         age = factor(age, labels = c("15 years", "18 years", "25 years",
                                      "33 years"))) %>% 
  dplyr::select(-kood)

# variable labelling -------------------------------------------------------
# variable names in a single string for processing
names <- miceadds::VariableNames2String(names(df_full), breaks = 5000)
labels <- list(age = "Age, years", sugu = "Sex", kohort = "Cohort",
               Wmaxkg = "Maximum power output, per kg bwt",
               Calcium = "Calcium, mg", Iodine = "Iodine, µg",
               Iron = "Iron, mg", Magnesium = "Magnesium, mg",
               Manganese = "Manganese, mg", Phosphorus = "Phosphorus, mg",
               Potassium = "Potassium, mg", Selenium  = "Selenium, µg",
               Sodium = "Sodium, mg", Zink = "Zinc, mg",
               kcal = "Daily energy intake, kcal/day",
               BMI = html("BMI, kg/m<sup>2</sup>"),
               Carb = "Carbohydrates, g", Cerealprod = "Cereal products, g",
               CHL = "Cholesterol, mmol/l", Eggs = "Eggs, g",
               Fatsg = "Fats, g", Fish = "Fish, g", Folate = "Folate, µg",
               FruitsBerries = "Fruits & berries, g",
               HDL = "HDL-cholesterol, mmol/l",
               HOMA = "HOMA, units", LDL = "LDL-cholesterol, mmol/l",
               Lipid = "Lipids, g", Meat = "Meat, g", Milk = "Milk, g",
               Niacin = "Niacin, mg", Protein = "Proteins, g",
               SugSweets = "Sugar & sweets, g", Veget = "Vegetables, g",
               VitA = "Vitamin A, µg", VitB1 = "Vitamin B1, mg",
               VitB12 = "Vitamin B12, µg", VitB2 = "Vitamin B2, mg",
               VitB6 = "Vitamin B6, mg", VitC = "Vitamin C, mg",
               VitD = "Vitamin D, µg", VitE = "Vitamin E, mg",
               Alco = "Pure alcohol, g",
               aImp = "Adaptive impulsivity scale, units",
               mImp = "Maladaptive impulsivity scale, units",
               preciseage = "Age, years", m_educ = "Mother's education")
var_label(df_full) <- labels

# summary table for the manuscript ----------------------------------------
# print(theme_gtsummary_journal(journal = "lancet", set_theme = FALSE))
df_paper <- df_full %>% dplyr::select(sugu, preciseage, aImp, mImp, Wmaxkg,
                                      Alco, Veget, Cerealprod, Fish, Sodium, 
                                      Zink, Selenium, VitB6, age)
paper_summary_tbl <- df_paper %>% tbl_summary(by = "age") 

# saving table as flextable
# path_doc <- here("..", "Manuscript", "tables", "summary_table_main.docx")
# ft <- paper_summary_tbl %>% as_flex_table()
# ft <- font(ft, fontname = "Times New Roman", part = c("all"))
# ft <- fontsize(ft, size = 8)
# ft <- fontsize(ft, size = 10, part = c("header"))
# ft <- fontsize(ft, size = 10, part = c("footer"))
# ft <- line_spacing(ft, space = 1) %>% autofit() %>% fit_to_width(max_width = 11)
# save_as_docx(ft, path = path_doc)

# summary table for suppl materials ---------------------------------------
df_suppl <- df_full %>% dplyr::select(-c(preciseage))
paper_suppl_tbl <- df_suppl %>% tbl_summary(by = "age") 

# saving table as flextable
path_doc <- here("..", "Manuscript", "tables", "summary_table_suppl.docx")
ft <- paper_suppl_tbl %>% as_flex_table()
ft <- font(ft, fontname = "Times New Roman", part = c("all"))
ft <- fontsize(ft, size = 8)
ft <- fontsize(ft, size = 10, part = c("header"))
ft <- fontsize(ft, size = 10, part = c("footer"))
ft <- line_spacing(ft, space = 1) %>% autofit() %>% fit_to_width(max_width = 11)
save_as_docx(ft, path = path_doc)
