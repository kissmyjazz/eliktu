library(psych)
library(lavaan)
library(lavaanPlot)
library(here)
library(tidyverse)
library(data.table)


# function to generate ESEM syntax for lavaan -----------------------------
# https://msilvestrin.me/post/esem/
make_esem_model <- function (loadings_dt, anchors){
  # make is_anchor variable
  loadings_dt[, is_anchor := 0]
  for (l in names(anchors)) loadings_dt[latent == l & item == anchors[l], is_anchor := 1]
  
  # make syntax column per item; syntax is different depending on is_anchor
  loadings_dt[is_anchor == 0, syntax := paste0("start(",value,")*", item)]
  loadings_dt[is_anchor == 1, syntax := paste0(value,"*", item)]
  
  #Make syntax for each latent variable
  each_syntax <- function (l){
    paste(l, "=~", paste0(loadings_dt[latent == l, syntax], collapse = "+"),"\n")
  }
  
  # Put all syntaxes together
  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = " ")
}


# load data ---------------------------------------------------------------
path <- here("mod_data", "df_for_imputation.csv")
df <- read_csv(path)
names <- colnames(df)
names_amis <- c(".imp", names[1:3], names[5], names[16:17], names[45:68])
path_amis_c1 <- here("imputed_data", "blimp", 'imps_c1f_AMIS.csv')
path_amis_c2 <- here("imputed_data", "blimp", 'imps_c2f_AMIS.csv')
df_amis_c1 <- read_csv(path_amis_c1, col_names = names_amis) %>%
  dplyr::select(-c(Wmaxkg, kcal, BMI))
df_amis_c2 <- read_csv(path_amis_c2, col_names = names_amis) %>%
  dplyr::select(-c(Wmaxkg, kcal, BMI))
df_amis <- bind_rows(list(df_amis_c1, df_amis_c2))
df_amis[df_amis == -999] <- NA

# remove suspicious observations
df_amis_clean <- df_amis %>% dplyr::filter(!(kood == 1824 & age == 18)) %>% 
  dplyr::filter(!(kood == 1157 & age == 33)) %>% 
  dplyr::filter(!(kood == 1307 & age == 18)) %>% 
  dplyr::filter(.imp != 0) 

# for factor analysis
df_amis2 <- df_amis_clean %>% dplyr::filter(.imp == 2) %>% 
  dplyr::select(AMIS1:AMIS24)

# derive starting loadings ------------------------------------------------
# keep AMIS8 item for now
# the following code is mostly borrowed from https://msilvestrin.me/post/esem/
esem_efa <- fa(df_amis2, nfactors = 2,rotate = "geominQ",
               fm = 'wls', delta = .5)
esem_efa$loadings

esem_loadings <- data.table(matrix(round(esem_efa$loadings, 2),
                                   nrow = 24, ncol = 2))
names(esem_loadings) <- c("F1","F2")
esem_loadings$item <- paste0("AMIS", c(1:24))
esem_loadings <- melt(esem_loadings, "item", variable.name = "latent")
esem_loadings

# anchors
# AMIS13 shows high loading on F1 and very low loading on F2
# AMIS15 shows high loading on F2 and very low loading on F1
anchors <- c(F1 = "AMIS13", F2 = "AMIS10")

# make model
esem_model <- make_esem_model(esem_loadings, anchors)
# print model
writeLines(esem_model)

# run lavaan --------------------------------------------------------------
esem_fit <- cfa(model = esem_model, data = df_amis2, ordered = names(df_amis2),
    estimator = "WLSMV", std.lv = TRUE)
summary(esem_fit, fit.measures = TRUE, standardized = TRUE)
parameterestimates(esem_fit, standardized=TRUE)
modificationindices(esem_fit, sort = TRUE)
fitm1 <- fitmeasures(esem_fit, fit.measures = c("cfi", "cfi.scaled", "rmsea", 
                                       "rmsea.ci.lower", "rmsea.ci.upper"))
fitm2 <- fitmeasures(esem_fit, fit.measures = c("cfi", "cfi.scaled", "rmsea", 
                                       "rmsea.ci.lower", "rmsea.ci.upper"))

lav_predictions <- lavPredict(esem_fit, newdata = df_amis2, type = "lv", 
                              method = "EBM")

# function to compute latent scores ---------------------------------------
latent_scores <- function(df, esem_model) {
  df <- df %>% dplyr::select(.imp, kood, age, AMIS1:AMIS24)
  n <- max(df$.imp)
 params <- vector(mode = "list", length = n)
 lvs <- vector(mode = "list", length = n)
 for (i in 1:n) {
   df_temp <- df %>% dplyr::filter(.imp == i)
   fit <- cfa(model = esem_model, data = df_temp, ordered = names(df_temp),
       estimator = "WLSMV", std.lv = TRUE)
   params[[i]] <- fitmeasures(fit)
   lvs[[i]] <- cbind(df_temp[, c("kood", "age")], 
                     lavPredict(fit, newdata = df_temp, type = "lv", 
                          method = "EBM"))
 }
 out <- list("params" = params, "scores" = lvs)
 out
}

cfa_results <- latent_scores(df_amis_clean, esem_model = esem_model)
