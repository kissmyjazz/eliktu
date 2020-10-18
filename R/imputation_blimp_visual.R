library(tidyverse)
library(here)
library(miceadds)
library(mitml)
path <- here("mod_data", "df_for_imputation.csv")

df <- read_csv(path)
names <- colnames(df) 
names_food <- c(".imp", names[1:44])
names_amis <- c(".imp", names[1:5], names[16:17], names[45:68])

path_food <- here("imputed_data", "blimp", 'imps_food.csv')
path_amis <- here("imputed_data", "blimp", 'imps_amis.csv') 

df_food <- read_csv(path_food, col_names = names_food)
df_food[df_food == -999] <- NA

df_amis <- read_csv(path_amis, col_names = names_amis) %>% 
  dplyr::select(-c(sugu, kohort, Wmaxkg, kcal, BMI))
df_amis[df_amis == -999] <- NA

df_blimp <- df_food %>% dplyr::left_join(df_amis, by = c(".imp", "kood", "age"))
df_blimp <- df_blimp %>% group_by(.imp) %>% mutate(.id = row_number()) 
mids <- df_blimp %>% dplyr::filter(kohort == 1) %>% 
  dplyr::select(-kohort) %>% as.mids()
################################################################################
## ----fddplotimp, echo = FALSE-------------------------------------------------
# code from FIMD books by S. van Buuren
# example of kcal from first 5 imputations 
df_1 <- df_blimp %>% dplyr::filter(.imp <= 5, kohort == 1) %>% 
  dplyr::select(-kohort)
iv <- is.na(df_1[df_1$.imp==0,]$kcal)
ivn <- ifelse(iv,1,0)
col12  <- c("grey80","grey80",
            mdc(2),mdc(1),
            mdc(2),"transparent",
            mdc(2),"transparent",
            mdc(2),"transparent",
            mdc(2),"transparent")

grp <- 2*as.integer(df_1$.imp) - ivn
df_1ss <- data.frame(df_1, grp=grp) %>% dplyr::filter(kood %in% c(1088:1094, 1100:1108))
trellis.par.set(strip.background=list(col="grey95"))
trellis.par.set(list(layout.heights = list(strip = 1)))
tp1 <- xyplot(kcal~factor(age)|factor(kood), data=df_1ss, type="l",
              layout = c(4,4),
              groups=factor(.imp), col="grey80",
              ylab="kcal", 
              main = "Examples of Blimp imputed data for kcal (in magenta)",
              pch=19, cex=1,
              xlab="Age", xlim=c("15","18","25"),
              as.table=TRUE)
print(tp1)
tp2 <- xyplot(kcal~factor(age)|factor(kood), data=df_1ss, type="p",
              layout = c(4,4),
              groups=grp, col=col12,
              ylab="kcal",
              main = "Examples of Blimp imputed data for kcal (in magenta)",
              pch=19,
              cex=0.8,
              xlab="Age", xlim=c("15","18","25"),
              as.table=TRUE)
print(tp2, newpage=FALSE)
################################################################################
# Density plots for first 10 imputations
densityplot(mids, ~ Zink + kcal + BMI + Carb, subset = .imp <= 10, 
            main = "Density curves of imputed data (in magenta)")
################################################################################
# Density plots for first 10 imputations of AMIS questionnaire items
densityplot(mids, ~ AMIS1 + AMIS2 + AMIS3 + AMIS4, subset = .imp <= 10, 
            main = "Density curves of imputed AMIS data (in magenta)", bw = "SJ", 
            adjust = 5)
################################################################################
# some examples of response frequencies on AMIS questionnaire items
df_1 %>% filter(.imp <= 5) %>% select(.imp, AMIS1) %>% group_by(.imp) %>% 
  nest() %>% mutate(prop = data %>% map(~{table(.x) %>% prop.table() %>% 
      as.data.frame()})) %>% unnest(prop) %>% 
  select(.imp, response = .x, proportion = Freq) %>% spread(response, proportion) %>% 
  head(11) %>% round(2)

df_1 %>% filter(.imp <= 5) %>% select(.imp, AMIS8) %>% group_by(.imp) %>% 
  nest() %>% mutate(prop = data %>% map(~{table(.x) %>% prop.table() %>% 
      as.data.frame()})) %>% unnest(prop) %>% 
  select(.imp, response = .x, proportion = Freq) %>% spread(response, proportion) %>% 
  head(11) %>% round(2)