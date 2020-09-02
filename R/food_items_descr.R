library(here)
library(tidyverse)
library(psych)
library(rJava)
library(xlsx)
options(scipen=999)
path <- here("mod_data", "AMIS_toitumine_wide.csv")
data <- read_csv(path)
desc <- describe(data)[125:312, ]
desc <- cbind(indicator=row.names(desc), desc)
desc <- desc[order(desc$indicator), ]
write_csv(desc, here("summary_data", "toitumine_summary.csv"))
