# Load Packages
library(readxl)
library(tidyverse)
library(dplyr)
library(data.table)

# Load the Data
setwd("~/Projects/rstudio/Econometrics-in-R")
wb = read_excel("data/indian_banks.xlsx", sheet = 'sbi', col_names = c("VAR", "MAR20", "MAR19", "MAR18", "MAR17", "MAR16"), skip = 1)
df = data.frame(wb)

