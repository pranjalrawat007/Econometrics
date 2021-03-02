# Set working directory
setwd("~/Projects/rstudio/Econometrics-in-R")
rm("c")

# ANNUAL DATA
library("readxl")
# http://www.macrohistory.net/data/
wb = read_excel('data/JSTdatasetR4.xlsx', sheet = 'Data')
names( wb)
vars = c("year","country", "rgdpmad", "cpi", "pop", "rconpc","hpnom", "iy", "stir", "narrowm", "money")

# Pre-Processing
library(tidyverse)
library(dynlm)
library(tsibble)
δ=0.0000001
df = select(data.frame(wb), vars)
df = as_tsibble(df, key='country', index = 'year')
df = df %>% mutate(gdp = rgdpmad * pop)
df = df %>% filter(year >= 1920 & year <= 1940) #, country %in% vars
df = df %>% mutate(gdp = log(gdp+ δ))
df = df %>% group_by(country) %>% mutate(inflation=(cpi-lag(cpi))/cpi)
df_clean = df %>% group_by(country) %>% mutate_at(c("gdp", "money", "rconpc", "inflation"), ~(scale(.) %>% as.vector))


# Plotting
library(gridExtra)
output_norm = df_clean %>% ggplot(aes(x=year, y=gdp, group=country)) + geom_point() + geom_line()
inflation = df_clean %>% ggplot(aes(x=year, y=inflation, group=country)) + geom_point() + geom_line() 
M2_norm = df_clean %>% ggplot(aes(x=year, y=money, group=country)) + geom_point() + geom_line() 
saving_rate = df_clean %>% filter(country != 'France') %>% ggplot(aes(x=year, y=iy, group=country)) + geom_point() + geom_line() 
grid.arrange(output_norm, inflation, M2_norm, saving_rate, ncol=2, nrow = 2)

# QUARTERLY
library("readxl")
# https://www.nber.org/research/data/tables-american-business-cycle
wb = read_excel('data/gd_qua.xlsx', sheet = 'gd_qua')
vars = c('year', 'quarter', 'GNP', 'RGNP72', 'GNPDEF', 'CSTOCK', 'CORPYIELD', 'CPRATE', 'M1', 'M2', 'BASE', 'CSTOCK', 'WPRICE67', 'PRODUR72', 'NONRES72', 'CDUR72', 'IRES72', 'CDUR72', 'CNDUR72') 

# Pre-Processing
df = select(data.frame(wb), vars) %>% filter(year >= 1919 & year <= 1941)
df$date = as.Date(as.yearqtr(paste(df$year, df$quarter), "%Y %q"))
df = as_tsibble(df, index = 'date')
df = df %>% select(-year, -quarter)
df_clean = df %>% mutate(output=log(RGNP72), inflation=(WPRICE67-lag(WPRICE67))/WPRICE67, msupply=log(M2), stock_idx = CSTOCK, int_rate = CPRATE, producer_eqpmnt = log(PRODUR72), building_nonresdnt = NONRES72, cons_nondur = CNDUR72, cons_durable = CDUR72, building_investmnt = IRES72)

# Quarterly Plots
output = df_clean %>% ggplot(aes(x=date, y=output)) + geom_point() + geom_line()
inflation = df_clean %>% ggplot(aes(x=date, y=inflation)) + geom_point() + geom_line() 
msupply = df_clean %>% ggplot(aes(x=date, y=msupply)) + geom_point() + geom_line() 
stock_idx = df_clean %>% ggplot(aes(x=date, y=stock_idx)) + geom_point() + geom_line() 
int_rate = df_clean %>% ggplot(aes(x=date, y=int_rate)) + geom_point() + geom_line() 
producer_eqpmnt = df_clean %>% ggplot(aes(x=date, y=producer_eqpmnt)) + geom_point() + geom_line()
building_nonresdnt = df_clean %>% ggplot(aes(x=date, y=building_nonresdnt)) + geom_point() + geom_line() 
cons_nondur = df_clean %>% ggplot(aes(x=date, y=cons_nondur)) + geom_point() + geom_line() 
cons_durable = df_clean %>% ggplot(aes(x=date, y=cons_durable)) + geom_point() + geom_line() 
building_investmnt = df_clean %>% ggplot(aes(x=date, y=building_investmnt)) + geom_point() + geom_line() 

grid.arrange(output, inflation, msupply, stock_idx, int_rate, producer_eqpmnt, building_nonresdnt, cons_nondur, cons_durable, ncol=3, nrow = 3)


# MONTHLY 
library("readxl")
# https://www.nber.org/research/data/tables-american-business-cycle
wb = read_excel('data/gd_mon.xlsx', sheet = 'gd_mon')
vars = c("YEAR", "MONTH", "PRDCTN29","SHPMNT29","INVTRY29","PRDCTN","SHPMNT","INVTRY") 

# Pre-Processing
df = select(data.frame(wb), vars) %>% filter(YEAR >= 1919 & YEAR <= 1941)
df$date = as.Date(as.yearmon(paste(df$YEAR, df$MONTH), "%Y %m"))
df = df %>% select(-YEAR, -MONTH) %>% filter(INVTRY != 11119)
df = df %>% group_by(date) %>% filter(row_number() == 1)
df = as_tsibble(df, index = 'date')

# Monthly Plots
PRDCTN29 = df %>% ggplot(aes(x=date, y=PRDCTN29)) + geom_point() + geom_line()
SHPMNT29 = df %>% ggplot(aes(x=date, y=SHPMNT29)) + geom_point() + geom_line() 
INVTRY29 = df %>% ggplot(aes(x=date, y=INVTRY29)) + geom_point() + geom_line() 
PRDCTN = df %>% ggplot(aes(x=date, y=PRDCTN)) + geom_point() + geom_line() 
SHPMNT = df %>% ggplot(aes(x=date, y=SHPMNT)) + geom_point() + geom_line() 
INVTRY = df %>% ggplot(aes(x=date, y=INVTRY)) + geom_point() + geom_line()
grid.arrange(PRDCTN29, SHPMNT29, INVTRY29, PRDCTN, SHPMNT, INVTRY, ncol=3, nrow = 2)


