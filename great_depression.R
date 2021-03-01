# you need to install car and data.table as well. 
install.packages('lubridate')

library(car)
library(AER)
library(dynlm)
library(forecast)
library(readxl)
library(stargazer)
library(scales)
library(quantmod)
library(urca)
library(xts)

# Load Annual Data
library("readxl")
# http://www.macrohistory.net/data/
wb = read_excel('data/JSTdatasetR4.xlsx', sheet = 'Data')
vars = c('year', 'country', 'gdp', 'cpi', 'pop', 'rconpc', 'iy', 'stir', 'narrowm', 'money')
df = select(data.frame(wb), vars)
df = as_tsibble(df, key='country', index = 'year')

# Pre-Processing
library(tidyverse)
library(tsibble)
df_clean = df %>% filter(year >= 1900 & year <= 1950 & country == 'France') %>% mutate(lgdp=log(gdp), inf=(cpi-lag(cpi))/cpi, lpop = log(pop), lcon = log(rconpc)) 

# Plotting Annual data
plot_gdp = df_clean %>% ggplot(aes(x=year, y=lgdp, group=country)) + geom_line()
plot_m = df_clean %>% ggplot(aes(x=year, y=money, group=country)) + geom_line() 
plot_pop = df_clean %>% ggplot(aes(x=year, y=lpop, group=country)) + geom_line() 
plot_inf = df_clean %>% ggplot(aes(x=year, y=inf, group=country)) + geom_line() 
require(gridExtra)
grid.arrange(plot_gdp, plot_m, plot_pop, plot_inf, ncol=2, nrow = 2)

# Loading Quarterly Data
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

# Quarterly Plot 1

output = df_clean %>% ggplot(aes(x=date, y=output)) + geom_point() + geom_line() + geom_rect(aes(xmin='1929-01-01', xmax='1933-01-01', ymin=0, ymax=Inf))

grid.arrange(output, ncol=1, nrow = 1)

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





df = data.frame(wb)
?ggplot
