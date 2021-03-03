# Do oil prices effect GDP growth? If so, by how much? 
# Quarterly Data from FRED, Avg Spot Oil Price (WTI) and US Real GDP YOY Growth

rm(function)
library("readxl")
library(tidyverse)
library(dynlm)
library(tsibble)

# Load the Data
library("readxl")
setwd("~/Proj cts/rstudio/Econometrics-in-R")
wb = read_excel('data/US_gdp_WTI_oil.xls', skip = 11)
names(wb) = c('DATE', 'OILPRICE', 'RGDPGROWTH')

# Processing
df$DATE = as.Date(as.yearqtr(df$DATE, "%Y-%M-%D"))
df = df %>% mutate(OILPRICEGROWTH=(OILPRICE-lag(OILPRICE,4))/(lag(OILPRICE,4)+0.0001))
df = df %>% filter(DATE >= '1948-01-01' & DATE <= '2020-10-01')
df = tsibble(df, index = 'DATE')

# Plot 
GDP = df %>% ggplot(aes(x=DATE, y=RGDPGROWTH)) + geom_point() + geom_line()
OIL = df %>% ggplot(aes(x=DATE, y=OILPRICE)) + geom_point() + geom_line() 
grid.arrange(GDP, OIL, ncol=1, nrow = 2)

# Rolling Correlations
win = 30
df_xts = xts(df, order.by = df$DATE) 
cor1 <- rollapply(df_xts,win,function(x) cor(as.numeric(x[, 3]), as.numeric(x[, 4])), by.column=F)
cor2 <- rollapply(df_xts,win,function(x) cor(as.numeric(x[, 3]), as.numeric(x[, 2])), by.column=F)
plot(cor1)
plot(cor2)

df = df %>% mutate(PRE = ifelse(DATE<=1990, 1, 0))

df_pre = df %>% filter(PRE == 1)
cor(df_pre$RGDPGROWTH, df_pre$OILPRICEGROWTH)
model1 <- dynlm(df$RGDPGROWTH ~ df$OILPRICEGROWTH)
coeftest(model1, vcov. = vcovHAC)
