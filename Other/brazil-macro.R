# Macroeconometric Models - Application to Brazil
install.packages('Hmisc')
library(readxl)
library(tsibble)
library(mFilter)
library(ggplot2)
library(gridExtra)
library(vars)

# Set working directory
setwd("~/Projects/rstudio/Econometrics-in-R")
rm("c")

# Interest Rates, Government Securities, Treasury Bills for Brazil, Percent per Annum, Monthly, Not Seasonally Adjusted
# Brazil / U.S. Foreign Exchange Rate, Brazilian Reals to One U.S. Dollar, Monthly, Not Seasonally Adjusted
# Consumer Price Index: All Items for Brazil, Index 2015=100, Monthly, Not Seasonally Adjusted
# Gross Domestic Product by Expenditure in Constant Prices: Total Gross Domestic Product for Brazil, Index Q1 2020=100 of (Chained 2000 National Currency Units), Quarterly, Seasonally Adjusted
# Real Broad Effective Exchange Rate for Brazil, Index 2010=100, Monthly, Not Seasonally Adjusted
# Interest Rates, Discount Rate for Brazil, Percent per Annum, Monthly, Not Seasonally Adjusted
# Production of Total Industry in Brazil, Index 2015=100, Monthly, Seasonally Adjusted
# M1 for Brazil, Index Feb 2020=100 of (National Currency), Monthly, Seasonally Adjusted
# CBOE Brazil ETF Volatility Index, Index, Monthly, Not Seasonally Adjusted

# Monthly data
# http://www.macrohistory.net/data/
wb = read_excel('data/brazil-macro.xlsx', sheet = 'FRED Graph', skip = 18)
vars = c("date","tbill", "ex", "cpi", "reer","dis", "iip", "m1", "vol")
names(wb) = vars
df1 = data.frame(wb)

# Quarterly data
wb = read_excel('data/brazil-macro.xlsx', sheet = 'Sheet1', skip = 18)
vars = c("date","gdp")
names(wb) = vars
df2 = data.frame(wb)

# MODEL 1
# Investment Saving + Monetary Policy Rule
# y[t] = ay[t-1] - bi[t] + v[t]
# i[t] = ci[t-1] - dy[t] + u[t]ff
df = df1[23:313, c('date', 'dis', 'iip')]

# Detrending
iip = ts(df$iip, start=c(1996, 10), end = c(2020,12), frequency=12) 
df$iip_cycle = hpfilter(iip, freq=14400)$cycle
dis = ts(df$dis, start=c(1996, 10), end = c(2020,12), frequency=12) 
df$dis_cycle = hpfilter(dis, freq=14400)$cycle

# Plot Trend and Cycle
plot(hpfilter(iip, freq=14400))
plot(hpfilter(dis, freq=14400))

# Plot iip and dis Cycles
iip_cycle = df %>% ggplot(aes(x=date, y=iip_cycle)) + geom_point() + geom_line()
dis_cycle = df %>% ggplot(aes(x=date, y=dis_cycle)) + geom_point() + geom_line() 
grid.arrange(iip_cycle, dis_cycle, ncol=1, nrow = 2)

# VAR Estimation
data = cbind(df$iip_cycle, df$dis_cycle)
var.est1 <- VAR(data, p = 1, type = "none", season = NULL)
summary(var.est1)


# Estimate VAR Linear Models
require(graphics)
library(Hmisc)
df$iip_cycle1 = Lag(df$iip_cycle, 1)
df$dis_cycle1 = Lag(df$dis_cycle, 1)
eq1 = lm(df$iip_cycle ~ df$iip_cycle1 + df$dis_cycle1 - 1)
eq2 = lm(df$dis_cycle ~ df$iip_cycle1 + df$dis_cycle1 - 1)


coefficients(eq1)[1]

# Obtain Residuals
eq1_u = residuals(eq1)
eq2_u = residuals(eq2)

# VAR Error Covariance 
M <- cbind(eq1_u, eq2_u)
cov(M)
s11 = cov(M)[1,1]
s22 = cov(M)[2,2]
s21 = cov(M)[2,1]

# SVAR Estimates Recovery
s2 = s22
b12 = s21/s22
s1 = s22 - b12 * s2


# SVAR recovery 
# https://kevinkotze.github.io/ts-8-svar/ 

SVAR(x, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = NULL)
