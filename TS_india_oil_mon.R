# Load Packages
library(readxl)
library(tidyverse)
library(dynlm)
library(tsibble)
library(gridExtra)
library(dplyr)
library(xts)
library(AER)
library(quantmod)
library(dynlm)
library(orcutt)
library(nlme)
library(stargazer)

# Load the Data
setwd("~/Projects/rstudio/Econometrics-in-R")
wb = read_excel('data/indiaoilmon.xls', skip = 11)
names(wb) = c('DATE', 'RGDPGROWTH', 'OILPRICE')

# Processing
df = data.frame(wb)
df$DATE = as.Date(as.yearqtr(df$DATE, "%Y-%M-%D"))
df = df %>% mutate(OILPRICEGROWTH=(OILPRICE-lag(OILPRICE,4))/(lag(OILPRICE,4)+0.0001))
df = df %>% filter(DATE >= '1995-01-01' & DATE <= '2018-12-01')
df = tsibble(df, index = 'DATE')

# Plot 
GDP = df %>% ggplot(aes(x=DATE, y=RGDPGROWTH)) + geom_point() + geom_line()
OIL = df %>% ggplot(aes(x=DATE, y=OILPRICE)) + geom_point() + geom_line() 
grid.arrange(GDP, OIL, ncol=1, nrow = 2)


# Rolling Correlations
win = 200
df_xts = xts(df, order.by = df$DATE) 
cor1 <- rollapply(df_xts,win,function(x) cor(as.numeric(x[, 2]), as.numeric(x[, 3])), by.column=F)
cor2 <- rollapply(df_xts,win,function(x) cor(as.numeric(x[, 2]), as.numeric(x[, 4])), by.column=F)
plot(cor1)
plot(cor2)

mean <- rollapply(df_xts,win,function(x) mean(as.numeric(x[, 2]), as.numeric(x[, 3])), by.column=F)

# Pre-Processing
df_pre = df %>% mutate(PRE = ifelse(DATE<='2017-01-01', 1, 0)) %>% filter(PRE == 0)
RGDPGROWTH = zoo(df$RGDPGROWTH)
OILPRICE = zoo(df$OILPRICE)

# Model 1
lags = 24
model1 <- dynlm(RGDPGROWTH ~ L(OILPRICE, 1:lags))
coeftest(model1, vcov. = vcovHAC)
linearHypothesis(model1, c("L(OILPRICE, 0:lags)0=0", "L(OILPRICE, 0:lags)1=0", "L(OILPRICE, 0:lags)2=0", "L(OILPRICE, 0:lags)3=0", "L(OILPRICE, 0:lags)4=0", "L(OILPRICE, 0:lags)5=0"), vcov. = vcovHAC)


SE <- list(sqrt(diag(NeweyWest(model1, lag = 7, prewhite = F)))) 
point_estimates <- model1$coefficients
CI_bounds <- cbind("lower" = point_estimates - 1.96 * SE[[1]],
                   "upper" = point_estimates + 1.96 * SE[[1]])[-1, ]

# IRF for Dynamic Multiplier

plot(1:lags, point_estimates[-1], type = "l", lwd = 2, ylim = c(-1, 1),xlab = "Lag",ylab = "Dynamic multiplier",main = "Dynamic Effect of Oil Price on RGDP Growth")
abline(h = 0, lty = 2)
lines(1:lags, CI_bounds[,1], col = "darkred")
lines(1:lags, CI_bounds[,2], col = "darkred")

point_estimates[-1]
# Cumnulative DM
model2 <- dynlm(RGDPGROWTH ~ L(d(OILPRICE), 1:lags))
point_estimates <- model2$coefficients
SE <- list(sqrt(diag(NeweyWest(model2, lag = 7, prewhite = F)))) 

CI_bounds <- cbind("lower" = point_estimates - 1.96 * SE[[1]],
                   "upper" = point_estimates + 1.96 * SE[[1]])[-1,]

plot(1:lags, point_estimates[-1], type = "l", lwd = 2, ylim = c(-1, 1),xlab = "Lag",ylab = "Cum. Dynamic multiplier",main = "Cum. Dynamic Effect of Oil Price on RGDP Growth")
abline(h = 0, lty = 2)
lines(1:lags, CI_bounds[,1], col = "darkred")
lines(1:lags, CI_bounds[,2], col = "darkred")
