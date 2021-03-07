# Do oil prices effect GDP growth? If so, by how much? 
# Quarterly Data from FRED, Avg Crude Oil Price and India Real GDP YOY Growth

# Load Packages
library("readxl")
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
wb = read_excel('data/indiaoil.xls', skip = 11)
names(wb) = c('DATE', 'RGDPGROWTH', 'OILPRICE')

# Processing
df = data.frame(wb)
df$DATE = as.Date(as.yearqtr(df$DATE, "%Y-%M-%D"))
df = df %>% mutate(OILPRICEGROWTH=(OILPRICE-lag(OILPRICE,4))/(lag(OILPRICE,4)+0.0001))
df = df %>% filter(DATE >= '1996-04-01' & DATE <= '2019-07-01')
df = tsibble(df, index = 'DATE')

# Plot 
GDP = df %>% ggplot(aes(x=DATE, y=RGDPGROWTH)) + geom_point() + geom_line()
OIL = df %>% ggplot(aes(x=DATE, y=OILPRICE)) + geom_point() + geom_line() 
grid.arrange(GDP, OIL, ncol=1, nrow = 2)


# Rolling Correlations
win = 30
df_xts = xts(df, order.by = df$DATE) 
cor1 <- rollapply(df_xts,win,function(x) cor(as.numeric(x[, 2]), as.numeric(x[, 3])), by.column=F)
cor2 <- rollapply(df_xts,win,function(x) cor(as.numeric(x[, 2]), as.numeric(x[, 4])), by.column=F)
plot(cor1)
plot(cor2)

# Pre-Processing
df_pre = df %>% mutate(PRE = ifelse(DATE<='2017-01-01', 1, 0)) %>% filter(PRE == 0)
RGDPGROWTH = zoo(df$RGDPGROWTH)
OILPRICE = zoo(df$OILPRICE)

# Model 1
lags = 4
model1 <- dynlm(RGDPGROWTH ~ L(RGDPGROWTH) + L(OILPRICE, 0:lags))
coeftest(model1, vcov. = vcovHAC)
linearHypothesis(model1, c("L(OILPRICE, 0:lags)0=0", "L(OILPRICE, 0:lags)1=0", "L(OILPRICE, 0:lags)2=0", "L(OILPRICE, 0:lags)3=0", "L(OILPRICE, 0:lags)4=0", "L(OILPRICE, 0:lags)5=0"), vcov. = vcovHAC,)


SE <- list(sqrt(diag(NeweyWest(model1, lag = 7, prewhite = F)))) 
point_estimates <- model1$coefficients
CI_bounds <- cbind("lower" = point_estimates - 1.96 * SE[[1]],
                   "upper" = point_estimates + 1.96 * SE[[1]])[-1, ]

# IRF for Dynamic Multiplier
plot(0:lags, point_estimates[-1], type = "l", lwd = 2, ylim = c(-0.4, 1),xlab = "Lag",ylab = "Dynamic multiplier",main = "Dynamic Effect of Oil Price on RGDP Growth")
abline(h = 0, lty = 2)
lines(0:lags, CI_bounds[,1], col = "darkred")
lines(0:lags, CI_bounds[,2], col = "darkred")


# Cumnulative DM
model2 <- dynlm(RGDPGROWTH ~ L(d(OILPRICE), 0:lags))
point_estimates <- model2$coefficients
SE <- list(sqrt(diag(NeweyWest(model2, lag = 7, prewhite = F)))) 

CI_bounds <- cbind("lower" = point_estimates - 1.96 * SE[[1]],
                   "upper" = point_estimates + 1.96 * SE[[1]])[-1,]

plot(0:lags, point_estimates[-1], type = "l", lwd = 2, ylim = c(-1, 1),xlab = "Lag",ylab = "Cum. Dynamic multiplier",main = "Cum. Dynamic Effect of Oil Price on RGDP Growth")
abline(h = 0, lty = 2)
lines(0:lags, CI_bounds[,1], col = "darkred")
lines(0:lags, CI_bounds[,2], col = "darkred")
