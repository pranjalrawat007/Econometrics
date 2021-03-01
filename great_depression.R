# you need to install car and data.table as well. 
install.packages('xts')

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

# Load the data
library("readxl")
# http://www.macrohistory.net/data/
wb = read_excel('data/JSTdatasetR4.xlsx', sheet = 'Data')
vars = c('year', 'country', 'gdp', 'cpi', 'pop', 'rconpc', 'iy', 'stir')
df = select(data.frame(wb), vars)
df = as_tsibble(df, key='country', index = 'year')

library(tidyverse)
library(tsibble)
df_clean = df %>% filter(year >= 1900 & year <= 1950 & country == 'USA') %>% mutate(lgdp=log(gdp), inf=(cpi-lag(cpi))/cpi, lpop = log(pop), lcon = log(rconpc)) 
df_clean %>% ggplot(aes(x=year, y=stir, group=country)) + geom_line() 


df_1920 = df %>% filter(year == 1920 & country != 'Germany' & country != 'Netherlands') %>% select(country, gdp, year)
df_gdp = df %>% select(country, year, gdp)
df2 = left_join(df, df_1920, by='country') %>% select(-year.y) 
df2
df2 = df2 %>% mutate(gdp.indexed=(gdp.x+0.0000001)*100/gdp.y)
df2
df2 %>% ggplot(aes(x=year.x, y=gdp.indexed, group=country)) + geom_line() 

df %>% filter(year > 1920 & year < 1945) %>% select(country, year, gdp)	%>% mutate(lgdp = log(gdp)) %>% ggplot(aes(x = year, y = lgdp, group = country)) + geom_point() 

a = df %>% filter(year == 1920) %>% select(country, gdp)

inner_join(data1, data2, by="SEQN")


?ggplot