rm(list=ls()) #Removes all items in Environment!
library(systemfit)
library(broom) #for `glance(`) and `tidy()`
library(PoEdata) #for PoE4 dataset
library(knitr) #for kable()

 install.packages("devtools")  # if not already installed
 library(devtools)
 install_git("https://github.com/ccolonescu/PoEdata")

 
data("truffles", package="PoEdata")
D <- q~p+ps+di
S <- q~p+pf
sys <- list(D,S)
instr <- ~ps+di+pf
truff.sys <- systemfit(sys, inst=instr, 
                       method="OLS", data=truffles)
summary(truff.sys)
