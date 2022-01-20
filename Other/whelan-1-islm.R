# Whelan - ISLM Model
setwd("~/Projects/rstudio/Econometrics-in-R")
rm("c")
#install.packages('')
library(ggplot2)
library(gridExtra)

# Goods Market Parameters
c0 = 10
i0 = 10
g0 = 10
t0 = 10
c1 = 0.5
i1 = 0.2
c2 = 1
i2 = 1
t0 = 10

# Money Market Parameters
m0 = 100
p0 = 10
l0 = 1
l1 = 2

# Steady State Solution
A = matrix(c(1-c1-i1, -l0/l1, c2+i2, 1), nrow = 2)
b = matrix(c(c0+i0+g0-c1*t0, -m0/(p0*l1)))
X = solve(A, b)
Y_eq = X[1, 1]
R_eq = X[2, 1]
print(R_eq)
print(Y_eq)

# Initiatization and Investment Shocks
V = rnorm(100, mean = 0, sd = 1)
U = rnorm(100, mean = 0, sd = 1)
Y = rep(Y_eq, 101)
R = rep(R_eq, 101)

# Path of Economy
ITER = seq(from=2, to = 100)
for (i in ITER) {
	Y[i] = c0 + i0 + g0 - c1*t0 + (c1+i1) * Y[i-1] - (c2+i2) * R[i-1] + V[i]
	R[i] = l0*Y[i]/l1 - m0/(p0*l1)
	if (i>50) {
		g0=g0+1
	}
	if (i>75) {
		p0=p0+10
	}	
}

# Plotting Path
df = data.frame(seq(1,101,1))
names(df) = 'T'
df$Y = Y
df$R = R
df = df[10:100,]
TS_Y = df %>% ggplot(aes(x=T, y=Y)) + geom_point() + geom_line()
TS_R = df %>% ggplot(aes(x=T, y=R)) + geom_point() + geom_line() 
grid.arrange(TS_Y, TS_R, ncol=1, nrow = 2)

# Comparative Statics
# 1. rise in g0 -> rise in Y but also rise in R! Why? Because govt spending raises the transaction demand for money, people sell bonds to get cash, so a higher interest rate arises in the market to offset this.
# 2. rise in m0 -> rise in Y, fall in R. Why? When CB buy bonds, increase the Comm Bank reserves, they are going to lend more. This raises the total money supply. With more money in hand, people are ready to buy bonds even for lower interest rates. Lower rates boost investment. 
# 3. rise in g0 and t0 (balanced budget) -> rise in Y, and rise in R at slower pace. Fiscal stimulus w/0 going into debt, is possible, because households do not spend as efficiently as govt can. 
# 3. rise in m0 and rise in p0 (bit later) -> Fall in R, Rise in Y; both reversed!! 
# 4. rise in g0 and rise in p0 (bit later) -> Fall in R, Rise in Y ; completely reversed!! 


