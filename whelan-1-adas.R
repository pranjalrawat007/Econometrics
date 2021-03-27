# Whelan - AD-AS Model
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
c1 = 0.7
i1 = 0.1
c2 = 1
i2 = 1
t0 = 10

# Money Market Parameters
m0 = 500
l0 = 1
l1 = 20

# Price Change Parameter
k0 = 20
k1 = 0.9
k2 = 0.5

# Initiatization and Shocks
T = 500
V = rnorm(T, mean = 0, sd = 1)
U = rnorm(T, mean = 0, sd = 1)
E = rnorm(T, mean = 0, sd = 1)
Y = rep(2,T+1)
R = rep(2,T+1)
P = rep(10,T+1)

# Path of Economy
ITER = seq(from=3, to = T+1)
for (i in ITER) {
	Y[i] = c0 + i0 + g0 - c1*t0 + (c1+i1) * Y[i-1] - (c2+i2) * R[i-1] + V[i]
	R[i] = 0.9 * R[i-1] + l0*Y[i]/l1 - m0/(100*l1) + U[i]/l1
	P[i] = k0 + k1 * P[i-1] + k2*(Y[i-1] - Y[i-2]) + E[i]
	if (i>T/2) {
		m0 = m0 + 1
	}
	if (i>3*T/4) {
	}	
}

# Plotting Path
df = data.frame(seq(1,T+1,1))
names(df) = 'T'
df$Y = Y
df$R = R
df$P = P
df = df[25:T+1,]
TS_Y = df %>% ggplot(aes(x=T, y=Y)) + geom_point() + geom_line()
TS_R = df %>% ggplot(aes(x=T, y=R)) + geom_point() + geom_line() 
TS_P = df %>% ggplot(aes(x=T, y=P)) + geom_point() + geom_line() 
grid.arrange(TS_Y, TS_R, TS_P, ncol=1, nrow = 3)

