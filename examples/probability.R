# Basic Probability - RandInt, Binomial, Normal, Custom, Chi, t, F

install.packages("stats")
library("stats")
set.seed(42) # set pseudorandom seed for replication

# RANDOM INTEGERS
sample(1:100, 5, replace = TRUE)

# BINOMIAL
n = 10 # number of draws
p = 0.3 # prob of heads

# Prob Mass Function
pdf = function(x) dbinom(x, n, p)
f(2) #P(x=2)
f(2:5) #P(2),P(3),P(4),P(5)
sum(f(2:5)) #P(2<x<5)

# Cum Mass Function
c = function(x) pbinom(x, n, p)
c(2) #P(x<2)
c(2:4) #P(x<2), P(x<3), P(x<4)

# Plotting PMF, CDF
pdf_plot = function(Dx) {
	plot(f(Dx), xlab = 'Number of Heads (x)', ylab = 'P(X=x)', main = 'PMF')
}

cdf_plot = function(Dx) {
	plot(c(Dx), xlab = 'Number of Heads (x)', ylab = 'P(X<x)', main = 'CMF')
}

Dx = seq(0, 10, 1) # domain of Prob Mass Function
pdf_plot(Dx)
cdf_plot(Dx)

# Sampling
r = function(x) rbinom(x, n, p)
x = r(100)
hist_sample = function(x) {
	hist(x, xlab = 'Sampled Values', main = 'Histogram of samples from BIN(10, 0.3)')
}
hist_sample(x)

# Summary Stats
info = function(x) {
	print(mean(x))
	print(var(x))
	print(summary(x))
	hist(x, xlab = 'Sampled Values', main = 'Histogram of samples')
	}
info(x)

# NORMAL
μ = 10 # number of draws
σ = 2 # prob of heads

# Prob Density Function (Only for plotting, does not have a interpretation)
f = function(x) dnorm(x, μ, σ)
f(10) #P(x=2)

# Cum Mass Function
c = function(x) pnorm(x, μ, σ)
c(10) #P(x<2)
c(10:14) #P(x<2), P(x<3), P(x<4)

# Plotting PMF, CDF
Dx = seq(0, 20, 1) # domain of Prob Mass Function
pdf_plot(Dx)
cdf_plot(Dx)

# Sampling
r = function(x) rnorm(x, μ, σ)
x = r(100)
hist_sample(x)

# Summary Stats
info(x)

# CHI-SQUARE
# Sum of Squares of m Standard Normals is distributed by Chi-Square
# Parameter is m (degrees of freedom)
# Expectation - m, variance - 2m. 
# Resembles Normal if m exceeds 30. 
# In Econometrics if residuals are assumed to be distributed normally, the residual sum of squares is assumed to be a Chi-Square variable

m = 10 # number of standard normals whose squares are summed

# Prob Density Function (Only for plotting, does not have a interpretation)
f = function(x) dchisq(x, m)
f(3) #P(x=2)

# Cum Mass Function
c = function(x) pchisq(x, m)
c(10) #P(x<2)
c(10:14) #P(x<2), P(x<3), P(x<4)

# Plotting PMF, CDF
Dx = seq(0, 60, 1) # domain of Prob Mass Function
pdf_plot(Dx)
cdf_plot(Dx)

# Sampling
r = function(x) rchisq(x, m)
x = r(50)
hist_sample(x)

# Summary Stats
info(x)

# Student T
# if Z is a standard normal, and C is a Chi-Square of M degrees of freedom
# t = Z/SQRT(C/M)
# Parameters: M>1, Mean = 0, Variance = M/(M-2)
# Tends to resemble Normal if M is beyond 30. If M is small it is just a slightly fatter normal distribution. 
# Helps in hypothesis testing in small samples, since the t-Dist is fatter (variance more than1) than the normal, it means that test statistic values have to a bit further away far from 0, to be able to reject any null hypothesis as compared to the normal dist. 

m = 4 # number of standard normals whose squares are summed

# Prob Density Function (Only for plotting, does not have a interpretation)
f = function(x) dt(x, m)
f(3) #P(x=2)

# Cum Mass Function
c = function(x) pt(x, m)
c(10) #P(x<2)
c(10:14) #P(x<2), P(x<3), P(x<4)

# Plotting PMF, CDF
Dx = seq(-10, 10, 1) # domain of Prob Mass Function
pdf_plot(Dx)
cdf_plot(Dx)

# Sampling
r = function(x) rt(x, m)
x = r(50)
hist_sample(x)

# Summary Stats
info(x)


# F-Distribution
# if C1 and C2 are two Chi-Squares, with degrees of freedoms M and N
# parameters - M and N
# F = C1/M / C2/N (ratio)
# Mean = 

M = 10 # number of standard normals whose squares are summed in C1
N = 20 # number of standard normals whose squares are summed in C2

# Prob Density Function (Only for plotting, does not have a interpretation)
f = function(x) df(x, M, N)
f(1) #P(x=2)

# Cum Mass Function
c = function(x) pf(x, M, N)
c(1) # P(x<2)
c(10:14) # P(x<2), P(x<3), P(x<4)

# Plotting PMF, CDF
Dx = seq(0, 5, 0.01) # domain of Prob Mass Function
pdf_plot(Dx)
cdf_plot(Dx)

# Sampling
r = function(x) rf(x, M, N)
x = r(1000)
hist_sample(x)

# Summary Stats
info(x)
