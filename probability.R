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
f = function(x) dbinom(x, n, p)
f(2) #P(x=2)
f(2:5) #P(2),P(3),P(4),P(5)
sum(f(2:5)) #P(2<x<5)

# Cum Mass Function
c = function(x) pbinom(x, n, p)
c(2) #P(x<2)
c(2:4) #P(x<2), P(x<3), P(x<4)

# Plotting PMF, CDF
Dx = seq(0, 10, 1) # domain of Prob Mass Function
plot(f(Dx), xlab = 'Number of Heads (x)', ylab = 'P(X=x)', main = 'PMF of Bin(10,0.3)')
plot(c(Dx), xlab = 'Number of Heads (x)', ylab = 'P(X<x)', main = 'CMF of Bin(10,0.3)')

# Sampling
r = function(x) rbinom(x, n, p)
x = r(100)
hist(x, xlab = 'Sampled Values', main = 'Histogram of samples from BIN(10, 0.3)')

# Summary Stats
info = function(x) {
	print(mean(x))
	print(var(x))
	print(summary(x))
	hist(x, xlab = 'Sampled Values', main = 'Histogram of samples')
	}
info(x)

# NORMAL
dnorm(5, 10, 2) # P(x = 5 | mean = 10, stdev = 2)
sum(dnorm(5:10, 10, 2)) # P(5 <= x <= 10| mean = 10, stdev = 2)
pnorm(4, 10, 2) # P(x<10, 10, 2)
Dx = seq(from = 5, to = 15, by = 0.1) # domain vector
pmf = dnorm(Dx, n, p) # Vector, Prob(x=k|n=10,p=0.5)
cdf = cumsum(pmf)
plot(pmf, xlab = 'x', 'main'='Probability')
plot(cdf, xlab = 'x', 'main'='Probability')
x = rnorm(1000, 10, 2) # 1000 samples of N(10, 2)
mean(x)
median(x)
var(x)

# Custom Density Function
f = function(x) 3/x^4
Dx = seq(from = 1, to = 4, by = 0.1) # domain vector
pdf = f(Dx)
cdf = cumsum(pdf)
plot(pdf, xlab = 'x', 'main'='Probability')

integrate(f, 2, 4)$value # P(2<x<4)

Ef = function(x) x * f(x)
integrate(Ef, 1, Inf)$value

Vf = function(x) x^2 * f(x)
integrate(Vf, 1, Inf)$value

# From Normal to Chi Square
x = seq(-3, 3,by = 0.1)
fx = pdf(x)


