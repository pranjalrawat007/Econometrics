install.packages("stats")
library("stats")
set.seed(42)

# Dice Roll
sample(1:6, 1)
pmf = rep(1/6, 6)
cdf = cumsum(pmf)
plot(pmf, xlab = 'x', 'main'='Probability')
plot(cdf, xlab = 'x', 'main'='Probability')

# R stats -> d:pdf/pmf, p:cdf, q:inverse cdf, r:sampling
# Binomial
n = 10
p = 0.5
dbinom(5, n, p) #Prob(x=5|n=10,p=0.5)
sum(dbinom(5:8, n, p)) #Prob(5<=x<=8|n=10,p=0.5)
pbinom(8, n, p) #P(x<5|n=10,p=0.5)
Dx = 0:10 # domain
pmf = dbinom(Dx, n, p) # Vector, Prob(x=k|n=10,p=0.5)
cdf = cumsum(pmf)
plot(pmf, xlab = 'x', 'main'='Probability')
plot(cdf, xlab = 'x', 'main'='Probability')

# Normal Distribution
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


