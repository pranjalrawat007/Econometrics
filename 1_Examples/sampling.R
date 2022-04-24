# RANDOM SAMPLING
# Simple Random Sampling - each instance is independent and identically distributed (i.i.d). E.g.: roll of a dice, toss of a coin. 
# Let Y be a random variable, let there be N instances. 
# Sample Sum: S = sum(Ys)
# The distribution of the sample sum (S) is called sampling distribution
# Key point: the mean of S is equal to mean of Y(i), the variance of S is a fraction of variance of Y(i).

# Monte Carlo Simulation 1:
# Let Y be Normally distributed
# Generate R samples of size N each, calculate S using N observations, and get R number of Ss to plot Sampling Distribution

# Y~N(10, 20)
μ=10 
σ=20
r = function(x) rnorm(x, μ, σ)
hist(r(100)) # Y Has high variance.

N = 100 # Sample Size
R = 1000 # Repetitions
samples = replicate(R, r(N)) 

# Sample Average
samples.avgs = colMeans(samples)
hist(samples.avgs) # S has lower variance, same mean

# Monte Carlo Simulation 2:
# Y is chi-Square
M = 10
r = function(x) rchisq(x, df=M)
hist(r(100)) # Y is left skewed

# Replicate
N = 100 # Sample Size
R = 1000 # Repetitions
samples = replicate(R, r(N)) 

# Sample Average
samples.avgs = colMeans(samples)
hist(samples.avgs) # S is normally distributed. 

# Large Sample Approximation to Sampling Distribution
# If Y's dist is known e.g. binomial, bernoulli, etc. Then you can analytically obtain the exact sample distribution
# If Y's dist is unknown, then we want to approximate the "distribution of S, with N instances" from a large sample of Ys (Asymptotic distribution).

# Law of large numbers (intuitive): in large samples, S is close to Expectation of Y, with very high probability. Or, in a large sample, the sample mean is very likely close to the population mean. As the sample size increases, the sample mean converges to the population mean. 
# LLN is the basis for casinos. They know the way the cards are, the exact expected benefit. So even if someone wins big, they know that with enough trials, these wins will disappear and then the 'house always wins'.

# Central Limit Theorem (astonishing): for any distribution Y, the sample mean calculated on N instances, will increasing look like a Normal distribution (with same mean as the population mean, and reduces variance), as N increases. In a sense, every distribution's mean behaves Normally. 

#
