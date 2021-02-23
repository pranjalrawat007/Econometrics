
# RANDOM SAMPLING 1
# Simple Random Sampling - each instance is independent and identically distributed (i.i.d). Example, roll of a dice. 

# Let Y be a random variable e.g. Bernoulli Trial / Roll of Dice
# Sample Sum (S): Rolling a dice 2 times and taking the Sum
# Total possibilities for S: 2 to 12, Each is not equally likely
# the distribution of the sample sum (S) is called sampling distribution
S = sum(sample(1:6, 2, replace = T))

# Key point: the mean of S is equal to mean of s(i), the variance of S is a fraction of s(i). Irrespective of distribution of s(i).
# if Y was nromally distributed, 


# Monte Carlo Simulation on Normal Random Variable:
# generate R samples of size N each, calculate S using N observations, and get R number of Ss to plot Sampling Distribution

N = 100
R = 1000
samples = replicate(R, rnorm(N, 10, 20)) # Y is allowed to be of high variance
dim(samples)

# collect Ss
samples.avgs = colMeans(samples)
hist(samples.avgs) # S is not high variance

# Monte Carlo Simulation 2: showing that SS for Nromals leads to Chi-Sq :
N = 3
R = 1000
Z = replicate(R, rnorm(N)) # Y is Chi-Sq of DF 3.
S = colSums(Z ^ 2)
hist(S)

