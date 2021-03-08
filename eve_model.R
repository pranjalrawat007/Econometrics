# The Eve Model
# One good ('Eggplant'), One person ('Eve'), Pure Production Model
# Everyday, Eve must choose how much to labor so as to optimally consume Eggplant as well as enjoy leisure. 
# Three Parameters: Technology: a (general technology), b (labor elasticity of output); Utility: c (leisure-loving)
# Observable Data: Labor Use (L), Quantity Produced (Q)

# Load Packages
library(tidyverse)
library(gridExtra)

# Define Technology and Utility for Eve
ProdFn = function(L, a=1, b = 0.5) {
	return (a * L^b)
}

UtilFn = function(L, Q, c=0.5) {
	return (Q * (24-L)^c)
}

# SCENARIO 1: BASELINE
L = seq(0, 26, 0.1)
df = tibble(L, ProdFn(L), UtilFn(L, ProdFn(L)))
names(df) = c('L', 'Q', 'U')
U_opt1 = max(df$U, na.rm = T)
L_opt1 = df$L[which.max(df$U)]
Q_opt1 = df$Q[df$L==L_opt1]
PROD1 = df %>% drop_na() %>% ggplot(aes(L, Q)) + geom_point(aes(L_opt1, Q_opt1)) + geom_line()
UTIL1 = df %>% drop_na() %>% ggplot(aes(L, U)) + geom_point(aes(L_opt1, U_opt1)) + geom_line()
grid.arrange(PROD1, UTIL1, ncol=1, nrow = 2)

# SCENARIO 2: Labor-Neutral Technological Improvement e.g. Climatic Conditions - optimal labor remains same, but with increased output and utility. This is contingent on the fact that consumption-leisure are complimentary in the utility fn.
df = df %>% mutate(Q2 = ProdFn(L, a=2))
df = df %>% mutate(U2 = UtilFn(L, Q2))
U_opt2 = max(df$U2, na.rm = T)
L_opt2 = df$L[which.max(df$U2)]
Q_opt2 = df$Q2[df$L==L_opt2]
PROD2 = df %>% drop_na() %>% ggplot(aes(L, Q2)) + geom_point(aes(L_opt2, Q_opt2)) + geom_line()
UTIL2 = df %>% drop_na() %>% ggplot(aes(L, U2)) + geom_point(aes(L_opt2, U_opt2)) + geom_line()
grid.arrange(PROD2, UTIL2, ncol=1, nrow = 2)

# SCENARIO 3: Labor-saving technological progress e.g. Eve learns a new technique for plantation. 
df = df %>% mutate(Q3 = ProdFn(L, b=0.8))
df = df %>% mutate(U3 = UtilFn(L, Q3))
U_opt3 = max(df$U3, na.rm = T)
L_opt3 = df$L[which.max(df$U3)]
Q_opt3 = df$Q3[df$L==L_opt3]
PROD3 = df %>% drop_na() %>% ggplot(aes(L, Q3)) + geom_point(aes(L_opt3, Q_opt3)) + geom_line()
UTIL3 = df %>% drop_na() %>% ggplot(aes(L, U3)) + geom_point(aes(L_opt3, U_opt3)) + geom_line()
print(c(L_opt3, Q_opt3, U_opt3))
grid.arrange(PROD3, UTIL3, ncol=1, nrow = 2)

# SCENARIO 4: Eve becomes leisure-loving: optimal labor use falls, quantity falls, optimal leisure increases.
df = df %>% mutate(Q4 = ProdFn(L))
df = df %>% mutate(U4 = UtilFn(L, Q4, c=0.5))
U_opt4 = max(df$U4, na.rm = T)
L_opt4 = df$L[which.max(df$U4)]
Q_opt4 = df$Q4[df$L==L_opt4]
PROD4 = df %>% drop_na() %>% ggplot(aes(L, Q4)) + geom_point(aes(L_opt4, Q_opt4)) + geom_line()
UTIL4 = df %>% drop_na() %>% ggplot(aes(L, U4)) + geom_point(aes(L_opt4, U_opt4)) + geom_line()
grid.arrange(PROD4, UTIL4, ncol=1, nrow = 2)

# COMPARISION OF SCENARIOS
grid.arrange(PROD1, PROD2, PROD3, PROD4, UTIL1, UTIL2, UTIL3, UTIL4, ncol=4, nrow = 2)

# KEY IDEAS
# 1. Technology and Preferences determine labor used and quantity produced. 
# 2. Observables - labor, leisure and quantity
#
# 4. Functional form of technology and preferences matter (complimentary/substitutes/separability, etc.)
# 5. If labor use remains the same, but quantity rises. Then it must be labor-neutral technology. 
# 4. Preference

# SCENARIO 5: Baseline for Estimation
df = df %>% mutate(Q5 = ProdFn(L, a = 10, b = 0.3))
df = df %>% mutate(U5 = UtilFn(L, Q5, c=0.7))
U_opt5 = max(df$U5, na.rm = T)
L_opt5 = df$L[which.max(df$U4)]
a = 10
b = 0.3
c = 0.7
L = 7.2 + rnorm(100)*0.01
v = rnorm(100) * 0.01
Q = ProdFn(L, a = a, b = b) + v

# Estimation
library(dplyr)
plot(Q ~ L)
model <- lm(log(Q) ~ log(L))
a_hat = exp(model$coeff[1])
b_hat = model$coeff[2]
Li = 24 - L
R = Li/L
c_hat = b_hat * mean(R)

print(c(a, b, c))
print(c(a_hat, b_hat, c_hat))

# So just using L, Q we can obtain a, b, c. Where a and b are technology parameters, and c is the preference for leisure parameter. 
# The next step is to allow for standard errors. 

