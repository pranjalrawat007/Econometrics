# This code borrows from the "Econometrics with R" online textbook found here: https://www.econometrics-with-r.org/
# And the "Use R!" book: https://jrsyzx.njau.edu.cn/__local/C/94/F1/35C7CC5EDA214D4AAE7FE2BA0FD_0D3DFF32_3CDD40.pdf?e=.pdf

# CRAN - Comprehensive R Archive Network, is a repository for code
nrow(available.packages(repos="http://cran.us.r-project.org"))

install.packages("tidyverse")  
library(tidyverse)
# 4 Windows in RStudio - code editor, terminal, data viewer and grapher

# R Calculator
10*10
1+3
4-3.5
5/6
2^2
2+3*4 #BODMAS
5%%3 #modulus

# Inequalities
4 > 3
4 >= 3
4.5 < -4
3==3.0
3 != 3

# Boolean
!4>3 #not
4>3 & 4<3 #and
4>3 | 4<3 #or

# Assignment
3 ->> x # rightward assignment
x <<- x # leftward assignment

# precision issue for infinitely expanding decimals
(2)^(1/2) * (2)^(1/2) - 2 # should be zero

#vectors
x = c(1,2,3,4)
y = c(-1, -2, -3, -4)
z = c('hello', 'world')
x + y
x = 2:5

# Subset vector
x[4]
x[-4] # all but 4th
x[2:4]
x[c(1,5)] # 1st and 5th element
x[x==1] # by value
x[x>1]
x[x<1]
x[x %in% c(1,2,5)] # where in 

# vectors can contain characters, logicals, ints and numerics
x = vector('character', length=10)
y = numeric(10)

length(y)
typeof(y)
class(y)
str(y)
as.character(y) 
as.numeric(y)

# MATRICES
# Creating Matrices
m = matrix(1:16, nrow = 4, ncol = 4) #1
m

n = 1:16 #2
dim(n) = c(4, 4)
n

x = 1:3  #3
y = 10:12
cbind(x, y)
rbind(x, y)

x = matrix(c(1,2,3, 11,12,13), #4
       nrow = 2, ncol = 3, byrow = TRUE,
       dimnames = list(c("r1", "r2"),
                       c("c1", "c2", "c3")))
x

#5
ones = matrix(1,3,10)
zeros = matrix(0, 3, 10)
random = matrix(rnorm(36), nrow = 6)

# checks
is.matrix(m)
is.vector(x)

# operations
2 * m
n + m
n * m # kroneker product
n %*% m # dot product
t(n) # transpose
diag(n) # diagonal vector
qr(n) # rank
det(random) # determinant
solve(random) # inverse
dim(n) # dimensions
nrow(n) # rows
ncol(n) # cols
colSums(n) 
rowSums(n)
colMeans(n)
rowMeans(n)

# loops
x = c(1:10)

for (i in x){
  i = i+1
  print(i)
}

i = 1
while (i < 5){
  print(i)
  i = i + 1
}

# if then

x = 1
if (x == 2){
  print('no')
} else {
  print('yes')
}

x# functions
z = seq(from=1, to=20, by=2)
mean(z)
sort(x) #sort
rev(x) #reverse
table(x)
unique(x) #nunique




# help and install
help.search('mean')
help(package='auto')
install.packages(‘dplyr’)
class(iris)


# dplyr
install.packages(‘dplyr’)
df = read_csv('open_police.csv') # tibble data structure
df = select(df, date, time, subject_race, subject_age)

slice(df, 1:4) # subset on rows
slice
