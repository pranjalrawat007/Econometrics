# This code borrows from the "Econometrics with R" online textbook found here: https://www.econometrics-with-r.org/
# And the "Use R!" book: https://jrsyzx.njau.edu.cn/__local/C/94/F1/35C7CC5EDA214D4AAE7FE2BA0FD_0D3DFF32_3CDD40.pdf?e=.pdf

# CRAN - Comprehensive R Archive Network, is a repository for code
nrow(available.packages(repos="http://cran.us.r-project.org"))

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

# subset vector
x[4]
x[-4] # all but 4th
x[2:4]
x[c(1,5)] # 1st and 5th element
x[x==1] # by value
x[x>1]
x[x<1]
x[x %in% c(1,2,5)] # where in 

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
