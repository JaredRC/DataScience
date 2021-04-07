# Exercise 4 - 
# Exercise 1. Bank earnings
n <- 10000
loss_per_foreclosure <- -200000
p_default <- 0.03
set.seed(1)
defaults <- sample(c(0,1),n,prob=c(1 - p_default, p_default), replace = TRUE)
S <- sum(defaults * loss_per_foreclosure)

# Exercise 2. Bank earnings Monte Carlo
B <- 10000
S <- replicate(B, {
  defaults <- sample(c(0,1),n,prob=c(1 - p_default, p_default), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
hist(S)

# Exercise 3. Bank earnings expected value
n * (p_default * loss_per_foreclosure + (1-p_default) * 0)
# Exercise 4. Bank earnings standard error
sqrt(n) * abs(loss_per_foreclosure) * sqrt(p_default * (1 - p_default))
# Exercise 5. Bank earnings interest rate - 1
x = - loss_per_foreclosure * p_default/(1-p_default)
x/180000

# Exercise 6. Bank earnings interest rate - 2
z <- qnorm(0.05)
x <- -loss_per_foreclosure * 
  ( n * p_default - z * sqrt(n * p_default * (1 - p_default)))/
  ( n * (1 - p_default) + z * sqrt(n*p_default * (1 - p_default)))
x/180000

# Exercise 7. Bank earnings - minimize money loss
#Reduce default