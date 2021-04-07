# 4.2 Assessment
library(dslabs)
data("death_prob")
# Questions 1 and 2: Insurance rates, part 1
gain <- 1150
loss <- -150000
n <- 1000
# Question 1a
p <- 0.003193
p1 <- 1 - p
# Question 1b - Expected Value
E <- (gain * p1 + loss * p)
# Question 1c - Standard Error
SE <- sqrt(1) * abs(loss - gain) * sqrt(p * p1)
# Question 1d
E <- n * (gain * p1 + loss * p)
# Question 1e
SE <- sqrt(n) * abs(loss - gain) * sqrt(p * p1)
# Question 1f
pnorm(0,E,SE)

# Question 2a
p <- 0.005013
# Question 2b
E <- 700000
a <- -150000
b <- 1459
E <- n * (a*p + b*(1 - p))
# Question 2c
SE <- sqrt(n) * abs(a - b) * sqrt(p * (1-p))
# Question 2d
pnorm(0,E,SE)

# Part 2
gain <- 1150
loss <- -150000
n <- 1000
# Question 3a
p <- 0.015
E <- n * (loss * p + gain * (1 - p))
# Question 3b
SE <- sqrt(n) * abs(loss - gain) * sqrt(p * (1-p))
# Question 3c
pnorm(0,E,SE)
# Question 3d
pnorm(-1000000,E,SE)
# Question 3e
p <- seq(.01, .03, .001)
pFunc <- function(p){
  E <- n * (loss * p + gain * (1 - p))
  SE <- sqrt(n) * abs(loss - gain) * sqrt(p * (1-p))
  p90 <- pnorm(0,E,SE)
  ifelse(p90 > .9,p90,NA) 
}
sapply(p, pFunc)
# Question 3f
p <- seq(.01, .03, .0025)
pFunc <- function(p){
  E <- n * (loss * p + gain * (1 - p))
  SE <- sqrt(n) * abs(loss - gain) * sqrt(p * (1-p))
  p90 <- pnorm(-1000000,E,SE)
  ifelse(p90 > .9,p90,NA) 
}
sapply(p, pFunc)

# Question 4a
set.seed(25, sample.kind = "Rounding")
p_loss <- .015
profit <- sample(c(gain,loss),n,prob=c(1 - p_loss, p_loss), replace = TRUE)
S <- sum(profit)
S/1e+6
# Question 4b
set.seed(27, sample.kind = "Rounding")
B <- 10000
S <- replicate(B,{
  profit <- sample(c(gain,loss),n,prob=c(1 - p_loss, p_loss), replace = TRUE)
  sum(profit)
})
mean(S < -1000000)

# Part 3
n <- 1000
p_loss <- 0.05
p <- 0.015
# Question 5a - Premium
l <- -150000
z <- qnorm(p_loss)
x <- -l * ( n * p - z * sqrt(n * p * (1 - p)))/
  ( n * (1 - p) + z * sqrt(n*p * (1 - p)))
# Question 5b - Expected profit / policy
E <- (l * p + x * (1 - p))
# Question 5c - Expected profit, 1000 policies
E <- n * (l * p + x * (1 - p))
# Question 5d - Monte Carlo