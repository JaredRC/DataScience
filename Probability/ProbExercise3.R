# Section 3.1 Random Variables and Sampling Models
# Exercise 1. American Roulette
green <- 2
black <- 18
red <- 18
p_green <- green/(green + black + red)
# Exercise 2. American Roulette payout
p_not_green = 1 - p_green
n <- 1
X <- sample(c(-1, 1), n, replace = TRUE, prob = c(p_not_green,p_green))
# Exercise 3. American Roulette expected value
n*(17*p_green + -1*p_not_green)
# Exercise 4. American Roulette standard error
abs(-1 - 17) * sqrt(p_green * p_not_green)
# Exercise 5. American Roulette sum of winnings
set.seed(1)
n <- 1000
X <- sample(c(-1, 17), n, replace = TRUE, prob = c(p_not_green,p_green))
S <- sum(X)
# Exercise 6. American Roulette winnings expected value
n*(17*p_green + -1*p_not_green)
# Exercise 7. American Roulette winnings Standard Error
sqrt(n) * abs(-1 - 17) * sqrt(p_green * p_not_green)

# Section 3.2 The Central Limit Theorem
# Exercise 1. American Roulette probability of winning money
p_green <- 2 / 38
p_not_green <- 1-p_green
n <- 100
avg <- n * (17*p_green + -1*p_not_green)
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green) #Standard Error
pnorm(0,avg,se)
1 - pnorm(0,avg,se)
# Exercise 2. American Roulette Monte Carlo simulation
B <- 10000
set.seed(1)
S <- replicate(B, {
  X <- sample(c(17, -1), n, replace = TRUE, prob = c(p_green,p_not_green))
  sum(X)
})
mean(S)
sd(S)
# Exercise 3. American Roulette Monte Carlo vs CLT
mean(S > 0)
# Exercise 4. American Roulette Monte Carlo vs CLT comparison
#   probability is low CLT does not apply.
# Exercise 5. American Roulette average winnings per bet
set.seed(1)
n <- 10000
X <- sample(c(17, -1), n, replace = TRUE, prob = c(p_green,p_not_green))
Y <- mean(X)
# Exercise 6. American Roulette per bet expected value
17*p_green + -1*p_not_green
# Exercise 7. American Roulette per bet standard error
(abs(-1 - 17) * sqrt(p_green * p_not_green))/sqrt(n)
# Exercise 8. American Roulette winnings per game are positive
avg <- 17*p_green + -1*p_not_green
se <- 1/sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)
pnorm(0,avg,se)
1 - pnorm(0,avg,se)
# Exercise 9. American Roulette Monte Carlo again
n <- 10000
B <- 10000
set.seed(1)
S <- replicate(B, {
  X <- sample(c(17, -1), n, replace = TRUE, prob = c(p_green,p_not_green))
  mean(X)
})
mean(S)
sd(S)
# Exercise 10. American Roulette comparison
mean(S > 0)

# Assessment 3.3 - SAT testing
correct <- 1/5
wrong <- 1 - correct
# Question 1b
(1 * correct + -0.25 * wrong)
# Question 1c - Expected Score: e
n <- 44
e <- n * (1 * correct + -0.25 * wrong)
# Question 1d - standard error
se <- sqrt(n) * abs(-0.25 - 1) * sqrt(correct * wrong)
# Question 1e CLT
1 - pnorm(8, e, se)
# Question 1f - Monte Carlo
set.seed(21, sample.kind = "Rounding")
B <- 10000
n <- 44
S <- replicate(B, {
  X <- sample(c(1, -0.25), n, replace = TRUE, prob = c(correct,wrong))
  sum(X)
})
mean(S > 8)

# Question 2a
correct <- 1/4
wrong <- 1 - correct
n <- 44
E <- n * (1 * correct + 0 * wrong)
# Question 2b
p <- seq(0.25, 0.95, 0.05)
pFunc <- function(p){
  correct <- p
  wrong <- 1 - correct
  E <- n * (1 * correct + 0 * wrong)
  SE <- sqrt(n) * abs(0 - 1) * sqrt(correct * wrong)
  p35 <- 1 - pnorm(35,E,SE)
  ifelse(p35 > .8,p35,NA) 
}
sapply(p, pFunc)

# Question 3: Betting on Roulette
win <- 5/38
loose <- 1 - win
n <- 500
# Question 3a - Expected Value
E <- 1 * (6 * win + -1 * loose)
# Question 3b - Standard Error
SE <- sqrt(1) * abs(-1 - 6) * sqrt(win * loose)
# Question 3c - Average E
E <- ( n * (6 * win + -1 * loose) )/n
# Question 3d - Average SE
SE <- sqrt(n) * abs(-1 - 6) * sqrt(win * loose)
SE/n
# Question 3e - Sum E
E <- n * (6 * win + -1 * loose)
# Question 3f - Sum SE
SE <- sqrt(n) * abs(-1 - 6) * sqrt(win * loose)
# Question 3g - Prob losing money
pnorm(0,E,SE)
