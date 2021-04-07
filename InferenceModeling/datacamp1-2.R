# Exercise 1. Polling - expected value of S
E <- 25*p
# Exercise 2. Polling - standard error of S
SE <- sqrt(25 * p * (1-p))
# Exercise 3. Polling - expected value of X-bar
X <- p
# Exercise 4. Polling - standard error of X-bar
SE <- sqrt(p*(1-p)/N)
# Exercise 5. se versus p
N <- 25
p <- seq(0,1,length.out = 100)
se <- sqrt(p*(1-p)/N)
plot(p,se)
# Exercise 6. Multiple plots of se versus p
p <- seq(0, 1, length = 100)
sample_sizes <- c(25, 100, 1000)
for (N in sample_sizes) {
  se <- sqrt(p*(1-p)/N)
  plot(p,se)
}
# Exercise 7. Expected value of d
2*p - 1
# Exerc2ise 8. Standard error of d
2 * sqrt(p*(p-1)/N)
# Exercise 9. Standard error of the spread
N <- 25
p <- 0.45
se <- 2 * sqrt(p*(1-p)/N)
# Exercise 10. Sample size

# For Section 2
# Exercise 1. Sample average
take_sample <- function(p,N){
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
}

p <- 0.45
N <- 100
set.seed(1)
take_sample(p,N)
# Exercise 2. Distribution of errors - 1
B <- 10000
set.seed(1)
errors <- replicate(B, {
  p - take_sample(p,N)
})
mean(errors)
hist(errors)
# Exercise 3. Distribution of errors - 2
hist(errors)
# Exercise 4. Average size of error
p <- 0.45
N <- 100
B <- 10000
set.seed(1)
errors <- replicate(B, p - take_sample(p, N))
errors <- abs(errors)
mean(errors)
# Exercise 5. Standard deviation of the spread
set.seed(1)
errors <- replicate(B, p - take_sample(p, N))
mean(errors**2)
sqrt(mean(errors**2))
# Exercise 6. Estimating the standard error
p <- 0.45
N <- 100
se <- sqrt(p*(1-p)/N)
X_bar <- take_sample(p,N)
# Exercise 7. Standard error of the estimate
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
X_bar <- mean(X)
se <- sqrt(X_bar*(1-X_bar)/N)
# Exercise 8. Plotting the standard error
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N,se)
# Exercise 9. Distribution of X-hat
N <- 100
# Exercise 10. Distribution of the errors
# Practically 0
# Exercise 11. Plotting the errors
set.seed(1)
errors <- replicate(B, p - take_sample(p, N))
qqnorm(errors)
qqline(errors)
# Exercise 12. Estimating the probability of a specific value of X-bar
p <- 0.45
N <- 100
X_hat <- 0.5
X_hat - p
# Pr(X > p + 0.05)
se <- sqrt(p*(1-p)/N)
1-pnorm(0.05/se)
# Exercise 13. Estimating the probability of a specific error size
X_hat <- 0.51
se_hat <- sqrt(X_hat*(1-X_hat)/N)
probwin <- pnorm(0.01/se_hat) - pnorm(-0.01/se_hat)
1 - probwin
