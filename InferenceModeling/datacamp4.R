# Datacamp 4
library(dslabs)
data(heights)
# Exercise 1 - Heights Revisited
x <- heights %>% filter(sex == "Male") %>%
  .$height
mean(x)
sd(x)

# Exercise 2 - Sample the population of heights
head(x)
N <- 50
set.seed(1)
X <- sample(x,N,replace = TRUE)
mean(X)
sd(X)

# Exercise 3 - Sample and Population Averages
#mean and sd/sqrt(N)

# Exercise 4 - Confidence Interval Calculation
head(x)
set.seed(1)
N <- 50
X <- sample(x, N, replace = TRUE)
X_bar <- mean(X)
se <- sd(X)/sqrt(N)
ci <- c(X_bar - qnorm(0.975)*se, X_bar + qnorm(0.975)*se)

# Exercise 5 - Monte Carlo Simulation for Heights
mu <- mean(x)
N <- 50
B <- 10000
set.seed(1)
res <- replicate(B,{
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  interval <- c(mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)
  between(mu,interval[1],interval[2])
})
mean(res)

# Exercise 6 - Visualizing Polling Bias
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
polls %>% ggplot(aes(pollster,spread)) +
  geom_boxplot() + geom_point()

# Exercise 7 - Defining Pollster Bias
# How are biases different: b1 <> b2

# Exercise 8 - Derive Expected Value
 # d + b1

# Exercise 9 - Expected Value and Standard Error of Poll 1
 #Include bias so d + b1 and sd/sqrt(N)

# Exercise 10 - Expected Value and Standard Error of Poll 2
 # d + b1 and sd/sqrt(N)

# Exercise 11 - Difference in Expected Values Between Polls
 # b1 - b2

# Exercise 12 - Standard Error of the Difference Between Polls
 # sqrt(sd1**2/N1 + sd2**2/N2)

# Exercise 13 - Compute the Estimates
head(polls)
sigma <- polls %>% group_by(pollster) %>% 
  summarize(s = sd(spread))

# Exercise 14 - Probability Distribution of the Spread
 # If Ns are large enough, Y1 Y2 are normal.

# Exercise 15 - Calculate the 95% Confidence Interval of the Spreads
head(polls)
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread),N = n())
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[1]**2/res$N[1] + res$s[2]**2/res$N[2])
ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)

# Exercise 16 - Calculate the P-value
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n())
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
z <- estimate/se_hat
2*(1-pnorm(z))

# Exercise 17 - Comparing Within-Poll and Between-Poll Variability
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()
var <- polls %>% group_by(pollster) %>%
  summarize(N = n(),avg = mean(spread), s = sd(spread))