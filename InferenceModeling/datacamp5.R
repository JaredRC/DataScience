# DataCamp 5
# Exercise 2 - Recalculating the SIDS Statistics
Pr_1 <- 1/8500
Pr_2 <- 1/100
Pr_2 * Pr_1

# Exercise 3 - Bayes' Rule in the Courtroom
# Pr(A) = Murderer
# Pr(B) = 2 Kids dead
# Pr(A | B) = Pr(B | A) * Pr(A) / Pr(B)

# Exercise 4 - Calculate the Probability
Pr_1 <- 1/8500
Pr_2 <- 1/100
Pr_B <- Pr_1*Pr_2
Pr_A <- 1/1000000 # mother murder rate
Pr_BA <- 0.50
Pr_AB <- Pr_BA * Pr_A / Pr_B

# Exercise 5 - Misuse of Statistics in the Courts
# He discounted rate of murder mothers

# Exercise 6 - Back to Election Polls
library(dplyr)
library(dslabs)
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
head(polls)
results <- polls %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(n()))

# Exercise 7 - The Prior Distribution
# mu to 0 and r 0.02 before seeing any polls.

# Exercise 8 - Estimate the Posterior Distribution
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
e <- B * mu + (1 - B)*Y

# Exercise 9 - Standard Error of the Posterior Distribution
se <- sqrt(1/(1/sigma^2 + 1/tau^2))

# Exercise 10- Constructing a Credible Interval
ci <- c(e - qnorm(0.975)*se,e + qnorm(0.975)*se)

# Exercise 11 - Odds of Winning Florida
exp_value <- B*mu + (1-B)*Y
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
pnorm(0,exp_value,se)

# Exercise 12 - Change the Priors
taus <- seq(0.005, 0.05, len = 100)
p_calc <- function(tau){
  B <- sigma^2 / (sigma^2 + tau^2)
  exp_value <- B*mu + (1-B)*Y
  se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
  p <- pnorm(0,exp_value,se)
}
ps <- sapply(taus, p_calc) # %>% plot(tau,p)
plot(taus,ps)
