# DataCamp 3
library(dslabs)
# Exercise 1. Confidence interval for p
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>% 
  filter(enddate >= '2016-10-31' & state == "U.S.")
N <- polls$samplesize[1]
X_hat <- polls$rawpoll_clinton[1]/100
se_hat <- sqrt(X_hat*(1-X_hat)/N)
z <- qnorm(0.975)
pnorm(z)
ci <- c(X_hat - z * se_hat, X_hat + z * se_hat)

# Exercise 2. Pollster results for p
head(polls)
pollster_results <- polls %>%
  mutate(X_hat = rawpoll_clinton/100,
                 se_hat = sqrt(X_hat*(1-X_hat)/polls$samplesize),
                 lower = X_hat - qnorm(0.975) * se_hat,
                 upper = X_hat + qnorm(0.975) * se_hat) %>%
  select(pollster, enddate,X_hat,se_hat,lower,upper)

# Exercise 3. Comparing to actual results - p
p <- 0.482
avg_hit <- pollster_results %>%
  mutate(hit = 0.482 >= lower & 0.482 <= upper) %>%
  select(lower,upper,hit) %>% summarize(mean(hit))

# Exercise 4. Theory of confidence intervals
# 95% probability that the random interval falls on top of p.
# Exercise 5. Confidence interval for d
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(d_hat = (rawpoll_clinton - rawpoll_trump)/100)
N <- polls$samplesize[1]
d_hat <- polls$d_hat[1]
X_hat <- (d_hat+1)/2
se_hat <- 2 * sqrt(X_hat*(1-X_hat)/N)
z <- qnorm(0.975)
ci <- c(d_hat - z * se_hat, d_hat + z * se_hat)
ci <- (2*X_hat - 1) + c(-z, z)*se_hat # Same as line 41

# Exercise 6. Pollster results for d
head(polls)
pollster_results <- polls %>% mutate(X_hat = (d_hat+1)/2,
         se_hat = 2 * sqrt(X_hat*(1-X_hat)/samplesize),
         lower = d_hat - qnorm(0.975) * se_hat,
         upper = d_hat + qnorm(0.975) * se_hat) %>%
  select(pollster,samplesize, enddate,d_hat,lower,upper)

# Exercise 7. Comparing to actual results - d
head(pollster_results)
avg_hit <- pollster_results %>% mutate(hit = 0.021 >= lower & 0.021 <= upper) %>%
  summarise(mean(hit))

# Exercise 8. Comparing to actual results by pollster
head(polls)
data <- polls %>% mutate(error = d_hat - 0.021)
data %>% ggplot(aes(x = pollster , y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Exercise 9. Comparing to actual results by pollster - multiple polls
head(polls)
data <- polls %>% group_by(pollster) %>% filter(n() >= 5) %>%
  mutate(error = d_hat - 0.021)
data %>% ggplot(aes(x = pollster , y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
