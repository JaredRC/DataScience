# Poll aggregators
library(tidyverse)
library(dslabs)
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d + 1) / 2

polls <- map_df(Ns, function(N) {
  x <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / N)
  list(estimate = 2 * x_hat - 1, 
       low = 2*(x_hat - 1.96*se_hat) - 1, 
       high = 2*(x_hat + 1.96*se_hat) - 1,
       sample_size = N)
}) %>% mutate(poll = seq_along(Ns))
sum(polls$sample_size)
d_hat <- polls %>% 
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>% 
  pull(avg)

# Poll data
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade)))
# We add a spread estimate:
  polls <- polls %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
  
d_hat <- polls %>% 
    summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>% 
    pull(d_hat)
#  and the standard error is:
p_hat <- (d_hat+1)/2 
moe <- 1.96 * 2 * sqrt(p_hat * (1 - p_hat) / sum(polls$samplesize))
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

# 16.2 Data-driven models
one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()
# Here is a histogram of the data for these 15 pollsters:
qplot(spread, data = one_poll_per_pollster, binwidth = 0.01)

sd(one_poll_per_pollster$spread)
# We are now ready to form a new confidence interval based on our new data-driven model:
results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), 
            se = sd(spread) / sqrt(length(spread))) %>% 
  mutate(start = avg - 1.96 * se, end = avg + 1.96 * se) 
round(results * 100, 1)

# 16.4 Bayesian statistics

# 16.5 Bayes theorem simulation
prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease","Healthy"), N, replace = TRUE, 
                  prob = c(prev, 1 - prev))
# Note that there are very few people with the disease:
N_D <- sum(outcome == "Disease")
N_H <- sum(outcome == "Healthy")
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"]  <- 
  sample(c("+", "-"), N_D, replace = TRUE, prob = c(accuracy, 1 - accuracy))
test[outcome == "Healthy"]  <- 
  sample(c("-", "+"), N_H, replace = TRUE, prob = c(accuracy, 1 - accuracy))
table(outcome, test)

# 16.6 Hierarchical models

# 16.8 Case study: election forecasting
# tidyverse and dslabs loaded previously
polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>% 
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
#Bayesian approach
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
posterior_mean + c(-1.96, 1.96)*posterior_se
#The posterior probability
1 - pnorm(0, posterior_mean, posterior_se)

set.seed(3)
J <- 6
N <- 2000
d <- .021
p <- (d + 1)/2
X <- d + rnorm(J, 0, 2 * sqrt(p * (1 - p) / N))

I <- 5
J <- 6
N <- 2000
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2 * sqrt(p * (1 - p) / N))
})
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d + 1) / 2
h <- rnorm(I, 0, 0.025)
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, 0, 2 * sqrt(p * (1 - p) / N))
})
sd(one_poll_per_pollster$spread)

mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
1 - pnorm(0, posterior_mean, posterior_se)
#Predicting the electoral college
results_us_election_2016 %>% top_n(5, electoral_votes)
results <- polls_us_election_2016 %>%
  filter(state!="U.S." & 
           !str_detect(state, "CD") & 
           enddate >="2016-10-31" & 
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

results %>% arrange(abs(avg))
results <- left_join(results, results_us_election_2016, by = "state")
results_us_election_2016 %>% filter(!state %in% results$state) %>% 
  pull(state)
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))
#Monte Carlo
B <- 10000
mu <- 0
tau <- 0.02
clinton_EV <- replicate(B, {
  results %>% mutate(sigma = sd/sqrt(n), 
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B * mu + (1 - B) * avg,
                     posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
                     result = rnorm(length(posterior_mean), 
                                    posterior_mean, posterior_se),
                     clinton = ifelse(result > 0, electoral_votes, 0)) %>% 
    summarize(clinton = sum(clinton)) %>% pull(clinton) + 7
})

mean(clinton_EV > 269)

tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/n  + bias_sd^2),  
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2)),
                     result = rnorm(length(posterior_mean), 
                                    posterior_mean, posterior_se),
                     clinton = ifelse(result>0, electoral_votes, 0)) %>% 
    summarize(clinton = sum(clinton) + 7) %>% 
    pull(clinton)
})
mean(clinton_EV_2 > 269)
#Forecasting
one_pollster <- polls_us_election_2016 %>% 
  filter(pollster == "Ipsos" & state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
se <- one_pollster %>% 
  summarize(empirical = sd(spread), 
            theoretical = 2 * sqrt(mean(spread) * (1 - mean(spread)) /
                                     min(samplesize)))

# 16.10 The t-distribution
one_poll_per_pollster %>%
  ggplot(aes(sample=spread)) + stat_qq()
z <- qt(0.975,  nrow(one_poll_per_pollster)-1)
one_poll_per_pollster %>% 
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>% 
  mutate(start = avg - moe, end = avg + moe)
qt(0.975, 14)
# is bigger than
qnorm(0.975)

data("polls_us_election_2016")
polls_us_election_2016 %>%
  filter(state =="Wisconsin" &
           enddate >="2016-10-31" & 
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  mutate(state = as.character(state)) %>%
  left_join(results_us_election_2016, by = "state") %>%
  mutate(actual = clinton/100 - trump/100) %>%
  summarize(actual = first(actual), avg = mean(spread), 
            sd = sd(spread), n = n()) %>%
  select(actual, avg, sd, n)
