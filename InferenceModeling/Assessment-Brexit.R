# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

# Question 1: Expected value and standard error of a poll
N <- 1500
e <- p*N # Total Remain
se <- sqrt(N * p * (1-p)) # SE of total Remain
e <- p # Expected val of X_hat propertion of Remain
se <- sqrt(p*(1-p)/N) # SE of X_hat - Remain
d <- 2*p-1 #Spread
sed <- 2 * se # SE of spread d between Remain and Leave.

# Question 2: Actual Brexit poll estimates
brexit_polls <- brexit_polls %>% mutate(x_hat = (spread+1)/2)
mean(brexit_polls$spread)
sd(brexit_polls$spread)
mu <- mean(brexit_polls$x_hat)
s <- sd(brexit_polls$x_hat)

# Question 3: Confidence interval of a Brexit poll
x_hat <- brexit_polls$x_hat[1]
N <- brexit_polls$samplesize[1]
se <- sqrt(x_hat*(1-x_hat)/N)
x_hat - qnorm(0.975)*se
x_hat + qnorm(0.975)*se

# Question 4: Confidence intervals for polls in June
june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se = 2 * se_x_hat,
         lower = spread - qnorm(0.975)*se,
         upper = spread + qnorm(0.975)*se,
         hit = (d >= lower & d <= upper))
june_polls %>% summarize(zero = sum(0 >= lower & 0 <= upper)/n() ) %>% .$zero
june_polls %>% summarize(posi = sum(lower > 0)/n() ) %>% .$posi
june_polls %>% summarize(hits = sum(hit)/n() ) %>% .$hits
sum(june_polls$hit)

# Question 5: Hit rate by pollster
jp<- june_polls %>% group_by(pollster) %>% 
  summarize(n = n(), prophits = sum(hit)/n) %>% 
  arrange(prophits)

# Question 6: Boxplot of Brexit polls by poll type
june_polls %>% select(type = poll_type,spread) %>%
  ggplot(aes(type,spread)) + geom_boxplot()

# Question 7: Combined spread across poll type
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type <- mutate(combined_by_type,
                           se = 2 * sqrt(p_hat*(1-p_hat)/N),
                           lower = spread - qnorm(0.975)*se,
                           upper = spread + qnorm(0.975)*se)

# Question 8: Interpreting combined spread estimates across poll type

#***************** Brexit poll analysis - Part 3 ****************
# brexit_polls added x_hat as in Q2

# Question 9: Chi-squared p-value
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
totals <- brexit_hit %>% group_by(hit,poll_type) %>%
  summarize(n = n()) %>% spread(poll_type,n)
chisq_test <- totals %>% select(-hit) %>% chisq.test()
chisq_test$p.value

# Question 10: Odds ratio of online and telephone poll hit rate
oddsOnl <- (totals$Online[2] / sum(totals$Online)) / 
  (totals$Online[1] / sum(totals$Online))
oddsTel <- (totals$Telephone[2] / sum(totals$Telephone)) / 
  (totals$Telephone[1] / sum(totals$Telephone))
oddsOnl/oddsTel

# Question 11: Plotting spread over time
brexit_polls %>% ggplot(aes(enddate,spread,color = poll_type)) + 
  geom_smooth(method = "loess", span = 0.4) + geom_point() + 
  geom_hline(yintercept =  -0.038)

# Question 12: Plotting raw percentages over time
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long %>% ggplot(aes(enddate,proportion,color = vote)) + 
  geom_smooth(method = "loess", span = 0.3)
