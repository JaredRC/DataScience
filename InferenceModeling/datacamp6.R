# Datacamp 6
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
# Exercise 1 - Confidence Intervals of Polling Data
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
cis <- polls %>% mutate(X_hat = (spread+1)/2,
         se = 2 * sqrt(X_hat*(1-X_hat)/samplesize),
         lower = spread - qnorm(0.975)*se,
         upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# Exercise 2 - Compare to Actual Results
add <- results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
p_hits <- ci_data %>% 
  mutate(hit = actual_spread >= lower & actual_spread <= upper) %>%
   summarize(mean(hit))

# Exercise 3 - Stratify by Pollster and Grade
add <- results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
p_hits <- ci_data %>% 
  mutate(hit = actual_spread >= lower & actual_spread <= upper) %>%
  group_by(pollster) %>% filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>%
  arrange(desc(proportion_hits))

# Exercise 4 - Stratify by State
add <- results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
p_hits <- ci_data %>% 
  mutate(hit = actual_spread >= lower & actual_spread <= upper) %>%
  group_by(state) %>% filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits))

# Exercise 5- Plotting Prediction Results
head(p_hits)
p_hits %>% ggplot(aes(state,proportion_hits)) + 
  geom_bar(stat = "identity") + coord_flip()

# Exercise 6 - Predicting the Winne
head(ci_data)
ci_data <-  
  mutate(ci_data,error = spread - actual_spread,
         hit = sign(spread) == sign(actual_spread))
errors <- ci_data
tail(errors)

# Exercise 7 - Plotting Prediction Results
errors <- cis %>% 
  mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
p_hits <- errors %>% 
  group_by(state) %>% filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit), n = n())
p_hits %>% ggplot(aes(state,proportion_hits)) + 
  geom_bar(stat = "identity") + coord_flip()

# Exercise 8 - Plotting the Errors
head(errors)
hist(errors$error)
median(errors$error)

# Exercise 9- Plot Bias by State
head(errors)
errors %>% filter(grade %in% c("A+","A","A-","B+")) %>%
  arrange(error) %>%
  ggplot(aes(state,error)) + 
  geom_boxplot() + geom_point()

# Exercise 10 - Filter Error Plot
head(errors)
errors %>% filter(grade %in% c("A+","A","A-","B+")) %>%
  group_by(state) %>% filter(n() >= 5) %>% 
  ungroup() %>% arrange(error) %>%
  ggplot(aes(state,error)) + 
  geom_boxplot() + geom_point()

#************ t-Distribution ******************
# Exercise 1 - Using the t-Distribution
curvtail <- 1 - pt(2, df = 3)
2 * curvtail

# Exercise 2 - Plotting the t-distributio
df <- 3:50
pt_func <- function(f){
  curvtail <- 1 - pt(2, df = f)
  2 * curvtail
}
probs <- sapply(df, pt_func)
plot(df,probs)

# Exercise 3 - Sampling From the Normal Distribution
data(heights)
x <- heights %>% filter(sex == "Male") %>% .$height
mu <- mean(x)
N <- 15
B <- 10000
set.seed(1)
res <- replicate(B,{
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  interval <- c(mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)
  between(mu,interval[1],interval[2])
})
mean(res)

# Exercise 4 - Sampling from the t-Distribution
mu <- mean(x)
N <- 15
B <- 10000
set.seed(1)
res <- replicate(B,{
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  interval <- c(mean(X) - qt(0.975,N-1)*se, mean(X) + qt(0.975,N-1)*se)
  between(mu,interval[1],interval[2])
})
mean(res)

# Exercise 5 - Why the t-Distribution?
#Allowed smaller sample sizes accounts for variability.