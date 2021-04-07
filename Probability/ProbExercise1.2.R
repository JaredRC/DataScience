# Exercise 2. Sampling with replacement
cyan <- 3
magenta <- 5
yellow <- 7

# Assign the variable 'p_yellow' as the probability that a yellow ball is drawn from the box.
p_yellow <- yellow / (cyan + magenta + yellow)
# Using the variable 'p_yellow', calculate the probability of drawing a yellow ball on the sixth draw. Print this value to the console.
p_yellow

balls <- rep(c("cyan","magenta","yellow"),times = c(3,5,7))
B <- 10000
events <- replicate(B, sample(balls, 1))
tab <- table(events)
prop.table(tab)

# Exercise 3. Rolling a die
p_no6 <- 1 - 1/6
p_no6 ^ 6

# Exercise 4. Probability the Celtics win a game
# Assign the variable `p_cavs_win4` as the probability that the Cavs will win the first four games of the series.
p_cavs_win4 <- 0.6 ^ 4
# Using the variable `p_cavs_win4`, calculate the probability that the Celtics win at least one game in the first four games of the series.
1 - p_cavs_win4

# Exercise 5. Monte Carlo simulation for Celtics winning a game
B <- 10000
set.seed(1)
celtic_wins <- replicate(B,{
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games == "win")
})
mean(celtic_wins)
