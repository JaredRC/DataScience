# Continuous Probability
# Exercise 1. Distribution of female heights - 1
# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

pnorm(60, female_avg, female_sd)

# Exercise 2. Distribution of female heights - 2
1 - pnorm(72, female_avg, female_sd)

# Exercise 3. Distribution of female heights - 3
pnorm(67, female_avg, female_sd) - pnorm(61, female_avg, female_sd)

# Exercise 4. Distribution of female heights - 4
female_avg <- 64*2.54
female_sd <- 3*2.54
pnorm(67*2.54, female_avg, female_sd) - pnorm(61*2.54, female_avg, female_sd)

# Exercise 5. Probability of 1 SD from average
female_avg <- 64
female_sd <- 3
taller <- female_avg + female_sd
shorter <- female_avg - female_sd
pnorm(taller, female_avg, female_sd) - pnorm(shorter, female_avg, female_sd)

# Exercise 6. Distribution of male heights
male_avg <- 69
male_sd <- 3
# 99th percentile
qnorm(0.99,male_avg,male_sd)

# Exercise 7. Distribution of IQ scores
B <- 1000
set.seed(1)
highestIQ <- replicate(B, {
  random_data <- rnorm(10000, 100, 15)
  max(random_data)
})
hist(highestIQ)
