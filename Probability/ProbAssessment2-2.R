# 2.2 Assessment: Continuous Probability
set.seed(16,sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)
# Question 1a
actmean <- mean(act_scores)
actsd <- sd(act_scores)
# Question 1c
perfect <- 0
i <- 0
for (scr in act_scores) {
  if(scr >= 36){
    i<-i+1
    perfect[i] = scr
  }
}

# Question 1d
mean(act_scores <= 10)

# Question 2
x <- 1:36
f_x <- data.frame(x, f = dnorm(x,20.9,5.7))
f_x %>% ggplot(aes(x,f)) + geom_line()

# Question 3
ZScores <- data.frame(ActScore = act_scores, Z = scale(act_scores))
# Question 3a
mean(z > 2)
# Question 3b
actsd*2 + actmean
# Question 3c
qnorm(0.975,actmean,actsd)

# Question 4
F <- function(a){
  mean(act_scores <= a)
}
# Question 4a
sapply(x, F)
# Question 4b
qnorm(0.95,20.9,5.7)
# Question 4c
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores,p)
# Question 4d
theoretical_quantiles <- qnorm(p,20.9,5.7)
#data.frame(theoretical_quantiles, sample_quantiles) %>%
  qqplot(theoretical_quantiles, sample_quantiles)
