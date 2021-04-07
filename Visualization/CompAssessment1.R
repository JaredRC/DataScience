library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)
#Question 1
mean(stars$magnitude)
sd(stars$magnitude)
# Question 2
stars %>% ggplot(aes(magnitude)) + geom_density()

# Question 3
stars %>% ggplot(aes(temp)) + geom_density()

# Question 4
stars %>% ggplot(aes(temp,magnitude)) + geom_point()

# Question 5
stars %>% ggplot(aes(temp,magnitude)) + 
  geom_point() + scale_y_reverse() + scale_x_log10() + scale_x_reverse()

# Question 8
stars %>% ggplot(aes(temp,magnitude)) + 
  geom_point() + scale_y_reverse() + scale_x_log10() + 
  scale_x_reverse() + geom_text(aes(label = star, hjust = -0.25))
# Question 9
stars %>% ggplot(aes(temp,magnitude, color = type)) + 
  geom_point() + scale_y_reverse() + scale_x_log10() + 
  scale_x_reverse()
