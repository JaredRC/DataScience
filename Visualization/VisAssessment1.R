# Assessments
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic %>%
  ggplot(aes(Age, color = Sex)) + 
  geom_density(bw = 0.25, alpha = 0.25, position = "stack")

titanic %>%
  ggplot(aes(Age,..count..,color = Sex)) + geom_density()

# Question 3: QQ-plot of Age Distribution
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) + 
  geom_qq(dparams = params) + 
  geom_abline()

# Question 4: Survival by Sex
titanic %>% ggplot(aes(Survived, fill = Sex)) + 
  geom_bar(position = position_dodge())

# Question 5: Survival by Age
titanic %>%
  ggplot(aes(Age,fill = Survived)) + 
  geom_density(alpha = 0.2)

# Question 6: Survival by Fare
titanic %>% 
  filter(Fare > 0) %>%
  ggplot(aes(Survived,Fare)) + 
  geom_boxplot() + scale_y_continuous(trans = "log2") +
  geom_jitter(width = 0.35, alpha = 0.2)

# Question 7: Survival by Passenger Class
titanic %>%
  ggplot(aes(Pclass,fill = Survived)) + 
  geom_bar()
titanic %>%
  ggplot(aes(Pclass,fill = Survived)) + 
  geom_bar(position = position_fill())
titanic %>%
  ggplot(aes(Survived,fill = Pclass)) + 
  geom_bar(position = position_fill())

# Question 8: Survival by Age, Sex and Passenger Class
titanic %>%
  ggplot(aes(Age,..count..,fill = Survived,is.na(Age))) + 
  geom_density() +
  facet_grid(Sex ~ Pclass)
