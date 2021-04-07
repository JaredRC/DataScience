library(dplyr)
library(ggplot2)
library(dslabs)
data(us_contagious_diseases)
# Exercise 5.2 - 1
dat <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% mutate(rate = count / population * 10000 * 52 / weeks_reporting)
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
state <- reorder(state,rate)
levels(state)

# Exercise 2: Customizing plots - redefining
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state = reorder(state,rate))  

dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

# Exercise 3: Showing the data and customizing plots
data("murders")
murders %>% mutate(rate = total/population*100000) %>%
  group_by(region) %>%
  summarize(avg = mean(rate)) %>%
  mutate(region = factor(region)) %>%
  ggplot(aes(region, avg)) +
  geom_bar(stat="identity") +
  ylab("Murder Rate Average")

# Exercise 4: Making a box plot
murders %>% mutate(rate = total/population*100000) %>%
  mutate(region = reorder(region, rate, FUN = median)) %>%
 ggplot(aes(region,rate)) + geom_boxplot() + geom_point()
