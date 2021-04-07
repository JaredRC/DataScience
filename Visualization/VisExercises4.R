library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
## fill out the missing parts in filter and aes
gapminder %>% filter(continent == "Africa" & year == 2012) %>%
  ggplot(aes(fertility, life_expectancy,color = region)) +
  geom_point()

df <- gapminder %>% 
  filter(continent == "Africa" & year == 2012 &
           fertility <= 3 & life_expectancy >= 70) %>%
  select(country,region)

countries <- c("Vietnam", "United States")
years <- 1960:2010
tab <- gapminder %>% filter(country %in% countries & year %in% years)
# Exercise 5
p <- tab %>% ggplot(aes(year,life_expectancy,color = country)) + geom_line()

# Exercise 6. Life expectancy in Cambodia
gapminder %>% filter(country == "Cambodia" & year %in% 1960:2010) %>% ggplot(aes(year,life_expectancy)) + geom_line()

# Exercise 7. Dollars per day - part 1
daydollars <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year == 2010 & !is.na(gdp))

# Exercise 8. Dollars per day - part 2
daydollars %>% ggplot(aes(dollars_per_day)) + 
  geom_density() +
  scale_x_continuous(trans = "log2")

# Exercise 9. Dollars per day - part 3 - multiple density plots
daydollars <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(gdp))
daydollars %>% ggplot(aes(dollars_per_day)) + 
  geom_density() +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ .)

# Exercise 10. Dollars per day - part 4 - stacked density plot
daydollars %>% ggplot(aes(dollars_per_day, group = region, fill = region)) + 
  geom_density(bw = 0.5, position = "stack") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ .)

#Exercise 11. Infant mortality scatter plot - part 1
gapminder_Africa_2010 <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year == 2010 & !is.na(gdp))
gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality,  color = region)) +
  geom_point()
# Exercise 12. Infant mortality scatter plot - part 2 - logarithmic axis
gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality,  color = region)) +
  geom_point() + scale_x_continuous(trans = "log2")
# Exercise 13. Infant mortality scatter plot - part 3 - adding labels
gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality,  color = region,label = country)) +
  geom_point() + scale_x_continuous(trans = "log2") + 
  geom_text()
# Exercise 14. Infant mortality scatter plot - part 4 - comparison of scatter plots
gapminder %>%
  filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(gdp) & !is.na(infant_mortality)) %>%
  mutate(dollars_per_day = gdp/population/365) %>% 
  ggplot(data = ,aes(dollars_per_day, infant_mortality,  color = region,label = country)) +
  geom_point() + scale_x_continuous(trans = "log2") + 
  geom_text() + facet_grid(year ~ .)
  