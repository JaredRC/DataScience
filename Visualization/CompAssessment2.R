# CompAssessment 2
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
# Question 1
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% .$year %>% max()
  
# Question 2
yearfirst <- temp_carbon %>% filter(!is.na(carbon_emissions)) %>% 
  .$year %>% min()
yearlast <- temp_carbon %>% filter(!is.na(carbon_emissions)) %>% 
  .$year %>% max()
temp_carbon %>% 
  filter(year %in% c(yearfirst,yearlast) & !is.na(carbon_emissions))

# Question 3
idxFirst <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% 
  .$year %>% which.min()
idxLast <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% 
  .$year %>% which.max()
temp_carbon %>% select(year,temp_anomaly) %>%
  summarize(temp_anomaly[idxLast] - temp_anomaly[idxFirst])

# Question 4
p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year,temp_anomaly)) + geom_line()
p <- p + geom_hline(aes(yintercept = 0), color = "blue")

# Question 5
p <- p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(2000,0.05,label = "20th century mean"),color = "blue")

# Question 6

# Question 7
p + geom_line(aes(year,land_anomaly), color = "green") + 
  geom_line(aes(year, ocean_anomaly), color = "turquoise")

# Question 8
greenhouse_gases %>%
  ggplot(aes(year,concentration)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

# Question 10
temp_carbon %>% ggplot(aes(year,carbon_emissions)) + 
  geom_line() + geom_vline(xintercept = 1850, color = "lightblue") +
  geom_vline(xintercept = 2014, color = "purple") + 
  geom_vline(xintercept = 1960)

# Question 11
co2_time <- historic_co2 %>% 
  ggplot(aes(year, co2, color = source)) + geom_line()

from <- -375000
to <- -330000
# Question 12: -800000,-775000; -375000,-330000; -140000,-120000; -3000,2018
co2_time + scale_x_continuous(limits = c(from,to)) +
  geom_hline(yintercept = 180, color = "lightblue") + 
  geom_hline(yintercept = 300, color = "lightblue")

