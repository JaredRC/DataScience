# Load library
library(dplyr)
data(murders)

# Create new data frame called my_states (with specifications in the instructions)
my_states <- murders %>% 
  mutate(rate = total / population * 100000, rank = rank(-rate)) %>% 
  filter(region %in% c("Northeast", "West") & rate < 1) %>% 
  select(state,rate,rank)

## Define the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# show the result and only include the state, rate, and rank columns, all in one line, in that order
filter(murders, region %in% c("Northeast", "West") & rate < 1) %>% 
  select(state,rate,rank)