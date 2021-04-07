# Use filter to create a new data frame no_south
no_south <- filter(murders, region != "South")
# Use nrow() to calculate the number of rows
nrow(no_south)

# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <- filter(murders, region %in% c("Northeast","West"))
# Number of states (rows) in this category 
nrow(murders_nw)

# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))
# Create a table, call it my_states, that satisfies both the conditions 
my_states <- filter(murders,region %in% c("Northeast","West") & rate < 1)
# Use select to show only the state name, the murder rate and the rank
select(my_states,state)