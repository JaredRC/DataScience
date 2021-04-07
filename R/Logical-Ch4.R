# an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}
# changing the condition to < 0.25 changes the result
if(murder_rate[ind] < 0.25){
  print(murders$state[ind]) 
} else{
  print("No state has a murder rate that low.")
}
# the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA)

# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)

# the ifelse() function is also helpful for replacing missing values
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

x <- c(1,2,-3,4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not All Positives")
}

x <- c(FALSE,FALSE,TRUE)
all(!x)

char_len <- nchar(murders$state)
head(char_len)

x <- c(2, 3, -999, 1, 4, 5, -999, 3, 2, 9)
ifelse(x == -999, NA, x)

new_names <- ifelse(nchar(murders$state) > 8,murders$abb,murders$state)
