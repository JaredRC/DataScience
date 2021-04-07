# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}
avg(c(1,2,3,4,5,6))

# variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s

# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

my_func <- function(x){
  y <- x + 1
  y
}
my_func(5)

# Create function called `sum_n`
sum_n <- function(n){
  y <- 1:n
  sum(y)
}
# Use the function to determine the sum of integers from 1 to 5000
sum_n(5000)

# Create `altman_plot` 
altman_plot <- function(x,y){
  plot(x+y,y-x)
}
altman_plot(5,10)

log_plot <- function(x, y){
  plot(log10(x), log10(y))
}
log_plot(10,20)

x <- 8
my_func <- function(y){
  x <- 9
  print(x)
  y + x
}
my_func(x)
print(x)