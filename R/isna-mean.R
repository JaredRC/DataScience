# Create the ind vector
library(dslabs)
data(na_example)
ind <- is.na(na_example)

# We saw that this gives an NA
mean(na_example)

# Compute the average, for entries of na_example that are not NA 
sum(na_example[!ind])/length(na_example[!ind])
# or
mean(na_example[!ind])
