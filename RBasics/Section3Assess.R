library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers
data(olive)
head(olive)

avg <- mean(heights$height)
ind <- filter(heights, height > avg & sex == "Female")
nrow(ind)
which.min(heights$height)
min(heights$height)
x <- 50:82
heights2 <- mutate(heights,ht_cm = height * 2.54)
ord <- order(heights2$ht_cm)
heights2$ht_cm[18]
mean(heights2$ht_cm, heights2$sex == "Female")
sum(heights2$sex)
mean(heights2$ht_cm)
match(heights2$sex == "Female")
fem <- heights2$sex == "Female"
fem <- filter(heights2, sex == "Female")
mean(fem$ht_cm)
