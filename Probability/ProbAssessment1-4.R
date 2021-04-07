# Assessment 1.4
library(gtools)
library(tidyverse)
# Question 1: Olympic running
medals <- permutations(8,3)
medalsJ <- permutations(3,3)
nrow(medalsJ)/nrow(medals)

  
B <- 10000
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
results <- replicate(B,{
  winner <- sample(runners,3)
  (winner[1] == "Jamaica" & winner[2] == "Jamaica" & winner[3] == "Jamaica")
})
mean(results == TRUE)

# Question 2: Restaurant management
6* nrow(combinations(6,3))*3
# Question 2d
compute_prob <- function(n) {
  entree <- nrow(combinations(n,1))
  drink <- nrow(combinations(3,1))
  sides <- nrow(combinations(6,2))
  count <- entree*drink*sides
}
sapply(9, compute_prob)
# Question 2e
compute_prob <- function(n) {
  entree <- nrow(combinations(6,1))
  drink <- nrow(combinations(3,1))
  sides <- nrow(combinations(n,2))
  count <- entree*drink*sides
}
sapply(7, compute_prob)

# Questions 3 - # Question 3a
nrow(esoph)
# Question 3b
all_cases <- sum(esoph$ncases)
# Question 3c
all_controls <- sum(esoph$ncontrols)
# Question 4a
esoph %>% group_by(alcgp) %>% 
  summarize(ncontrols=sum(ncontrols),ncases=sum(ncases)) %>%
  arrange(desc(alcgp)) %>%
  mutate(prob=ncases/(ncontrols+ncases))
# Question 4b
# Question 4c group_by(tobgp) %>% 
esoph %>% filter(tobgp != "0-9g/day") %>% 
  summarise(casesTot=sum(ncases),prob=casesTot/all_cases)
# Question 4d
esoph %>% filter(tobgp != "0-9g/day") %>% 
  summarise(controlTot=sum(ncontrols),prob=controlTot/all_controls)
# Question 5a
esoph %>% group_by(alcgp) %>% summarise(grpCases=sum(ncases)/all_cases)
# Question 5b
esoph %>% group_by(tobgp) %>% summarise(grpCases=sum(ncases)/all_cases)
# Question 5c
esoph %>% group_by(alcgp,tobgp) %>% 
  summarise(gpcases=sum(ncases),prob=gpcases/all_cases)
# Question 5d
.225 + .155 - 0.05
# Question 6a
esoph %>% group_by(alcgp) %>% 
  summarise(gpcontr=sum(ncontrols),prob=gpcontr/all_controls)
# Question 6b
0.225/0.0687
# Question 6c
esoph %>% group_by(tobgp) %>% 
  summarise(gpcontr=sum(ncontrols),prob=gpcontr/all_controls)
# Question 6d
esoph %>% group_by(alcgp,tobgp) %>% 
  summarise(gpcontr=sum(ncontrols),prob=gpcontr/all_controls)
# Question 6e
0.0687 + 0.0841 - 0.0133
# Question 6f
0.33/0.1395
