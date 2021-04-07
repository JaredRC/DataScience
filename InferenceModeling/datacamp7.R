# *********** Association Tests ***************
# Exercise 1 - Comparing Proportions of Hits
setwd("data-science/InferenceModeling")
errors <- read.csv("err2.csv")
head(errors)
totals <- errors %>% filter(grade %in% c("A-","C-")) %>% 
  group_by(grade,hit) %>%
  summarize(n = n()) %>% spread(hit,n) %>% 
  mutate(Pct = `TRUE`/(`FALSE`+`TRUE`))
totals %>% filter(grade == "A-") %>% .$Pct
totals %>% filter(grade == "C-") %>% .$Pct

# Exercise 2 - Chi-squared Test
totals <- errors %>% filter(grade %in% c("A-","C-")) %>% 
  group_by(hit,grade) %>% summarize(n = n()) %>% spread(grade,n)
head(totals)
chisq_test <- totals %>%
  select(-hit) %>% chisq.test()
chisq_test$p.value

# Exercise 3 - Odds Ratio Calculation
head(totals)
odds_C <- (totals$`C-`[2] / sum(totals$`C-`)) / 
  (totals$`C-`[1] / sum(totals$`C-`))
odds_A <- (totals$`A-`[2] / sum(totals$`A-`)) / 
  (totals$`A-`[1] / sum(totals$`A-`))
odds_A/odds_C

# Exercise 4 - Significance
fisher.test(totals)
