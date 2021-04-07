library(dplyr)
library(NHANES)
data(NHANES)
ref_avg <- NHANES %>% filter(AgeDecade == " 20-29" 
                         & Gender == "female") %>% 
  summarize(average = mean(BPSysAve,na.rm = TRUE)) %>% .$average

## complete the line
NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female" & BPSysAve != "NA") %>% 
  summarize(minbp = min(BPSysAve, na.rm = TRUE), 
            maxbp = max(BPSysAve, na.rm = TRUE))
## Males - Group_By
NHANES %>% filter(Gender == "male") %>% 
  group_by(AgeDecade)
summarize(average = mean(BPSysAve, na.rm = TRUE), 
          standard_deviation = sd(BPSysAve, na.rm = TRUE))
# Exercise 7
bp <- NHANES %>% group_by(AgeDecade, Gender) %>%
summarize(average = mean(BPSysAve, na.rm = TRUE), 
          standard_deviation = sd(BPSysAve, na.rm = TRUE))
# Exercise 8. Arrange
NHANES %>% filter(Gender == "male" & AgeDecade == " 40-49") %>%
  group_by(Race1) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>%
  arrange(average)
  