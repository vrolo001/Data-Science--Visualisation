###### Section 3: Summarising with dplyr ######

###Section 3.1
library(dplyr)
library(ggplot2)
library(dslabs)
install.packages("NHANES")
library(NHANES)
data(NHANES)

  #1:Filter the NHANES dataset so that only 20-29 year old females are included and assign this new data frame to the object tab
tab <- NHANES %>% 
  filter (Gender == "female" & AgeDecade == " 20-29")
  #2: Change the line of code to save the average and standard deviation of systolic blood pressure as average and standard_deviation to a variable called ref.
ref<- NHANES %>% 
  filter (Gender == "female" & AgeDecade == " 20-29") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))
  #3: Modify the line from the above exercise to assign the average to a numeric variable called ref_avg using the . or pull
ref_avg<- NHANES %>% 
  filter (Gender == "female" & AgeDecade == " 20-29") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>%
  .$average
  #4: Report the min and max values. Within summarize, save the min and max of systolic blood pressure as minbp and maxbp
NHANES %>% 
  filter (Gender == "female" & AgeDecade == " 20-29") %>%
  summarize(minbp = min(BPSysAve, na.rm = TRUE), maxbp = max(BPSysAve, na.rm = TRUE))
  #5: Compute the average and standard deviation of systolic blood pressure for females for each age group separately
NHANES %>%
  filter(Gender == "female") %>%
  group_by(AgeDecade) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))
  #6: Calculate the average and standard deviation of systolic blood pressure for males for each age group separately using the same methods as in the previous exercise
NHANES %>%
  filter(Gender == "male") %>%
  group_by(AgeDecade) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))
  #7: Create a single summary table for the average and standard deviation of systolic blood pressure for each age group AND each gender
NHANES %>%
  group_by(AgeDecade, Gender) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))
  #8: Compute the average and standard deviation for each value of Race1 for males in the age decade 40-49. Order the resulting table from lowest to highest average systolic blood pressure
NHANES %>%
  filter(AgeDecade == " 40-49" & Gender == "male") %>%
  group_by(Race1) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>%
  arrange(average)


