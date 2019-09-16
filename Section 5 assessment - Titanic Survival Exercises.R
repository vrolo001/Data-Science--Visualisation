###### Titanic Survival Exercises ######

#The Titanic was a British ocean liner that struck an iceberg and sunk on its maiden voyage in 1912 
#from the United Kingdom to New York. More than 1,500 of the estimated 2,224 passengers and crew died 
#in the accident, making this one of the largest maritime disasters ever outside of war. The ship carried
#a wide range of passengers of all ages and both genders, from luxury travelers in first-class to immigrants
#in the lower classes. However, not all passengers were equally likely to survive the accident. We use real data
#about a selection of 891 passengers to learn who was on the Titanic and which passengers were more likely to survive.

options(digits = 3)    
library(tidyverse)
install.packages("titanic")
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
  #Q2: Make density plots of age grouped by sex. Try experimenting with combinations of faceting, alpha blending, stacking and using variable counts on the y-axis to answer which the following are true.
      #Females and males had the same general shape of age distribution. (true)
      #The age distribution was bimodal, with one mode around 25 years of age and a second smaller mode around 5 years of age. (true)
      #There were more females than males.  (false)
      #The count of males of age 40 was higher than the count of females of age 40. (true)
      #The proportion of males age 18-35 was higher than the proportion of females age 18-35. (true)
      #The proportion of females under age 17 was higher than the proportion of males under age 17. (true)
      #The oldest passengers were female  (false)

     #My graph (4/5 correct answers selected)
titanic %>%
  filter(!is.na(Age), !is.na(Sex)) %>%
  ggplot(aes(Age, y = ..count.. , fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack") +
  facet_grid(Sex ~.)

     #Suggested graphs to answer questions

titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2) +
  facet_grid(Sex ~ .)
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack")
titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2)

  #Q3: Use geom_qq to make a QQ-plot of passenger age and add an identity line with geom_abline. Filter out any individuals with an age of NA first. Use the following object as the dparams argument in geom_qq:
      #params <- titanic %>%
      # filter(!is.na(Age)) %>%
      # summarize(mean = mean(Age), sd = sd(Age))

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
titanic %>%
  filter (!is.na(Age)) %>%
  ggplot(aes(sample = Age)) + 
  geom_qq(dparams = params) +
  geom_abline()

  #Q4: To answer the following true/false questions, make barplots of the Survived and Sex variables using geom_bar. Try plotting one variable and filling by the other variable. You may want to try the default plot,
      #then try adding position = position_dodge() to geom_bar to make separate bars for each group.
      #Less than half of passengers survived. (true)
      #Most of the survivors were female. (true)
      #Most of the males survived.  (false)
      #Most of the females survived.  (false)

    #My graphs (2/3 correct answers selected)
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar()

titanic %>%
 ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())

titanic %>%
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar(position = position_dodge())

  #Q5: Make a density plot of age filled by survival status. Change the y-axis to count and set alpha = 0.2
      #Which age group is the only group more likely to survive than die? correctly answered 0-8
      #Which age group had the most deaths? correctly answered 18-30
      #Which age group had the highest proportion of deaths? correctly answered 70-80

titanic %>%
  ggplot(aes(Age, fill = Survived)) +
  geom_density(alpha = 0.2)

  #Q6: Filter the data to remove individuals who paid a fare of 0. Make a boxplot of fare grouped by survival status. Try a log2 transformation of fares. Add the data points with jitter and alpha blending. Answer which of the following are true
      #Passengers who survived generally payed higher fares than those who did not survive. (true)
      #The interquartile range for fares was smaller for passengers who survived. (false)
      #The median fare was lower for passengers who did not survive.  (true)
      #Only one individual paid a fare around $500. That individual survived. (false)
      #Most individuals who paid a fare around $8 did not survive.  (true)
    
    #My graph (3/3 answered correctly)
titanic %>%
  filter(Fare >0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_jitter(width = 0.1, alpha = 0.2)

  #Q7:The Pclass variable corresponds to the passenger class. Make three barplots. 
      #For the first, make a basic barplot of passenger class filled by survival.   
      #For the second, make the same barplot but use the argument position = position_fill() to show relative proportions in each group instead of counts. 
      #For the third, make a barplot of survival filled by passenger class using position = position_fill(). Answer which of the following are true

      #There were more third class passengers than passengers in the first two classes combined.  (true)
      #There were the fewest passengers in first class, second-most passengers in second class, and most passengers in third class. (false)
      #Survival proportion was highest for first class passengers, followed by second class. Third-class had the lowest survival proportion.  (true)
      #Most passengers in first class survived. Most passengers in other classes did not survive. (true)
      #The majority of survivors were from first class. (Majority means over 50%.)  (false)
      #The majority of those who did not survive were from third class. (true)

    #My graphs (3/3 answered correctly)
titanic %>%
  filter(!is.na(Pclass)) %>%
  ggplot(aes(Pclass), fill = Survived) +
  geom_bar()

titanic %>%
  filter(!is.na(Pclass)) %>%
  ggplot(aes(Pclass), fill = Survived) +
  geom_bar(aes(fill = Survived), position = "fill")

titanic %>%
  filter(!is.na(Pclass)) %>%
  ggplot(aes(Survived), fill = Pclass) +
  geom_bar(aes(fill = Pclass),position = "fill")

  #Q8: Create a grid of density plots for age, filled by survival status, with count on the y-axis, faceted by sex and passenger class. Which of the following are true
      #The largest group of passengers was third-class males. (true)
      #The age distribution is the same across passenger classes. (false)
      #The gender distribution is the same across passenger classes.  (false)
      #Most first-class and second-class females survived.  (true)
      #Almost all second-class males did not survive, with the exception of children. (true)

    #My graph (3/3 answered correctly)
titanic %>%
  filter(!is.na(Age), !is.na(Survived), !is.na(Sex), !is.na(Pclass)) %>%
  ggplot(aes(Age, y = ..count..)) +
  geom_density(aes(fill = Survived), position = "stack") +
  facet_grid(Sex ~ Pclass)



