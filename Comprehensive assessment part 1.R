###### Comprehensive Assessment Part 1: Properties of Stars ######
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
data(stars)
options(digits = 3)
update.packages()

  #Q1: What is the mean magnitude of stars? What is the SD of magnitude?
mean(stars$magnitude)
sd(stars$magnitude)
   
  #Q2: Make a density plot of the magnitude. How many peaks are there in the data?   (2 peaks)
stars %>%
  ggplot(aes(magnitude)) +
  geom_density()

  #Q3: Examine the distribution of star temperature. Which of these statements best characterizes the temperature distribution?
      #The majority of stars have a high temperature.
      #The majority of stars have a low temperature.    (correct)
      #The temperature distribution is normal.
      #There are equal numbers of stars across the temperature range.

stars %>%
  ggplot(aes(temp)) +
  geom_density()

  #Q4: Make a scatter plot of the data with temperature on the x-axis and magnitude on the y-axis and examine the relationship between the variables. 
      #Recall that lower magnitude means a more luminous (brighter) star. Fill in the blank below
      #Most stars follow a _______ trend. These are calles main sequence stars.   (answer: decreasing exponential)

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point()

  #Q5: Scientists do not always follow straight conventions when making plots, and astronomers usually transform values of star luminosity and temperature before plotting. 
      #Flip the y-axis so that lower values of magnitude are at the top of the axis (recall that more luminous stars have lower magnitude) using scale_y_reverse. 
      #Take the log base 10 of temperature and then also flip the x-axis. Fill in the blanks in the statements below to describe the resulting plot:
  
stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point() + 
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse()

   #The brighest, highest temperature stars are in the ______________ corner of the plot.   (upper left)
   #For main sequence stars, hotter stars have ______ luminosity.   (higher)

  #Q6: Some of the more rare stars are classified as "old" and "evolved" stars. These stars tend to be hotter stars, but also have low luminosity, and are known as white dwarfs.
      #How many white dwarfs are there in our sample?   (4, in the lower left part of the plot)

  #Q7: Consider stars which are not part of the Main Group but are not old/evolved (white dwarf) stars. These stars must also be unique in certain ways and are known as giants.
      #Use the plot from Question 5 to estimate the average temperature of a giant.   (Answer = 5000K)

    mean(stars$temp[stars$type == "G"])   #alternative way to solve question more precisely
 
  #Q8:We can now identify whether specific stars are main sequence stars, red giants or white dwarfs. Add text labels to the plot to answer these questions.
      #The least lumninous star in the sample with a surface temperature over 5000K is _________ (van Maanen's Star)
      #The two stars with lowest temperature and highest luminosity are known as supergiants. The two supergiants in this dataset are ____________ (Betelgeuse and Antares)
      #The Sun is a _____________ (main sequence star)
    
  ### MY SOLUTION FOR ALL 3 QUESTIONS; creating a vector with the names of the stars given in the multiple choice questions and oonly plotting said stars
      #to avoid cluttering###
    
library(ggrepel)
Stars <- c("Antares", "Castor", "Mirfak", "Polaris", "vanMaanen'sStar",
           "Rigel", "Deneb", "*SiriusB", "Alnilam", "Alnitam", "Betelgeuse", "Wolf359", "G51-I5", "Sun" )
        
stars %>%
  filter(star %in% Stars) %>%
  ggplot(aes(temp, magnitude)) +
  geom_point() + 
  scale_y_reverse() +
  scale_x_log10() +
  scale_x_reverse() +
  geom_text(aes(temp, magnitude, label = star)) +
  geom_text_repel(aes(temp, magnitude, label = star))

  ### CODE FROM COURSE ANSWERS ###
stars %>%
  ggplot(aes(log10(temp), magnitude)) +
  geom_point() +
  geom_text(aes(label = star)) +
  scale_x_reverse() +
  scale_y_reverse()

  #Q9: Remove the text labels and colour the points by star type. This classification describes the properties of the star's spectrum, the amount of light produced at various wavelengths.
      #Which star type has the lowest temperature?  (M)
      #Which star type has the highest temperature? (O)
      #The Sun is classified as a G-type star. Is the most luminous G-type star in this dataset also the hottest? (no)

stars %>%
  ggplot(aes(log10(temp), magnitude, color = type)) +
  geom_point() +
  scale_x_reverse() +
  scale_y_reverse()

  
