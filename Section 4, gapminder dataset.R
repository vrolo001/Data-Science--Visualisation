###### Section 4: Gapminder ######

###Section 4.2: Exploring the Gapminder Dataset
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

  #1: Using ggplot and the points layer, create a scatter plot of life expectancy versus fertility for the African continent in 2012
gapminder %>%
  filter(continent == "Africa", year == 2012) %>%
  ggplot(aes(fertility, life_expectancy)) +
    geom_point()
  #2: Remake the plot from the previous exercises but this time use color to distinguish the different regions of Africa.
gapminder %>%
  filter(continent == "Africa", year == 2012) %>%
  ggplot(aes(fertility, life_expectancy, color = region )) +
  geom_point()
  #3: Create a table showing the country and region for the African countries (use select) that in 2012 had fertility rates of 3 or less and life expectancies of at least 70.
      #Assign your result to a data frame called df
df<- gapminder%>%
  filter (continent == "Africa", year == 2012, fertility <= 3, life_expectancy >= 70)  %>%
  select(country, region)
  #4: Use filter to create a table tab with data for the years from 1960 to 2010 in Vietnam and the United States.
years<- c(1960:2010)
countries<- c("Vietnam", "United States")
tab<- gapminder%>%
  filter(year %in% years, country %in% countries)
  #5: Plot life expectancy vs year for Vietnam and the United States and save the plot as p.
p<-tab %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
  #6: Use a single line of code to create a time series plot from 1960 to 2010 of life expectancy vs year for Cambodia
gapminder %>%
  filter(country == "Cambodia", year %in% years) %>%
  ggplot(aes(year, life_expectancy)) +
  geom_line()
  #7: Create a dollars_per_day variable for African countries, which is defined as gdp/population/365. Remove NA values.
     #Save the dataset as daydollars
daydollars<-gapminder %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa", year == 2010, !is.na(dollars_per_day))
  #8: Create a smooth density plot of dollars per day from daydollars using log(base2)scale for the x axis
daydollars %>%
  ggplot(aes(x = dollars_per_day)) +
  geom_density() +
  scale_x_continuous(trans = "log2")
  #9: Create the dollars_per_day variable but for African countries in the years 1970 and 2010 this time.
      #Create a smooth density plot of dollars per day for 1970 and 2010 using a log (base 2) scale for the x axis.
gapminder %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa", year %in%  c(1970, 2010), !is.na(dollars_per_day)) %>%
  ggplot(aes(x = dollars_per_day)) +
  geom_density() +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)
  #10:  # Edit the code from Exercise 9 to show a stacked density plot of each region in Africa
gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(dollars_per_day)) %>%
  ggplot(aes(dollars_per_day, fill = region)) + 
  geom_density(bw = 0.5, position = "stack") + 
  scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ .)
  #11: Generate dollars_per_day using mutate and filter for the year 2010 for African countries.Store the mutated dataset in gapminder_Africa_2010.
       #Make a scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.
gapminder_Africa_2010 <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365) %>%
  filter (continent == "Africa" & year == 2010, !is.na(dollars_per_day))
gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
  geom_point()
  #12: Transform the x axis of the plot in the previous exercise to be in the log (base 2) scale
gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
  geom_point() + scale_x_continuous(trans = "log2")
  #13: Remake the plot from exercise 12 with country names instead of points to identify which countries are which.
gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality, color = region)) +                   #MY ANSWER
  geom_point() + scale_x_continuous(trans = "log2") + 
  geom_text(aes(dollars_per_day, infant_mortality, label = country))

gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +  #COURSE ANSWER
  geom_point() + scale_x_continuous(trans = "log2") + 
  geom_text()
  #14: Look at changes in the infant mortality and dollars per day patterns in African countries between 1970 and 2010 using the gapminder data. Use facet_grid to show different plots for 1970 and 2010 for the previous exercise. Align the plots vertically
gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970, 2010), !is.na(dollars_per_day), !is.na(infant_mortality)) %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country, !is.na(infant_mortality))) +  
  geom_point() + scale_x_continuous(trans = "log2") + 
  geom_text() +
  facet_grid(year ~.)
