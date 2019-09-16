###### Comprehensive Assessment Part 2: Climate Change ######

#In these exercises, we examine the relationship between global temperature changes, greenhouse gases and human carbon emissions using time series of 
#actual atmospheric and ice core measurements from the National Oceanic and Atmospheric Administration (NOAA) and Carbon Dioxide Information Analysis Center (CDIAC).

library(tidyverse)
library(dslabs)
library(ggplot2)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

  #Q2: Inspect the difference in carbon emissions in temp_carbon from the first available year to the last available year.
      #What is the first year for which carbon emissions data are available?  (1751)
      #What is the last year for which carbon emissions data are available?   (2014)
      #How many times larger were carbon emissions in the last year relative to the first year? (3285)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  summarise(max(year), min(year))


temp_carbon$carbon_emissions[temp_carbon$year == 2014]/temp_carbon$carbon_emissions[temp_carbon$year == 1751]

   #course answer for third question
    carbon1 <- temp_carbon %>%
     filter(year == 1751) %>%
     .$carbon_emissions

    carbon2 <- temp_carbon %>%
      filter(year == 2014) %>%
      .$carbon_emissions

    carbon2/carbon1

  #Q3: Inspect the difference in temperature in temp_carbon from the first available year to the last available year.
      #What is the first year for which global temperature anomaly (temp_anomaly) data are available?
      #What is the last year for which global temperature anomaly data are available?
      #How many degrees Celsius has temperature increased over the date range? Compare the temperatures in the most recent year versus the oldest year.
  
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  summarise(max(year), min(year))

temp_carbon$temp_anomaly[temp_carbon$year == 2018] - temp_carbon$temp_anomaly[temp_carbon$year == 1880]

    #course answer for third question
    temp1 <- temp_carbon %>%
     filter(year == 1880) %>%
     .$temp_anomaly

    temp2 <- temp_carbon %>%
      filter(year == 2018) %>%
      .$temp_anomaly

    temp2 - temp1

  #Q4: Create a time series line plot of the temperature anomaly. Only include years where temperatures are reported. Save this plot to the object p.
      #Which of the options given on the assessment adds a blue line indicating the 20th century mean temperature?
p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year,temp_anomaly)) +
  geom_line()

p <- p + geom_hline(aes(yintercept = 0), col = "blue")

  #Q5: Change the y-axis label of p to be "Temperature anomaly (degrees C)". Add a title, "Temperature anomaly relative to 20th century mean, 1880-2018". 
      #Also add a text layer to the plot: the x-coordinate should be 2000, the y-coordinate should be 0.05, the text should be "20th century mean", and the text color should be blue.

p + ylab("Temperature anomaly (degrees C)") + 
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

  #Q6: Use the plot created in the last two exercises to answer the following questions.Answers within 5 years of the correct answer will be accepted.
      #When was the earliest year with a temperature above the 20th century mean? (~1940)
      #When was the last year with an average temperature below the 20th century mean?  (~1975)
      #In what year did the temperature anomaly exceed 0.5 degrees Celsius for the first time?  (~1998)
  
  #Q7: Add layers to the previous plot to include line graphs of the temperature anomaly in the ocean (ocean_anomaly) and on land (land_anomaly). Assign different colors to the lines. 
      #Compare the global temperature anomaly to the land temperature anomaly and ocean temperature anomaly.  
      #Which region has the largest 2018 temperature anomaly relative to the 20th century mean? (Land)
      #Which region has the largest change in temperature since 1880? (Land)
      #Which region has a temperature anomaly pattern that more closely matches the global pattern? (Ocean)

p + geom_line(aes(year, ocean_anomaly), col = "purple") +
  geom_line(aes(year, land_anomaly), col = "green")

  #Q8: Complete the code outline provided to make a line plot of concentration on the y-axis by year on the x-axis. Facet by gas, aligning the plots vertically so as to ease comparisons along the year axis.
      #Add a vertical line with an x-intercept at the year 1850, noting the unofficial start of the industrial revolution and widespread fossil fuel consumption. Note that the units for ch4 and n2o are ppb while the units for co2 are ppm.

library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas ~ . , scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")
  
  #Q9: Interpret the plot of greenhouse gases over time from the previous question. You will use each answer exactly once ch4, co2, n2o, all, none).
      #Which gas was stable at approximately 275 ppm/ppb until around 1850? (CO2)
      #Which gas more than doubled in concentration since 1850? (CH4)
      #Which gas decreased in concentration since 1850? (none)
      #Which gas had the smallest magnitude change since 1850?  (N2O)
      #Which gas increased exponentially in concentration after 1850? (all)

  #Q10: Make a time series line plot of carbon emissions (carbon_emissions) from the temp_carbon dataset. The y-axis is metric tons of carbon emitted per year.
      #Which of the following are true about the trend of carbon emissions?
      #Carbon emissions were essentially zero before 1850 and have increased exponentially since then. (True)
      #Carbon emissions are reaching a stable level.  (False)
      #Carbon emissions have increased every year on record.  (False)
      #Carbon emissions in 2014 were about 4 times as large as 1960 emissions.  (True)
      #Carbon emissions have doubled since the late 1970s.(True)
      #Carbon emissions change with the same trend as atmospheric greenhouse gas levels (co2, ch4, n2o) (True)
      
temp_carbon %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  geom_vline(aes(xintercept = 1850)) +
 
  #Q11: Use the historic_co2 data to make a line plot of co2 concentration over time (year), coloring by the measurement source (source). Save this plot as co2_time.
      #Which of the following are true about co2_time, the time series of co2 over the last 800,000 years?
      #Modern co2 levels are higher than at any point in the last 800,000 years.  (True, correctly answered)
      #There are natural cycles of co2 increase and decrease lasting 50,000-100,000 years per cycle. (True, correctly answered)
      #In most cases, it appears to take longer for co2 levels to decrease than to increase. (True, missed answer)
      #co2 concentration has been at least 200 ppm for the last 800,000 years.  (False, correctly answered)
  
co2_time <- historic_co2 %>%
  ggplot(aes(year, co2), col = source) +
  geom_line() +
  geom_hline(aes(yintercept = 200))

  #Q12: Use the co2_time plot. Change the limits as directed to investigate the rate of change in co2 over various periods with spikes in co2 concentration.
      #Change the x-axis limits to -800,000 and -775,000. About how many years did it take for co2 to rise from 200 ppmv to its peak near 275 ppmv? (10,000 years)

co2_time <- historic_co2 %>%
  ggplot(aes(year, co2), col = source) +
  geom_line() +
  geom_hline(aes(yintercept = 200)) +
  xlim(-800000, -775000)

      #Change the x-axis limits to -375,000 and -330,000. About how many years did it take for co2 to rise from the minimum of 180 ppm to its peak of 300 ppmv? (25,000 years)

co2_time <- historic_co2 %>%
  ggplot(aes(year, co2), col = source) +
  geom_line() +
  geom_hline(aes(yintercept = 180)) +
  xlim(-375000, -330000)

      #Change the x-axis limits to -140,000 and -120,000. About how many years did it take for co2 to rise from 200 ppmv to its peak near 280 ppmv? (9,000 years)

co2_time <- historic_co2 %>%
  ggplot(aes(year, co2), col = source) +
  geom_line() +
  geom_hline(aes(yintercept = 200)) +
  xlim(-140000, -120000)

      #Change the x-axis limits to -3000 and 2018 to investigate modern changes in co2. About how many years did it take for co2 to rise from its stable level around 275 ppmv to the current level of over 400 ppmv? (250 years)

co2_time <- historic_co2 %>%
  ggplot(aes(year, co2), col = source) +
  geom_line() +
  geom_hline(aes(yintercept = 275)) +
  xlim(-3000, 2018)
