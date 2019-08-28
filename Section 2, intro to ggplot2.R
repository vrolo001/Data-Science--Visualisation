###### Section 2: Intro to ggplot2 ######

###Section 2.2: Customising plots, intro to ggplot2
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)

  #1:Take p<-ggplot(murders), what is the class of the object p?
p <- ggplot(murders)
class(p)
  #3:Using the pipe %>%, create an object p associated with the heights dataset instead of with the murders dataset as in previous exercises
p <- heights %>% ggplot() 
  #5: Fill out the sample code with the correct variable names to plot total murders versus population size
  #sample code : murders %>% ggplot(aes(x = , y = )) + geom_point()
murders %>% 
  ggplot(aes(x = population , y = total )) +
  geom_point()
  #6: Remake the plot but without the x and y arguments and flip the axes so that total is on the x-axis and population is on the y-axis
murders %>% 
  ggplot(aes(total, population )) +
  geom_point()
  #8:Rewrite the code from the previous exercise to:
  #   add a label aesthetic to aes equal to the state abbreviation
  #   use geom_label instead of geom_point
murders %>% 
  ggplot(aes(population, total,label = abb)) +
  geom_label()
  #10: Rewrite the code from the previous exercise to make the labels blue
murders %>% 
  ggplot(aes(population, total,label= abb)) +
  geom_label(color="blue")
  #12: Rewrite the code above to make the label color correspond to the state's region
p<-murders %>% 
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()
  #13: Change both axes to be in the log scale to account for the fact that the population distribution is skewed
murders %>% 
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label() +
  scale_x_log10() +
  scale_y_log10()
  #14: Edit the code above to add the title "Gun murder data" to the plot
p + scale_x_log10() + scale_y_log10() + ggtitle("Gun murder data")
  #16: Create a ggplot object called p using the pipe to assign the heights data to a ggplot object. Assign height to the x values through the aes function
p <- heights %>%
  ggplot(aes(height)) 
  #17: Add a layer to the object p (created in the previous exercise) using the geom_histogram function to make the histogram
p + geom_histogram()
  #18: Use the binwidth argument to change the histogram made in the previous exercise to use bins of size 1 inch
p + geom_histogram(binwidth = 1)
  #19: Add the appropriate layer to create a smooth density plot of heights
heights %>% 
  ggplot(aes(height)) +
  geom_density()
  #20: Create separate smooth density plots for males and females by defining group by sex
heights %>%
  ggplot(aes(height, group = sex)) +
  geom_density()
  #21: Change the density plots from the previous exercise to add color
heights %>%
  ggplot(aes(height, color = sex)) +
  geom_density()
  #22: If we use the fill argument, the area under the curves get colored. However the second density is drawn over the other.
  #   Set the parameter to 0.2 in the geom_density function to change this.
heights %>% 
  ggplot(aes(height, fill = sex)) + 
  geom_density(alpha = 0.2) 


