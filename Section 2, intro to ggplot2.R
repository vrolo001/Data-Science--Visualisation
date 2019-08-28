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
p<-ggplot(heights)