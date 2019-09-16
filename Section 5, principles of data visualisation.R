###### Section 5: Data Visualisation Principles ######

### Section 5.2: Data Visualisation Principles Part 2
library(dplyr)
library(ggplot2)
library(dslabs)

  #1: Redefine the state object in the code below so that the levels are re-ordered by rate. Print the new object state and its levels so you can see that the vector is now re-ordered by the levels.
dat <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% mutate(rate = count / population * 10000 * 52 / weeks_reporting)
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)

state <- reorder(dat$state,rate)  #MY ANSWER
levels(state)

state <- reorder(state, rate)     #COURSE ANSWER
print(state)
levels(state)
  #2: Add a single line of code to the definition of the dat table in the previous exercise that uses mutate to reorder the states by the rate variable.
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate (state = reorder(state,rate))  #LINE ADDED TO COMPLETE EXERCISE
dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()
  #4: Use the US murders data to make a box plot of murder rates by region, showing all points and ordered by their median murder
data("murders")
murders %>% mutate(rate = total/population*100000) %>%
  mutate(region = reorder (region, rate, FUN = median)) %>%
  ggplot(aes(region, rate)) +
  geom_boxplot()+
  geom_point() +
  geom_jitter(width = 0.1, alpha = 0.2)
  
###Section 5.3: Data Visualisation Principles Part 3
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

  #1: Modify the tile plot below to look at smallpox cases instead. Exclude years in which cases were reported in fewer than 10 weeks from the plot
      # the_disease = "Measles"
      #dat <- us_contagious_diseases %>% 
      #filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>% 
      #mutate(rate = count / population * 10000) %>% 
      #mutate(state = reorder(state, rate))

      #dat %>% ggplot(aes(year, state, fill = rate)) + 
      #geom_tile(color = "grey50") + 
      #scale_x_continuous(expand=c(0,0)) + 
      #scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
      #theme_minimal() + theme(panel.grid = element_blank()) + ggtitle(the_disease) + 
      #ylab("") + xlab("")

the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease, weeks_reporting >= 10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")
  
  #2: Modify the plot below to look at smallpox cases instead.Once again, restrict the plot to years in which cases were reported in at least 10 weeks
      #the_disease = "Measles"
      #dat <- us_contagious_diseases %>%
      #filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>%
      #mutate(rate = count / population * 10000) %>%
      #mutate(state = reorder(state, rate))

      #avg <- us_contagious_diseases %>%
      # filter(disease==the_disease) %>% group_by(year) %>%
      #summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

      #dat %>% ggplot() +
      # geom_line(aes(year, rate, group = state),  color = "grey50", 
      #          show.legend = FALSE, alpha = 0.2, size = 1) +
    #geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
    #scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
    #ggtitle("Cases per 10,000 by state") + 
    #xlab("") + 
    #ylab("") +
    #geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
    #geom_vline(xintercept=1963, col = "blue")

the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
filter(!state%in%c("Hawaii","Alaska") & disease == the_disease, weeks_reporting >=10) %>%
mutate(rate = count / population * 10000) %>%
mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
filter(disease==the_disease) %>% group_by(year) %>%
summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
 geom_line(aes(year, rate, group = state),  color = "grey50", 
          show.legend = FALSE, alpha = 0.2, size = 1) +
geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
ggtitle("Cases per 10,000 by state") + 
xlab("") + 
ylab("") +
geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
geom_vline(xintercept=1963, col = "blue")

  #3: Modify the sample code to produce a time series plot for the state of California showing rates for all diseases. Include only years with 10 or more weeks reporting.
     #us_contagious_diseases %>% filter(state=="California") %>% 
     #group_by(year, disease) %>%
     #summarize(rate = sum(count)/sum(population)*10000) %>%
     #ggplot(aes(year, rate)) + 
     #geom_line()

us_contagious_diseases %>% 
  filter(state=="California", weeks_reporting >= 10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) + 
  geom_line()

  #4: Make a time series plot for the rates of all diseases in the US.

us_contagious_diseases %>% 
  filter(!is.na(population)) %>%
  group_by(year,disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) + 
  geom_line()




