###### Section 1: Overview ######

###Section 1.1: Intro to data visualisation, data types
library(dslabs)
data(heights)

  #1: Use the unique and length functions to determine how many unique heights were reported.
length(unique(heights$height))
  #2: Use the table function to compute the frequencies of each unique height value. Save the results in an object called tab
tab<-table(heights$height)
  #3: Use logicals and the function sum to count the number of times a value is reported only once
sum(tab ==1)

###Section 1.2: Intro to distributions, normal distribution
library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]

  #1: What proportion of the data is between 69 and 72 inches (taller than 69 but shorter or equal to 72)? A proportion is between 0 and 1.
mean(x>69)-mean(x>72)
  #2: Use the normal approximation to estimate the proportion of the data that is between 69 and 72 inches.
    #Given a normal distribution with a mean mu and standard deviation sigma,
    #you can calculate the proportion of observations less than or equal to a certain value with pnorm(value, mu, sigma)
avg <- mean(x)
stdev <- sd(x)
mean(pnorm(72, avg, stdev))-mean(pnorm(69,avg,stdev))
  #3: Use normal approximation to estimate the proportion of heights between 79 and 81 inches and save it in an object called approx. Report how many times bigger the actual proportion is compared to the approximation
exact <- mean(x > 79 & x <= 81)
approx<- mean(pnorm(81, mean(x), sd(x)))-mean(pnorm(79,mean(x), sd(x)))
exact/approx
  #4: Using the normal approximation, estimate the proportion of adult men that are taller than 7 feet (aka 84 inches). Assume that the distribution of adult men in the world as normally distributed with an average of 69 inches and a standard deviation of 3 inches
    #Use the pnorm function. Note that pnorm finds the proportion less than or equal to a given value, but you are asked to find the proportion greater than that value.
1-mean(pnorm(84,69,3))
  #5: We know that there are about 1 billion men between the ages of 18 and 40 in the world, the age range for the NBA. Use the normal distribution to estimate how many of these 1 billion men between these ages are at least seven feet tall. Round to the nearest integer.
p<-1-mean(pnorm(84,69,3))
round (p*10^9, digits = 0)
  #6: There are about 10 National Basketball Association (NBA) players that are 7 feet tall or higher. Calculate the proportion of the world's 18 to 40 year old seven footers that are in the NBA.
p<-1-mean(pnorm(84,69,3))
N<-round (p*10^9, digits = 0)
10/N
  #7:Repeat the calculations performed in the previous question for Lebron James' height: 6 feet 8 inches (aka 80 inches). There are about 150 players, instead of 10, that are at least that tall in the NBA.
p <- 1 - pnorm((6*12+8), 69, 3)
N <- round(p * 10^9)
150/N

###Section 1.3: Quantiles, percentiles, and boxplots
library(dslabs)
data(heights)

  #1: Define two varaibles male and female containing their respective heights. Report the length of each variable
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
length(male)
length(female)
  #2: Create two five row vectors showing the 10th, 30th, 50th, 70th, and 90th percentiles for the heights of each sex called these vectors female_percentiles and male_percentiles. Then create a data frame called df with these two vectors as columns. The column names should be female and male and should appear in that order
female_percentiles<-quantile(female, seq(0.10, 0.90, 0.20))
male_percentiles<-quantile(male, seq(0.10, 0.90, 0.20))
df<-data.frame(female = female_percentiles, male = male_percentiles)

###Section 1.4: Robust summaries with outliers
install.packages("HistData")
library(HistData)
data(Galton)
  #1:Compute the average and median of these data (use only the variable for children)
x <- Galton$child
mean(x)
median(x)
  #2:Compute the standard deviation and the median absolute deviation of these data
sd(x)
mad(x)
  #3: suppose that Galton made a mistake when entering the first value, forgetting to use the decimal point.
    #you can imitate this error by typing the following
x <- Galton$child
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
  #Report how many inches the average grows after this mistake
mean(x_with_error)-mean(x)
  #4: Report how many inches the SD grows after this mistake
sd(x_with_error)-sd(x)
  #5: Report how many inches the median grows after the mistake
median(x_with_error)-median(x)
  #6: Report how many inches the MAD grows after the mistake
mad(x_with_error)-mad(x)
  #8: Write a function called error_avg that takes a value k and returns the average of the vector x after the first entry changed to k. Show the results for k=10000 and k=-10000
error_avg <- function(k){
  x[1]<-k
  mean(x)
}
error_avg(10000)
error_avg(-10000)
