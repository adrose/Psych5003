## Load library(s)
library(foreign) ## Will be used to load .sav file
library(ggplot2) ## Will be used for plotting

## Read the data
all.data <- read.spss("./salary.sav", to.data.frame=T)

## Problem 1a
# Compute the mean, median, and variance of salary
mean.salary <- mean(all.dat$salary)
median.salary <- median(all.dat$salary)
var.salary <- var(all.dat$salary)

## Problem 1b
# Create a histogram, boxplot & qqplot of salary
hist(all.dat$salary)
# The data appear to be pseudo-normal from the histogram
x11()
boxplot(all.dat$salary)
# The boxplot suggests there is one lower bound outlier
x11()
qqnorm(all.dat$salary)
# The qq plot also suggests relative normalcy, as the majority of the points
# follow the normalcy line

## Problem 1c
# Create a scatter plot between pubs & salary
out.scat.one <- ggplot(data=all.dat, aes(x=pubs, y=salary)) +
  geom_point() +
  geom_smooth(method='lm')

## Problem 1d
# Now create the same scatter plot by gender
all.dat$sex <- factor(all.dat$sex)
out.scat.two <-  ggplot(data=all.dat, aes(x=pubs, y=salary, group=sex, color=sex)) +
  geom_point() +
  geom_smooth(method='lm')


## Problem 2
## First Create a PDF w/ a mean of 25 and a standard deviation of 5
x.seq <- seq(5, 45, by=.01)
tmp.dist <- dnorm(x=x.seq, mean=25, sd=5)

## Problem 2a
# Now find the percentile value for 22
quantile_value <- pnorm(q=22, mean=25, sd=5)
## Now compute the percent of individuals greater than this value
perc_greater <- 1 - quantile_value

## Probelm 2b
# Now calculate the area between 24 & 28
quantile_value_lower <- pnorm(q=24, mean=25, sd=5)
quantile_value_upper <- pnorm(q=28, mean=25, sd=5)
# Now calculate the difference between the upper and lower bound
percent_in_between <- quantile_value_upper - quantile_value_lower

## Problem 2c
# Now calculate the z score of the 50th percentile
fifty_z_score <- qnorm(.5)

## Problem 2d
# Which z scores form the upper and lower 50% of cases
# Here we need to identify the z scores for the 25th and 75th percntiles
twenty_z_score <- qnorm(.25)
seventy_z_score <- qnorm(.75)
# Now I need to go from the z score to the raw value
# To calculate this value I am going to multiply the standadrd deviation by the z score, and then add the mean
twenty_raw_value <- 25 + (twenty_z_score*5)
seventy_raw_value <- 25 + (seventy_z_score*5)

## Problem 2e
# Which z scores cutoff a total of 5% of all cases
# We need to identify the z scores for the 2.5 percentile and also the 97.5 percentile
two_z_score <- qnorm(.025)
nintey_z_score <- qnorm(.975)
## Now calculate the raw values
two_raw_value <- 25 + (two_z_score * 5)
nintey_raw_value <- 25 + (nintey_z_score * 5)
