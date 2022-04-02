# Assignment: American Community Survey Exercise
# Name: Aquino, Jackson
# Date: 2022-04-02

## a.	For this exercise, you will use the following dataset, 2014 American Community Survey. This data is maintained by the US Census Bureau and are designed to show how communities are changing. Through asking questions of a sample of the population, it produces national data on more than 35 categories of information, such as education, income, housing, and employment. For this assignment, you will need to load and activate the ggplot2 package. For this deliverable, you should provide the following:
library(ggplot2)
theme_set(theme_minimal())

setwd("C:/Users/jackson_aquino/dsc520")

acs_df <- read.csv("data/acs-14-1yr-s0201.csv")

## i.	What are the elements in your data (including the categories and data types)?
##      see below on the results of the str output

## ii.	Please provide the output from the following functions: str(); nrow(); ncol()
str(acs_df)
nrow(acs_df)
ncol(acs_df)

## iii.	Create a Histogram of the HSDegree variable using the ggplot2 package.
ggplot(acs_df) + geom_histogram(aes(x=HSDegree))

##   1.	Set a bin size for the Histogram.
ggplot(acs_df) + geom_histogram(aes(x=HSDegree),binwidth = 0.1)

## 2.	Include a Title and appropriate X/Y axis labels on your Histogram Plot.
ggplot(acs_df) + geom_histogram(aes(x=HSDegree),binwidth = 1) + xlab("Percentage of the population with a high school degree") + ylab("Count")

## iv.	Answer the following questions based on the Histogram produced:
## 1.	Based on what you see in this histogram, is the data distribution unimodal?
##       Looking at the most granular version of my histogram, with a binwidth of 0.1, I see 6 modes, so it's not unimodal
## 2.	Is it approximately symmetrical?
##       No, looking at the version with binwidth of 1, it does not look symmetrical.
## 3.	Is it approximately bell-shaped?
##       Yes, looking at the version with binwidth of 1, it looks bell shaped.
## 4.	Is it approximately normal?
##       No, not really
## 5.	If not normal, is the distribution skewed? If so, in which direction?
##       Yes, it is negatively skewed
## 6.	Include a normal curve to the Histogram that you plotted.
ggplot(acs_df) + geom_histogram(aes(x=HSDegree,y=.6*..density..),binwidth = 1)+ stat_function(fun = dnorm, col="blue", args = list(mean=mean(acs_df$HSDegree),sd=sqrt(var(acs_df$HSDegree))))+ xlab("Percentage of the population with a high school degree")

## 7.	Explain whether a normal distribution can accurately be used as a model for this data.
##       It looks like a pretty good fit to me.

##  v.	Create a Probability Plot of the HSDegree variable.
ggplot(acs_df) + stat_qq(aes(sample=acs_df$HSDegree),col="blue") + stat_qq_line(aes(sample=acs_df$HSDegree),col="blue")

## vi.	Answer the following questions based on the Probability Plot:
##  1.	Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know.
##             There are too many points that are far from the line, so it's not a normal distribution.
##  2.	If not normal, is the distribution skewed? If so, in which direction? Explain how you know.
##             Yes, it is negatively skewed, as the dots on the negative side are farther from the line than the ones that are on the positive side


##      vii.	Now that you have looked at this data visually for normality, you will now quantify normality with numbers using the stat.desc() function. Include a screen capture of the results produced.
library("pastecs")
stat.desc(acs_df$HSDegree, norm=TRUE)

##Results:
##nbr.val      nbr.null        nbr.na           min           max         range           sum        median 
##1.360000e+02  0.000000e+00  0.000000e+00  6.220000e+01  9.550000e+01  3.330000e+01  1.191800e+04  8.870000e+01 
##mean       SE.mean  CI.mean.0.95           var       std.dev      coef.var      skewness      skew.2SE 
##8.763235e+01  4.388598e-01  8.679296e-01  2.619332e+01  5.117941e+00  5.840241e-02 -1.674767e+00 -4.030254e+00 
##kurtosis      kurt.2SE    normtest.W    normtest.p 
##4.352856e+00  5.273885e+00  8.773635e-01  3.193634e-09 


##      viii.	In several sentences provide an explanation of the result produced for skew, kurtosis, and z-scores. In addition, explain how a change in the sample size may change your explanation?
##             Just like the image had showed before, the skewness is negative. The kurtosis I saw on the chart looked normal to me, but now looking at the number I can definitely say it's positive, so it's leptokurtic. An increase on the number of sample sizes could certainly influence these metrics, but how they would be impacted would depend a lot on how these new datapoints were. If they are all follow the same trend as the current observations, these metrics wouldn't be greatly impacted.
##             
