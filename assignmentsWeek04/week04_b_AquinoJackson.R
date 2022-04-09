# Assignment: data transformations
# Name: Aquino, Jackson
# Date: 2022-04-09

#For this exercise, you need to start practicing some data transformation steps - which will carry into next week, 
#as you learn some additional methods.  For this week, using either dataset (or one of your own - although 
#I will let you know ahead of time that the Housing dataset is used for a later assignment, so not a bad idea 
#for you to get more comfortable with now!), perform the following data transformations:
setwd("C:/users/jackson_aquino/dsc520")
library(readxl)
library(ggplot2)
library(dplyr)
housingData <- read_xlsx("data/week-6-housing.xlsx")
ncol(housingData)
nrow(housingData)
str(housingData)
head(housingData)
unique(housingData$ctyname)

#Use the apply function on a variable in your dataset
#   creating a data frame with only the columns about bathrooms
toilets_df <- data.frame(housingData$bath_full_count,housingData$bath_half_count,housingData$bath_3qtr_count)
#   totaling the number of toilets in the house using the apply function
toilets_count <- apply(toilets, 1, sum)


#Use the aggregate function on a variable in your dataset
#    Using this to see the average price of a home grouped by the number of bedrooms
aggregate(housingData$`Sale Price` ~ housingData$bedrooms,housingData,mean)

#Use the plyr function on a variable in your dataset - more specifically, I want to see you split some data, 
#perform a modification to the data, and then bring it back together
filter(housingData, housingData$bath_half_count > 0) 
filter(housingData, housingData$bath_half_count == 0)
newdf <- add_row(filter(housingData, housingData$bath_half_count > 0), filter(housingData, housingData$bath_half_count == 0))
newdf == housingData


#Check distributions of the data
ggplot(housingData) + geom_point(aes(x=housingData$sq_ft_lot,y=housingData$`Sale Price`,col=housingData$ctyname)) + geom_smooth(aes(x=housingData$sq_ft_lot,y=housingData$`Sale Price`))
ggplot(housingData) + geom_point(aes(x=housingData$square_feet_total_living,y=housingData$`Sale Price`)) + geom_smooth(aes(x=housingData$square_feet_total_living,y=housingData$`Sale Price`))
toilets_df$toiletCount <- toilets_count
ggplot(toilets_df) + geom_histogram(aes(x=toiletCount),binwidth = 1)


#Identify if there are any outliers
#  I chose to use the Z-scores method to see if there are outliers on the number of total toilets
zScoresToilets <- c()
meanToiletsCount <- mean(toilets_count)
sdToiletsCount <- sd(toilets_count)
ToiletOutliersCount = 0
for (toiletCount in toilets_count) {
  thisZScore <- (toiletCount - meanToiletsCount)/sdToiletsCount
  zScoresToilets <- append(zScoresToilets,thisZScore)
  if (thisZScore > 3 || thisZScore < -3) {
    ToiletOutliersCount <- ToiletOutliersCount + 1
  }
}

ToiletOutliersCount
ToiletOutliersCount/nrow(housingData)
# there are 111 (0.8%) outliers


#Create at least 2 new variables
housingData$toilets_count <- toilets_count
housingData$toilets_count_zscores <- zScoresToilets
str(housingData)