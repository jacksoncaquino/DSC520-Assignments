# Assignment: ASSIGNMENT 7
# Name: Aquino, Jackson
# Date: 2022-05-12

## Set the working directory to the root of your DSC 520 directory
setwd("C:/users/jackson_aquino/DSC520/")

## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("data/r4ds/heights.csv")

# Fit a linear model
earn_lm <-  lm(earn ~ age + height + ed + race + sex, data=heights_df)

# View the summary of your model
summary(earn_lm)

predicted_df <- data.frame(
  earn = predict(earn_lm),
  ed=heights_df$ed, race=heights_df$race, height=heights_df$height,
  age=heights_df$age, sex=heights_df$sex
  )

## Compute deviation (i.e. residuals)
mean_earn <- heights_df$earn
## Corrected Sum of Squares Total
sst <- sum((mean_earn - heights_df$earn)^2)
## Corrected Sum of Squares for Model
ssm <- sum((mean_earn - predicted_df$earn)^2)
## Residuals
residuals <- heights_df$earn - predicted_df$earn
## Sum of Squares for Error
sse <- sum(residuals^2)
## R Squared
r_squared <- ssm/sst

## Number of observations
n <- nrow(heights_df)
## Number of regression parameters
p <- 8
## Corrected Degrees of Freedom for Model 
dfm <- p - 1
## Degrees of Freedom for Error
dfe <- n - p
## Corrected Degrees of Freedom Total
dft <- n - 1

## Mean of Squares for Model
msm <- ssm / dfm
## Mean of Squares for Error
mse <- sse / dfe
## Mean of Squares Total
mst <- sst/dft
## F Statistic
f_score <- msm/mse

## Adjusted R Squared
adjusted_r_squared <- 1 - (1-r_squared)*(n-1)/(n-p)