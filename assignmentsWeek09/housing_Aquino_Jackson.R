# Assignment: Housing Data Assignment
# Name: Aquino, Jackson
# Date: 2022-05-12

library(readxl)

## Set the working directory to the root of your DSC 520 directory
setwd("C:/users/jackson_aquino/DSC520/")

## Loading the data
housing_df <- read_xlsx("data/week-7-housing.xlsx",1)

# Explain any transformations or modifications you made to the dataset
housing_df$bath_total <- housing_df$bath_full_count + 
  housing_df$bath_3qtr_count + housing_df$bath_half_count*.5 # creating a new field to contain the combined number of bathrooms

housing_df$CodeZip <- ifelse(housing_df$zip5==98052,"A",ifelse(housing_df$zip5==98053,"B",ifelse(housing_df$zip5==98074,"C","D"))) # Transforming the locations into a letter code so the regression makes dummy variables with this field instead of using the numbers
housing_df$CodeReason <- paste("R",housing_df$sale_reason,sep="")

# Create two variables; one that will contain the variables Sale Price and Square Foot of Lot (same variables used from previous assignment on simple regression) 
housing_lm_simple <- lm(`Sale Price` ~ sq_ft_lot, data = housing_df)

# and one that will contain Sale Price and several additional predictors of your choice. Explain the basis for your additional predictor selections.
## looking for correlations to avoid multi-collinearity, then I'll choose a few variables with the highest correlation with the outcome variable with the least correlation between themselves. 
library(Hmisc)
rcorr(as.matrix(housing_df[,c(2,3,4,8,13,14,15,19,20,22,25)]))
housing_lm <- lm(`Sale Price` ~ square_feet_total_living + year_built + building_grade, data = housing_df)
# I tried this version as well: housing_lm <- lm(`Sale Price` ~ bedrooms + bath_total + year_built + building_grade, data = housing_df)

# Execute a summary() function on two variables defined in the previous step to compare the model results. 
summary(housing_lm)
summary(housing_lm_simple)
#  What are the R2 and Adjusted R2 statistics? Explain what these results tell you about the overall model. 
#    The simle regression had an R2 of ~14%, while the multiple regression had an R2 of ~22%. This means that by adding these other factors we're now able to explain almost one fourth of the variation in price.
#  Did the inclusion of the additional predictors help explain any large variations found in Sale Price?
#    It did, but when I tried a simple regression with only square_feet_total_living, I got an R-squared of 20%, which is not too far from the 22% when I used 4 variables

# Considering the parameters of the multiple regression model you have created. What are the standardized betas for each parameter and what do the values indicate?
library(lm.beta)  
lm.beta(housing_lm)
# These are the standardized betas for each parameter
# square_feet_total_living: 0.35829506 
# year_built: 0.10116801
# building_grade: 0.08761888
# By standardizing them, we can more easily compare the impact of each of these variables on the outcome variable, as they are now measured in stardard deviations,
#    and not in square feet or other measuring units. This way we can see that the factor that most influenced the Sale price was size of the home.
#    We can also see that locations B and D decrease the value of a home, while location C increases it even more than a variation of one standard deviation on the year that the home was built.

# Calculate the confidence intervals for the parameters in your model and explain what the results indicate.
confint(housing_lm)
#                                2.5 %        97.5 %
# (Intercept)              -5465509.0300 -3960573.1263
# square_feet_total_living      137.0203      155.7362
# year_built                   1990.9371     2760.5182
# building_grade              23771.3160    41084.3554
# These numbers indicate by how much these predictors influence the outcome variable. For example, looking at square_feet_total_living, we can see that for each square foot
#   that we increase in the house, we can be 95% sure that it will increase the price of that house by some value that's between $137.02 and $155.73. On the same note and
#   as another example, we can say with 95% of confidence that a home that was built one year later, keeping all the other variables constant, will increase the home by
#   something between $23,771 and $41,084. We still have to keep in mind that the three parameters combined only explain 22% of the total variation of the price of the homes.


# Assess the improvement of the new model compared to your original model (simple regression model) by testing whether this change is significant by performing an analysis of variance.
anova(housing_lm_simple,housing_lm)
# Here's the result of the ANOVA:
# Model 1: `Sale Price` ~ sq_ft_lot
# Model 2: `Sale Price` ~ square_feet_total_living + year_built + building_grade
# Res.Df        RSS Df  Sum of Sq      F    Pr(>F)    
# 1  12863 2.0734e+15                                   
# 2  12861 1.6372e+15  2 4.3618e+14 1713.2 < 2.2e-16 ***
# With a very small Pr(>F), we can say that the multiple regression significantly 
#     improved the fit of the model when compared to the model created by the simple regression. 

# Perform casewise diagnostics to identify outliers and/or influential cases, storing each function's output in a dataframe assigned to a unique variable name.
housing_df$residuals <- resid(housing_lm)
housing_df$studentized_residuals <- rstudent(housing_lm)

# Calculate the standardized residuals using the appropriate command, specifying those that are +-2, storing the results of large residuals in a variable you create.
housing_df$std_residuals <- rstandard(housing_lm)
largeResiduals <- subset(housing_df,std_residuals < -2 | std_residuals > 2)
nrow(largeResiduals)/nrow(housing_df)
#    only 2% of the rows had large residuals, which is good and within the expected.

# Use the appropriate function to show the sum of large residuals.
sum(largeResiduals$std_residuals)

# Which specific variables have large residuals (only cases that evaluate as TRUE)?
View(largeResiduals)

# Investigate further by calculating the leverage, cooks distance, and covariance rations. Comment on all cases that are problematics.
housing_df$cooks_distance <- cooks.distance(housing_lm)
housing_df$leverage <- hatvalues(housing_lm)
housing_df$covariance_ratio <- covratio(housing_lm)
largeResiduals <- subset(housing_df,std_residuals < -2 | std_residuals > 2)
View(largeResiduals)
covmax <- 1 + (3*(3+1)/12865)# 1 + [3(k+1)/n] 
covmin <- 1 - (3*(3+1)/12865)# 1 + [3(k+1)/n]
cov_issues_df <- subset(largeResiduals,covariance_ratio < covmin | covariance_ratio > covmax)


# Perform the necessary calculations to assess the assumption of independence and state if the condition is met or not.
library(car)
dwt(housing_lm)
# The result of the test of below one, which is bad news. I tried to do some research on why this is bad, but didn't have luck finding it.


# Perform the necessary calculations to assess the assumption of no multicollinearity and state if the condition is met or not.
vif(housing_lm)
1/vif(housing_lm)
mean(vif(housing_lm))
# The tolerances are higher than 0.2, which is good
# the aberage VIF is significantly greater than 1. This may indicate bias on the regression.

# Visually check the assumptions related to the residuals using the plot() and hist() functions. Summarize what each graph is informing you of and if any anomalies are present.
library(ggplot2)
hist(housing_df$std_residuals)
plot(housing_lm)

# Overall, is this regression model unbiased? If an unbiased regression model, what does this tell us about the sample vs. the entire population model?
# the average VIF is ~1.92, which is substantially above 1, therefore this regression is probably biased. 




#Other lines of code I used to explore the data before choosing my predictors
housing_df_A <- subset(housing_df,CodeZip=="A")
housing_df_B <- subset(housing_df,CodeZip=="B")
housing_df_C <- subset(housing_df,CodeZip=="C")
A_LM <- lm(`Sale Price` ~ square_feet_total_living, data = housing_df_A)
B_LM <- lm(`Sale Price` ~ square_feet_total_living, data = housing_df_B)
C_LM <- lm(`Sale Price` ~ square_feet_total_living, data = housing_df_C)
housing_df_A$PricePredict <- predict(A_LM)
housing_df_B$PricePredict <- predict(B_LM)
housing_df_C$PricePredict <- predict(C_LM)
ggplot() + geom_line(aes(x=housing_df_A$square_feet_total_living, y=housing_df_A$PricePredict, color="red")) + geom_line(aes(x=housing_df_B$square_feet_total_living, y=housing_df_B$PricePredict, color="blue")) + geom_line(aes(x=housing_df_C$square_feet_total_living, y=housing_df_C$PricePredict, color="green"))
ggplot(housing_df, aes(x=CodeReason,y=`Sale Price`)) + geom_boxplot() + geom_point()
unique(housing_df$CodeReason)
length(unique(housing_df$CodeReason))
ggplot(housing_df,aes(CodeReason)) + geom_bar()
ggplot(housing_df) + geom_point(aes(x=square_feet_total_living,y=`Sale Price`,color=CodeReason))