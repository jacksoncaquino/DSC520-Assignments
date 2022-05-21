# Assignment: thoracic surgery data
# Name: Aquino, Jackson
# Date: 2022-05-16

surgery_data_df <- read.csv("C:/Users/jackson_aquino/dsc520/assignments/assginmentsWeek10/thoracic surgery data.csv")

#   Fit a binary logistic regression model to the data set that predicts whether or not the patient survived for one year (the Risk1Y variable) after the surgery. 
#       Use the glm() function to perform the logistic regression. See Generalized Linear Models for an example. Include a summary using the summary() function in your results.

head(surgery_data_df)

my_model <- glm(Died_in_1_year ~ Age_at_surgery + Forced_vital_capacity + Smoking + Pain_before_surgery + Weakness_before_surgery, data = surgery_data_df, family = binomial(link = "logit"))
summary(my_model)

# According to the summary, which variables had the greatest effect on the survival rate?
#                               Estimate  Std. Error z value Pr(>|z|)  
# (Intercept)                 -2.221359   1.324554  -1.677   0.0935 .
# Age_at_surgery               0.001758   0.016402   0.107   0.9147  
# Forced_vital_capacity       -0.133757   0.164053  -0.815   0.4149  
# SmokingTRUE                  0.749397   0.424659   1.765   0.0776 .
# Pain_before_surgeryTRUE      0.729916   0.461114   1.583   0.1134  
# Weakness_before_surgeryTRUE  0.513994   0.325353   1.580   0.1142  

# Looks like smoking was the variable with the greatest effect on the survival rate, 
#   followed by pain before the surgery being true and by weakness before surgery being true. 
#   I was surprised with how age seems to have such a small effect on the outcome.

#   To compute the accuracy of your model, use the dataset to predict the outcome variable. The percent of correct predictions is the accuracy of your model. 
#   What is the accuracy of your model?
compare <- data.frame(prediction = ifelse(round(predict(my_model, type = "response"))==0,FALSE,TRUE), death = surgery_data_df$Died_in_1_year)
accuracy <- sum(compare$prediction == compare$death)/nrow(compare)
#   85%, but I always got False, so I don't love my model...
#   Update: Incredible. I added several variables later and ran another regressions, still got all False. 
#   Then I ran a decision tree and now I get some Trues and some falses, but the accuracy of the new model is 86% 


#for reference only:
library(ggplot2)
my_model <- glm(Died_in_1_year ~ Diagnosis + Forced_vital_capacity + Volume_exhaled_forced_expiration_1s + Haemoptysis_before_surgery + Pain_before_surgery + Performance_status + Tumour_size + Weakness_before_surgery + Cough_before_surgery + Dyspnoea_before_surgery + diabetes_mellitus + MI_6m + Died_in_1_year + Age_at_surgery + Asthma + Smoking, data = surgery_data_df, family = binomial(link = "logit"))
test_df <- data.frame(died = surgery_data_df$Died_in_1_year, prediction = predict(my_model,type = "response"))
test_df$died_number <- ifelse(test_df$died == TRUE,1,0)
head(test_df,10)
View(test_df)
ggplot(test_df) + geom_point(aes(x=prediction,y=died_number))
test_df$prediction <- round(test_df$prediction)

library(rpart)
library(rpart.plot)
tree <- rpart(Died_in_1_year ~ Diagnosis + Forced_vital_capacity + Volume_exhaled_forced_expiration_1s + Haemoptysis_before_surgery + Pain_before_surgery + Performance_status + Tumour_size + Weakness_before_surgery + Cough_before_surgery + Dyspnoea_before_surgery + diabetes_mellitus + MI_6m + Died_in_1_year + Age_at_surgery + Asthma + Smoking, data = surgery_data_df)
rpart.plot(tree)
summary(tree)
test_df <- data.frame(died = surgery_data_df$Died_in_1_year, prediction = predict(tree))
test_df$prediction <- round(test_df$prediction)
test_df$died_number <- ifelse(test_df$died == TRUE,1,0)
ggplot(test_df) + geom_point(aes(x=prediction,y=died_number))
head(test_df)
test_df$Right <- ifelse(test_df$prediction == test_df$died_number,TRUE,FALSE)
paste("Accuracy: ",round(sum(test_df$Right == TRUE)/nrow(test_df)*100,1),"%",sep="")
sum(test_df$died_number == 1 & test_df$prediction == 0)
sum(test_df$died_number == 0 & test_df$prediction == 1)
sum(test_df$died_number == 1 & test_df$prediction == 1)
sum(test_df$died_number == 0 & test_df$prediction == 0)

sum(test_df$died_number == 1 & test_df$prediction == 0) + sum(test_df$died_number == 0 & test_df$prediction == 1)
sum(test_df$died_number == 1 & test_df$prediction == 1) + sum(test_df$died_number == 0 & test_df$prediction == 0)
