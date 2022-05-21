# Assignment: Fit a Logistic Regression Model
# Name: Aquino, Jackson
# Date: 2022-05-20
my_data <- read.csv("C:/users/jackson_aquino/DSC520/data/binary-classifier-data.csv")

# Fit a logistic regression model to the binary-classifier-data.csv dataset
# The dataset contains three variables; label, x, and y. The label variable is either 0 or 1 and is the output we want to predict using the x and y variables.

my_model <- glm(label ~ x + y, data = my_data, family = binomial(link="logit"))

# What is the accuracy of the logistic regression classifier?
my_data$label_prediction <- round(predict(my_model,type = "response"))
print(paste("Accuracy = ",round(sum(my_data$label == my_data$label_prediction)/nrow(my_data)*100,2),"%",sep = ""))

# Keep this assignment handy, as you will be comparing your results from this week to next week.