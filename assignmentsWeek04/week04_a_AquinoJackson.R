# Assignment: Test Scores Exercise
# Name: Aquino, Jackson
# Date: 2022-04-08

#A professor has recently taught two sections of the same course with only one difference between the sections. 
#In one section, he used only examples taken from sports applications, and in the other section, 
#he used examples taken from a variety of application areas. The sports themed section was advertised as such; 
#so students knew which type of section they were enrolling in. 
#The professor has asked you to compare student performance in the two sections using course grades 
#and total points earned in the course. You will need to import the Scores.csv dataset that has been provided for you.
setwd("C:/Users/jackson_aquino/dsc520")
scores_df <- read.csv("data/scores.csv")

#Use the appropriate R functions to answer the following questions:
#  What are the observational units in this study?
str(scores_df)

#  Identify the variables mentioned in the narrative paragraph and determine which are categorical and quantitative?
# $ Count  : int  10 10 20 10 10 10 10 30 10 10 ...
# $ Score  : int  200 205 235 240 250 265 275 285 295 300 ...
# $ Section: chr  "Sports" "Sports" "Sports" "Sports" ...

#  Create one variable to hold a subset of your data set that contains only the Regular Section and one variable for the Sports Section.
ssSports <- subset(scores_df, Section == "Sports")
ssRegular <- subset(scores_df, Section == "Regular")

#Use the Plot function to plot each Sections scores and the number of students achieving that score. Use additional Plot Arguments to label the graph and give each axis an appropriate label. Once you have produced your Plots answer the following questions:
library(ggplot2)
ggplot(ssSports) + geom_point(aes(x=Score,y=Count)) + xlab("Scores") + ylab("Number of students in this section with this score")
ggplot(ssRegular) + geom_point(aes(x=Score,y=Count)) + xlab("Scores") + ylab("Number of students in this section with this score")
#  Comparing and contrasting the point distributions between the two section, looking at both tendency and consistency: Can you say that one section tended to score more points than the other? Justify and explain your answer.
#It looks to me as if the students in the regular section scored more than the ones on the sports section, as I'm seeing bigger numbers on the right of the chart (the part with higher scores with higher frequencies)

#Did every student in one section score more points than every student in the other section? If not, explain what a statistical tendency means in this context.
# No, there were students in the sports section with a higher score than some students on the regular section and there were students 
#   on the regular section with a higher score than some students on the sports section.
#   Looking at the averages and the medians, we can have a better sense about how each section's scores compare to each other.
averageScoreSports <- sum(ssSports$Count * ssSports$Score)/sum(ssSports$Count)
averageScoreRegular <- sum(ssRegular$Count * ssRegular$Score)/sum(ssRegular$Count)

scoresRegular <- c()
rowNumber <- 0
for (eachRowCount in ssRegular$Count) {
  rowNumber <- rowNumber + 1
  for (MyCounter in c(1:eachRowCount)) {
      scoresRegular <- append(scoresRegular, ssRegular$Score[rowNumber])
  }
}
median(scoresRegular)

scoresSports <- c()
rowNumber <- 0
for (eachRowCount in ssSports$Count) {
  rowNumber <- rowNumber + 1
  for (MyCounter in c(1:eachRowCount)) {
    scoresSports <- append(scoresSports, ssSports$Score[rowNumber])
  }
}
median(scoresSports)

scoresSports_df <- data.frame(scoresSports)
ggplot(scoresSports_df) + geom_histogram(aes(x=scoresSports_df$scoresSports),binwidth = 50)

scoresRegular_df <- data.frame(scoresRegular)
ggplot(scoresRegular_df) + geom_histogram(aes(x=scoresRegular_df$scoresRegular),binwidth = 50)

#What could be one additional variable that was not mentioned in the narrative that could be influencing the point distributions between the two sections?
#    One variable that was not mentioned and it pretty important is the number of students on each section. It seems that the regular section had 30 students more than the sports section.
#    another variable that is not mentioned and not found on the dataset is the students' tenure in the university, or previous 
#    level of experience. What if the median score on the sports section is lower just because they are less experienced students? 
#    Comparing their scores for other classes could also give us an idea about their general academic performance to see if students
#    scoring lower on this class were more likely to score lower on other classes
sd(scoresRegular)
sd(scoresSports)
#    The section about sports had a higher standard deviation, which could mean that students in the regular section help each other 
#    more than those on the sports section. 
#    Another reason could be that these students come from more varied backgrounds and have more varied experience.

