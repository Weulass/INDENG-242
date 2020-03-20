library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)

set.seed(144)
patients = read.csv("framingham.csv")

#Make sure the train and test sets have respectively 70% and 30% of patients with or without the disease
split = sample.split(patients$TenYearCHD, SplitRatio = 0.7)

patients.train = filter(patients, split == TRUE)
patients.test = filter(patients, split == FALSE)

#Check if there are the same ratio for of TenYearCHD for the train and test set
#table(patients.test$TenYearCHD)      0    1   2171  390
#table(patients$TenYearCHD)  0   1   930   167
#Approximately 18 percent

mod1 = glm(TenYearCHD ~ male + age + education + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + sysBP + diaBP + BMI + heartRate + glucose, data = patients.train , family = binomial)
summary(mod1)

 

#Question iv)

predTest = predict(mod1, newdata=patients.test, type="response")
summary(predTest)

table(patients.test$TenYearCHD, predTest>0.16)

#Question v)



#Question a)
rocr.Tenyear <- prediction(predTest, patients.test$TenYearCHD)
logPerformance <- performance(rocr.Tenyear, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.Tenyear, "auc")@y.values)
