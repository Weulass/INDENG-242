
#libraries we may need
library(tm)
library(SnowballC)
library(wordcloud)
library(MASS)
library(caTools)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
install.packages("tm.plugin.webmining")
library(rJava)
library(tibble)
library("tm.plugin.webmining")
library(boot)

#read the ggplot csv file
Ggplot = read.csv("ggplot2questions2016_17.csv", stringsAsFactors=FALSE) 
for(i in c(1:nrow(Ggplot))){
  Ggplot$Body[i] = extractHTMLStrip(Ggplot$Body[i]) #delete the html tags
  Ggplot$Body[i]=gsub(pattern = "\n",replacement="",x=Ggplot$Body[i]) #take out the /n not deleted by extractHTMLStrip
}
str(Ggplot) #take a look at the dataset 

#create a corpus to clean the data
corpus_body = Corpus(VectorSource(Ggplot$Body))
corpus_title = Corpus(VectorSource(Ggplot$Title))

#make everthing in lower case
corpus_body = tm_map(corpus_body, tolower)
corpus_title=tm_map(corpus_title,tolower)

#remove punctuation, which is useless here for us
corpus_body=tm_map(corpus_body, removePunctuation)
corpus_title=tm_map(corpus_title, removePunctuation)

#remove stopword
#and ggplot and ggplot2 since all the questions of the dataset or about this library
corpus_body = tm_map(corpus_body, removeWords, c(stopwords("english")))
corpus_title=tm_map(corpus_title, removeWords, c(stopwords("english")))

#Stemming
corpus_body=tm_map(corpus_body,stemDocument)
corpus_title=tm_map(corpus_title,stemDocument)

#Remove words made only of numbers, since they are useless and are often 
#specific to each user (often the datasets they are working on)
corpus_body=tm_map(corpus_body,removeNumbers)
corpus_title=tm_map(corpus_title,removeNumbers)

#take a look again
strwrap(corpus_body[[1]])
strwrap(corpus_title[[1]])

#compute the number of times each bag of word appears
#Our solution to the possibility of overfitting is to only keep terms
#who appears at least in 2% of the body texts of the questions 
#or at least in 2% of the titles
frequencies_body = DocumentTermMatrix(corpus_body)
frequencies_title=DocumentTermMatrix(corpus_title)
frequencies_body
sparse_body = removeSparseTerms(frequencies_body, 0.98)
sparse_title=removeSparseTerms(frequencies_title,0.98)
data_body=as.data.frame(as.matrix(sparse_body))
data_title=as.data.frame(as.matrix(sparse_title))

#creation of a new data frame to combine both 
data=data_body 
for (i in names(data_body)){  #check if a column of body is in title, an add the two columns if true
  for (j in names(data_title)){
    if (i==j){
      data[i]=data_body[i]+data_title[j]
    }
  }
}
for (j in names(data_title)){ #check if a column of title isn't in data_body
  exist=0
  for (i in names(data_body)){
    if (j==i){
      exist=1
      break
    }
  }
  if (exist==0){     #if isn't, add the missing column
    data[j]=data_title[j]
  }
}
colnames(data) = make.names(colnames(data))#in case a column name begins with a number
data$Score=Ggplot$Score #add the dependant variable to our new dataset
data$Useful = as.factor(as.numeric(data$Score >=1)) #useful if score>=1
data$Score=NULL #don't need it anymore

set.seed(123)  # So we get the same result each time
spl = sample.split(data$Useful, SplitRatio = 0.7)

#separating the dataset into a training set and a test set
dataTrain = data %>% filter(spl == TRUE)
dataTest = data %>% filter(spl == FALSE)

#Logistic Regression
#stepwise regression would be great but too long to use
dataLog = glm(Useful ~ ., data = dataTrain, family = "binomial")
summary(dataLog) #print a summary of the result
PredictLog = predict(dataLog, newdata = dataTest, type = "response") #prediction on the test set
tableAccuracy <- function(test, pred) { #compute the accuracy
  t = table(test, pred)
  a = sum(diag(t))/length(test)
  return(a)
}
tableTPR <- function(test, pred) { #compute the TPR
  t = table(test, pred)
  a = t[2,2]/(t[2,1]+t[2,2])
  return(a)
}
tableFPR <- function(test, pred) { #compute the FPR
  t = table(test, pred)
  a = t[1,2]/(t[1,1]+t[1,2])
  return(a)
}
table(dataTest$Useful, PredictLog > 0.5) #without other informations we use 0.5 as a threshold
tableAccuracy(dataTest$Useful, PredictLog > 0.5)#accuracy of 0.563
tableTPR(dataTest$Useful,PredictLog>0.5) #TPR=0.515
tableFPR(dataTest$Useful,PredictLog>0.5) #FPR=0.391

#CART
set.seed(123)
#10_fold CV to choose the cp parameter
train.cart = train(Useful ~ .,
                   data = dataTrain,
                   method = "rpart",
                   tuneGrid = data.frame(cp=seq(0, 0.4, 0.002)),
                   trControl = trainControl(method="cv", number=10))
train.cart
train.cart$results
train.cart$bestTune #cp=0.006
mod.cart = train.cart$finalModel
prp(mod.cart) #print the tree
predict.cart = predict(mod.cart, newdata = dataTest, type = "class") # prediction using the test set
tableAccuracy(dataTest$Useful, predict.cart)#accuracy of 0.543 on the test set
tableTPR(dataTest$Useful,predict.cart) #TPR=0.476
tableFPR(dataTest$Useful,predict.cart) #FPR=0.391

 #Random Forest
set.seed(123)
dataRF = randomForest(Useful ~ ., data=dataTrain, mtry=21)
PredictRF = predict(dataRF, newdata = dataTest)
tableAccuracy(dataTest$Useful, PredictRF) #0.568
tableTPR(dataTest$Useful,PredictRF) #TPR=0.539
tableFPR(dataTest$Useful,PredictRF) #FPR=0.404

#Boosting with 10-fold CV
set.seed(123)
tGrid = expand.grid(n.trees = (1:50), interaction.depth = c(1,2,4,6,8,10,12,14,16,18,20,22),
                    shrinkage = 0.01, n.minobsinnode = 10) 

#10-fold double CV for n.trees and interaction.depth
train.boost <- train(Useful ~ .,
                     data = dataTrain,
                     method = "gbm",
                     tuneGrid = tGrid,
                     trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                     metric = "Accuracy",
                     distribution = "bernoulli")
train.boost
train.boost$results
train.boost$bestTune #n.trees=41 and interaction.depth=18
mod.boost=train.boost$finalModel
dataTrain.mm=as.data.frame(model.matrix(Useful ~ . +0, data = dataTrain))
dataTest.mm = as.data.frame(model.matrix(Useful ~ . +0, data = dataTest))
predict.boost = predict(mod.boost, newdata = dataTest.mm,n.trees=41, type = "response") #prediction on the test set
tableAccuracy(dataTest$Useful, predict.boost < 0.5) #accuracy=0.5558
ggplot(train.boost$results, aes(x = n.trees, y = Accuracy, colour = as.factor(interaction.depth))) + geom_line() + 
  ylab("CV Accuracy") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18)) + 
  scale_color_discrete(name = "interaction.depth")
tableTPR(dataTest$Useful,predict.boost < 0.5) #TPR=0.393
tableFPR(dataTest$Useful,predict.boost < 0.5) #FPR=0.286


#Let's use Bootstrap to determine the variability of the accuracy,TPR,FPR of each model
#begin by coding the metric functions I'll need
accuracy_log <- function(model,index) {  #to use for bootstrap to compute the accuracy for the LogReg
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tableAccuracy(responses,predictions >0.5))
}

accuracy <-function(model,index){ #for the other models
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tableAccuracy(responses,predictions))
}

accuracy_boost <-function(model,index){ #for the Boosting model
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tableAccuracy(responses,predictions<0.5))
}

tpr_log <-function(model,index) {  #to use for bootstrap to compute the TPR for the LogReg
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tableTPR(responses,predictions>0.5))
}

tpr_boost<-function(model,index) {  #for the Boosting model
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tableTPR(responses,predictions<0.5))
}

tpr<-function(model,index) {  #for the other models
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tableTPR(responses,predictions))
}

fpr_log <- function(model,index) {  #to use for bootstrap to compute the FPR for the LogReg
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tableFPR(responses,predictions>0.5))
}

fpr_boost <- function(model,index) {  #for the Boosting model
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tableFPR(responses,predictions<0.5))
}

fpr <- function(model,index) {  #for the other models
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tableFPR(responses,predictions))
}
#need the Precision for the last part of the HW
##
tablePrecision <-function(test,pred){
  t=table(test,pred)
  return(t[2,2]/(t[2,2]+t[1,2]))
}
precision_log <-function(model,index) {
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tablePrecision(responses,predictions>0.5))
}

precision_boost<- function(model,index){
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tablePrecision(responses,predictions<0.5))
}

precision<-function(model,index){
  responses <- model$response[index]
  predictions <- model$prediction[index]
  return(tablePrecision(responses,predictions))
}
##

all_metrics_log <- function(model,index) { #combine all metrics for the LogReg
  accuracy=accuracy_log(model,index)
  tpr=tpr_log(model,index)
  fpr=fpr_log(model,index)
  precision=precision_log(model,index)
  return(c(accuracy,tpr,fpr,precision))
}

all_metrics_boost<- function(model,index) { #combine all metrics for the Boosting model
  accuracy <- accuracy_boost(model,index)
  tpr=tpr_boost(model,index)
  fpr=fpr_boost(model,index)
  precision=precision_boost(model,index)
  return(c(accuracy,tpr,fpr,precision))
}

all_metrics<- function(model,index) { #combine all metrics for the other models
  accuracy=accuracy(model,index)
  tpr=tpr(model,index)
  fpr=fpr(model,index)
  precision=precision(model,index)
  return(c(accuracy,tpr,fpr,precision))
}

#CI Logistic Regression  
LogReg_test_set= data.frame(response = dataTest$Useful, prediction = PredictLog)

#sanitiy checks
accuracy_log(LogReg_test_set,1:2240)
tpr(LogReg_test_set,1:2240)
fpr(LogReg_test_set,1:2240)
all_metrics_log(LogReg_test_set,1:2240)
#Bootstrap
set.seed(123)
LogReg_boot <- boot(LogReg_test_set, all_metrics_log, R = 10000)
boot.ci(LogReg_boot, index =1 , type = "basic") #CI of accuracy with level 95%
boot.ci(LogReg_boot, index =2 , type = "basic") #CI of TPR 
boot.ci(LogReg_boot, index =3 , type = "basic") #CI of FPR

#CI CART
set.seed(123)
CART_test_set=data.frame(response = dataTest$Useful, prediction = predict.cart)
all_metrics(CART_test_set,1:2240)#sanity check
CART_boot=boot(CART_test_set,all_metrics,R=10000)
boot.ci(CART_boot, index =1 , type = "basic") #CI of accuracy with level 95%
boot.ci(CART_boot, index =2 , type = "basic") #CI of TPR 
boot.ci(CART_boot, index =3 , type = "basic") #CI of FPR


#CI RF
set.seed(123)
RF_test_set=data.frame(response = dataTest$Useful, prediction = PredictRF)
all_metrics(RF_test_set,1:2240)
RF_boot=boot(RF_test_set,all_metrics,R=10000)
boot.ci(RF_boot, index =1 , type = "basic") #CI of accuracy with level 95%
boot.ci(RF_boot, index =2 , type = "basic") #CI of TPR 
boot.ci(RF_boot, index =3 , type = "basic") #CI of FPR

#CI Boosting
set.seed(123)
Boost_test_set=data.frame(response = dataTest$Useful, prediction = predict.boost)
all_metrics_boost(Boost_test_set,1:2240) #sanity check
Boost_boot=boot(Boost_test_set,all_metrics_boost,R=10000)
boot.ci(Boost_boot, index =1 , type = "basic") #CI of accuracy with level 95%
boot.ci(Boost_boot, index =2 , type = "basic") #CI of TPR 
boot.ci(Boost_boot, index =3 , type = "basic") #CI of FPR

#precision of models
##Logistic Regression
precision_LogReg=tablePrecision(dataTest$Useful,PredictLog>0.5) #0.5607
boot.ci(LogReg_boot,index=4,type="basic") #CI of precision

##CART
precision_CART=tablePrecision(dataTest$Useful,predict.cart) #0.5412
boot.ci(CART_boot,index=4,type="basic") #CI of precision

##RF
precision_RF=tablePrecision(dataTest$Useful,PredictRF) #0.5641
boot.ci(RF_boot,index=4,type="basic") #CI of precision

##Boosting
precision_Boosting=tablePrecision(dataTest$Useful,predict.boost<0.5) #0.5712
boot.ci(Boost_boot,index=4,type="basic") #CI of precision

#Choice of a better threshold for the Boosting method

threshold <-function(model) {
  x=0
  best_i=0
  #a too severe threshold will be useless, so we ask for at least 100 TP on the training set
  for (i in seq(0.45,0.55,0.01)){
    if (tablePrecision(dataTrain$Useful,model<i)>x && table(dataTrain$Useful,model<i)[2,2]>100){ 
      x=tablePrecision(dataTrain$Useful,model<i)
      best_i=i
    }
  }
  return(c(x,best_i))
}

predict.boost_train = predict(mod.boost, newdata = dataTrain.mm,n.trees=41, type = "response")
best_threshold=threshold(predict.boost_train)[2] #0.46 as a new gbm threshold
tablePrecision(dataTest$Useful,predict.boost<best_threshold) #0.74

#Let's use directly the probabilities computed by the algorithm
#performance compared to the random  model used by SOF

set.seed(123)
perf_boosting<-function(model){
  y=0
  for (i in c(1:10000)){
  test=sample_n(dataTest,size=15,replace = FALSE)
  test.mm = as.data.frame(model.matrix(Useful ~ . +0, data = test))
  pred = predict(model, newdata = test.mm,n.trees=41, type = "response")
  x=as.numeric(test$Useful[pred==min(pred)]==1) #min here since gbm flips the probabilities
  y=y+x
  }
  return (y/10000)
}

perf_boosting(mod.boost) #0.65



