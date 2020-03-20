library(dplyr)
library(ggplot2)
library(caTools) # splits
library(rpart) # CART
library(rpart.plot) # CART plotting
library(caret)
library(ROCR)
library(randomForest)
library(gbm)
library(GGally)

library(MASS)


Letters = read.csv("Letters.csv")

#################################### Questions a) ##########################################################

#Define a variable 
Letters$isB = as.factor(Letters$letter == "B")
Letters$letter = as.factor(Letters$letter)


set.seed(456)

train.ids = sample(nrow(Letters), 0.65*nrow(Letters))          
Letters.train = Letters[train.ids,]
Letters.test = Letters[-train.ids,]

  #i) Accuracy Baseline model 
table(Letters.test$isB)

# Accuracy of Baseline model = 72.22%

  #ii) Logistic regression 
LogRe = glm(data = Letters.train, family = binomial, 
            isB ~ xbox+ybox+width+height+onpix+xbar
            +ybar+x2bar+y2bar+xybar+x2ybar+xy2bar
            +xedge+xedgeycor+yedge+yedgexcor)

summary(LogRe)

predLogRes = predict(LogRe, newdata = Letters.test, type = "response")

(tab_Log_Reg = table(Letters.test$isB, predLogRes > 0.5))
(Accuracy_LogRes = sum(diag(tab_Log_Reg))/sum(tab_Log_Reg))

#Accuracy Logistic Regression = 0.9468

  #iii) AUC curve

rocr.B <- prediction(predLogRes, Letters.test$isB)
logPerformance <- performance(rocr.B, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.B, "auc")@y.values)

  #iv) CART 

#Create the different cp values
cpVals = data.frame(cp = seq(0, .04, by=.002))

# First standard CV with respect to Accuracy, then the loss function
# Syntax below: 
# method = specify classification method, "rpart" for CART
# tuneGrid = gives the sequence of parameters to try, 
#             in this case, we try cp = 0 through cp=0.1 in increments of .002
# trControl = here using 10-fold cross validation
# metric = "Accuracy" for classification accuracy, "RMSE" or "Rsquared" or for regression
set.seed(456)

cart = train(data = Letters.train, 
             isB ~ xbox+ybox+width+height+onpix+xbar
             +ybar+x2bar+y2bar+xybar+x2ybar+xy2bar
             +xedge+xedgeycor+yedge+yedgexcor,
             method="rpart",
             tuneGrid = cpVals,
             trControl = trainControl(method = "cv", number=10),
             metric = "Accuracy")

cart$results

cart$bestTune          #After Cross validation cp = 0.004
cartBest = cart$finalModel
prp(cartBest, digits=3)



#We extract the matrix since cart doesn't work with factor variables 
Letters.test.ma = as.data.frame(model.matrix(isB~.+0, data=Letters.test))

predCart = predict(cartBest, newdata=Letters.test.ma, type="class")
table(Letters.test$isB,predCart )

(tab_cart = table(Letters.test$isB, predCart))
(Accuracy_cart = sum(diag(tab_cart))/sum(tab_cart))


#Accuracy CART = 0.924


  #v) Random Forest 

set.seed(456)
rf <- train(data = Letters.train, 
            isB ~ xbox+ybox+width+height+onpix+xbar
            +ybar+x2bar+y2bar+xybar+x2ybar+xy2bar
            +xedge+xedgeycor+yedge+yedgexcor,
            method = "rf")


rf$results  
rf
rf.best <- rf$finalModel
pred.best.rf <- predict(rf.best, newdata = Letters.test.ma)

ggplot(rf$results, aes(x = mtry, y = Accuracy)) + geom_point(size = 3) + 
  ylab("Accuracy") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))


(tab_rf = table(Letters.test$isB, pred.best.rf))
(Accuracy_rf = sum(diag(tab_rf))/sum(tab_rf))

#The Accuracy of Radom forest is 0.9715



################################## Question b) ################################################################### 
  
  #i) Baseline 

table(Letters.test$letter)

#The most frequent outcome is A with 546 occurences 

  #ii)LDA model 

ldam = lda(data = Letters.train,
           letter ~ xbox+ybox+width+height+onpix+xbar
           +ybar+x2bar+y2bar+xybar+x2ybar+xy2bar
           +xedge+xedgeycor+yedge+yedgexcor)

predLda = predict(ldam, Letters.test)
predLda_class = predLda$class

(tabLda = table(Letters.test$letter,predLda_class))

(Accuracy_Lda = sum(diag(tabLda))/sum(tabLda))

# Accuracy of Lda = 0.918

  #iii) CART model + cross validation 

set.seed(456)

cartm = train(letter ~ xbox+ybox+width+height+onpix+xbar
              +ybar+x2bar+y2bar+xybar+x2ybar+xy2bar
              +xedge+xedgeycor+yedge+yedgexcor,
              method = "rpart",
              metric = "Accuracy",
              tuneGrid = data.frame(cp = seq(0, .1, by=.0001)),
              trControl = trainControl(method = "cv",number = 10),
              data = Letters.train)

cartm$bestTune
cartm_best = cartm$finalModel
prp(cartm_best, digits=3)

#accuracy

predCart = predict(cartm_best, newdata=Letters.test.ma, type="class")
(tabCart = table(Letters.test$letter,predCart))

(Accuracy_Cart = sum(diag(tabCart))/sum(tabCart))
    
   #Accuracy of cartm = 0.8891


  #Iv) vanilla

set.seed(456)
vanilla =  randomForest(letter ~ xbox+ybox+width+height+onpix+xbar
             +ybar+x2bar+y2bar+xybar+x2ybar+xy2bar
             +xedge+xedgeycor+yedge+yedgexcor, data = Letters.train,
             mtry = 16)


pred_vanilla = predict(vanilla, newdata = Letters.test.ma)

(tabvanilla = table(Letters.test$letter, pred_vanilla))


(Accuracy_vanilla = sum(diag(tabvanilla))/sum(tabvanilla))

#Accuracy of vanilla = 0.9477




  #v) Random forrest multiclass 

set.seed(456)
rfm =  train(letter ~ xbox+ybox+width+height+onpix+xbar
            +ybar+x2bar+y2bar+xybar+x2ybar+xy2bar
            +xedge+xedgeycor+yedge+yedgexcor, data = Letters.train,
            method = "rf",
            tuneGrid = data.frame(mtry=1:16),
            trControl = trainControl(method="cv", number=10, verboseIter = TRUE),
            metric = "Accuracy")

#we choose mtry = 6


rfm$results  
rfm.best <- rfm$finalModel
pred.best.rfm <- predict(rfm.best, newdata = Letters.test.ma) # can use same model matrix

ggplot(rfm$results, aes(x = mtry, y = Accuracy)) + geom_point(size = 3) + 
  ylab("Accuracy") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))


(tabrfm = table(Letters.test$letter, pred.best.rfm))


(Accuracy_Cart = sum(diag(tabrfm))/sum(tabrfm))

#Accuracy of random forest  = 0.965


  #vi) Boosting 

set.seed(456)
boostm = gbm(letter ~ xbox+ybox+width+height+onpix+xbar
             +ybar+x2bar+y2bar+xybar+x2ybar+xy2bar
             +xedge+xedgeycor+yedge+yedgexcor,
             data = Letters.train,
             distribution = "multinomial",n.trees = 3300,
             interaction.depth = 10)


pred_boost = predict(boostm, newdata = Letters.test.ma, n.trees = 3300, type = "response")

pred = apply(pred_boost,1,which.max)
pred = factor(pred, levels = c(1,2,3,4), labels = c("A","B","P","R"))

(tabboost = table(Letters.test$letter, pred))

(Accuracy_Boost = sum(diag(tabboost))/sum(tabboost))

#Accuracy of Boosting = 0.9798


