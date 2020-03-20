wr = read.csv("Wrangler242-Fall2019.csv")

library(dplyr)
library(car)
library(ggplot2)

wr.train = filter(wr, Year <= 2015)
wr.test = filter(wr, Year > 2015)

reg1 = lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.Energy +
          CPI.All, data = wr.train)
summary(reg1)


#Compute OSR

salesPredictions = predict(reg1, newdata = wr.test)
SSE1 = sum((wr.test$WranglerSales - salesPredictions)^2)
SST1 = sum((wr.test$WranglerSales - mean(wr.train$WranglerSales))^2)
OS1 = 1 - SSE1/SST1 


#Testing the significance of regression coefficients
ggcoef(
  reg1,
  vline_color = "red",
  vline_linetype =  "solid",
  errorbar_color = "blue",
  errorbar_height = .25,
  exclude_intercept = TRUE
)

# At least we can reject the null hypothesis for WranglerQueries

ggscatmat(wr, columns = 5:8, alpha = 0.8)
#There is correlation between WranglerQueries and Unemployment, CPI.All and WranglerQueries
#Multicollinearity thus, we need to delete some data ----> 

#Compute VIF
vif(reg1)

#Remove CPI.All
reg2 = lm(WranglerSales ~ Unemployment + WranglerQueries +
            CPI.Energy, data = wr.train)
summary(reg2)

vif(reg2)

#OSR2
salesPredictions2 = predict(reg2, newdata = wr.test)
SSE2 = sum((wr.test$WranglerSales - salesPredictions2)^2)
SST2 = sum((wr.test$WranglerSales - mean(wr.train$WranglerSales))^2)
OS2 = 1 - SSE2/SST2 


#Remove Unemployment
reg3 = lm(WranglerSales ~ WranglerQueries +
            CPI.Energy, data = wr.train)
summary(reg3)

vif(reg3)

#OSR3
salesPredictions3 = predict(reg3, newdata = wr.test)
SSE3 = sum((wr.test$WranglerSales - salesPredictions3)^2)
SST3 = sum((wr.test$WranglerSales - mean(wr.train$WranglerSales))^2)
OS3 = 1 - SSE3/SST3 

#R^2 = 0.79  and OSR = 0.58

#Remove CPI.Energy
reg4 = lm(WranglerSales ~ WranglerQueries, data = wr.train)
           
summary(reg4)


#OSR4
salesPredictions4 = predict(reg4, newdata = wr.test)
SSE4 = sum((wr.test$WranglerSales - salesPredictions4)^2)
SST4 = sum((wr.test$WranglerSales - mean(wr.train$WranglerSales))^2)
OS4 = 1 - SSE4/SST4 

#R^2 = 0.79  and OSR = 0.54

#Ask question about the t values
#Add MonthFactor

reg5 = lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.Energy +
            CPI.All + MonthFactor, data = wr.train)
summary(reg5)


#Part C
reg6 = lm(WranglerSales ~ WranglerQueries + MonthFactor, data = wr.train)
summary(reg6)


salesPredictions6 = predict(reg4, newdata = wr.test)
SSE6 = sum((wr.test$WranglerSales - salesPredictions6)^2)
SST6 = sum((wr.test$WranglerSales - mean(wr.train$WranglerSales))^2)
OS6 = 1 - SSE6/SST6 

#R^2 = 0.86 and OSR=0.54



# I will add the price per barrel
price = select(Crude_Oil_Price, PricePerBarrel)
wr1 = wr %>% mutate(Barrel = price$PricePerBarrel)

wr1.train = filter(wr1, Year <= 2015)
wr1.test = filter(wr1, Year > 2015)

reg7 = lm(WranglerSales ~ WranglerQueries + MonthFactor + Barrel, data = wr1.train)
summary(reg7)

salesPredictions7 = predict(reg7, newdata = wr1.test)
SSE7 = sum((wr1.test$WranglerSales - salesPredictions7)^2)
SST7 = sum((wr1.test$WranglerSales - mean(wr1.train$WranglerSales))^2)
OS7 = 1 - SSE7/SST7 

#R^2=0.86  OSR=0.66 but P value Barrel = 0.48

ggplot(wr1, aes(x=Barrel, y=WranglerSales)) + geom_point() +
  xlab("Querries") + ylab("Barrel")


#Add the trend Oil

TrendOil = select(Google_Trends_Oil, Oil)
wr2 = wr %>% mutate(Trend = TrendOil$Oil)

wr2.train = filter(wr2, Year <= 2015)
wr2.test = filter(wr2, Year > 2015)

reg8 = lm(WranglerSales ~ WranglerQueries + MonthFactor + Trend, data = wr2.train)
summary(reg8)

salesPredictions8 = predict(reg8, newdata = wr2.test)
SSE8 = sum((wr2.test$WranglerSales - salesPredictions8)^2)
SST8 = sum((wr2.test$WranglerSales - mean(wr2.train$WranglerSales))^2)
OS8 = 1 - SSE8/SST8 
 
vif(reg8)

# R^2= 0.86  OSR = 0.65 but p value Trend = 0.67

#Last try trends of Range Rover, competitors

TrendRange = select(Range_Rover, Popul)
wr3 = wr %>% mutate(Trend = TrendRange$Popul)

wr3.train = filter(wr3, Year <= 2015)
wr3.test = filter(wr3, Year > 2015)

reg9 = lm(WranglerSales ~ WranglerQueries + MonthFactor + Trend, data = wr3.train)
summary(reg9)

salesPredictions9 = predict(reg9, newdata = wr3.test)
SSE9 = sum((wr3.test$WranglerSales - salesPredictions9)^2)
SST9 = sum((wr3.test$WranglerSales - mean(wr3.train$WranglerSales))^2)
OS9 = 1 - SSE9/SST9 

vif(reg8)


#Questions
#Why do the ViF don't work when I introduce de MonthFactor
#I definitely need more information about the t value. what is the meaning of p
#Demonstration ex 2
#Do you think the log can be useful? 
#3D plot with ggplot ? 
#Why 5% for the t test 