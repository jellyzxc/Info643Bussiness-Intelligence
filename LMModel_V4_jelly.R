###################### Xiaocui Zhang #######################
setwd("C:/Users/asus/Desktop/tasks/634BI/project/CodeingTest")
library(tidyverse)
library(readr)  
library(lubridate)   
library(dplyr)

#=======================1 load data======================

#----trainingData  and  testtingData----
trainingData <- read.csv("ModelData - Training Data.csv",header = TRUE )%>%select(-X)%>% 
  mutate(tradeyear = year(tradeTime),trademonth = month(tradeTime))%>%select(-tradeTime) 

testtingData<- read.csv("ModelData - Testing Data.csv",header = TRUE )%>%select(-X)%>% 
  mutate(tradeyear = year(tradeTime),trademonth = month(tradeTime))%>%select(-tradeTime) 

#----keep these nominal  varailbes as factors 
trainingData$buildingType = as.factor(trainingData$buildingType)
trainingData$renovationCondition = as.factor(trainingData$renovationCondition)
trainingData$buildingStructure = as.factor(trainingData$buildingStructure)
trainingData$district = as.factor(trainingData$district) 

testtingData$buildingType = as.factor(testtingData$buildingType)
testtingData$renovationCondition = as.factor(testtingData$renovationCondition)
testtingData$buildingStructure = as.factor(testtingData$buildingStructure)
testtingData$district = as.factor(testtingData$district) 


#---generate a new variable to represent all good features(2 level factor) 
trainingData<-trainingData%>%mutate(sum_feather=HasElevator+OwnOverFiveYears+HasSubway)
testtingData<-testtingData%>%mutate(sum_feather=HasElevator+OwnOverFiveYears+HasSubway)

glimpse(trainingData) 


#=====================2 choose predictors  by  step================
step(lm(totalPrice ~.-UnitPrice-HasElevator-OwnOverFiveYears-HasSubway,data=trainingData))

# ---when the min AIC=4811930   the lm like below 
fit_lm_0<-lm(formula = totalPrice ~ followers + PropertySize + NumBedroom + 
     NumLivingRoom + bathRoom + PropertyLevel + buildingType + 
     constructionTime + renovationCondition + buildingStructure + 
     district + communityAverage + tradeyear + trademonth + sum_feather, 
   data = trainingData) 
summary(fit_lm_0)

# Adjusted R-squared:  0.7864 
#remove   PropertyLevel   buildingStructure 
fit_lm_1<-lm(formula = totalPrice ~ followers + PropertySize + NumBedroom + 
             NumLivingRoom + bathRoom  + buildingType + constructionTime + 
             district + communityAverage + tradeyear + trademonth + sum_feather, 
           data = trainingData) 
summary(fit_lm_1)

#Adjusted R-squared:    0.7785  

#remove NumBedroom    bathRoom     
fit_lm<-lm(formula = totalPrice ~ followers + PropertySize + NumLivingRoom + constructionTime + 
               district + communityAverage + tradeyear + trademonth + sum_feather, 
             data = trainingData) 
summary(fit_lm)

# Adjusted R-squared:  0.7783 

# Althoug the Adjusted R-squared is not the largest but all pvaluse are small enough  and the number of the predictors  are the min

#==================3  adjust model 

#--- 3.1  include interaction term ============== 

fit_lm_aj<-lm(formula = totalPrice ~ followers + PropertySize + NumLivingRoom + constructionTime + 
             district + communityAverage + tradeyear + trademonth + sum_feather+trademonth+PropertySize*communityAverage, 
           data = trainingData) 
summary(fit_lm_aj) 

# Adjusted R-squared:  0.8328  increse


hist(trainingData$totalPrice)  #right-skewed
hist(log(trainingData$totalPrice))  #Left-skewed
hist(sqrt(trainingData$totalPrice))  #  Approximate normal distribution

#----3.2  change  trainingData$totalPrice   to  sqrt(trainingData$totalPrice)

fit_lm_aj_1<-lm(formula = sqrt(trainingData$totalPrice) ~ followers + PropertySize + NumLivingRoom + constructionTime + 
                district + communityAverage + tradeyear + trademonth + sum_feather+trademonth+PropertySize*communityAverage, 
              data = trainingData) 
summary(fit_lm_aj_1) 

#Adjusted R-squared:  0.8486   increse again!


#=====================4 regression test  ================
library(car)

# vif(fit_lm_aj_1)
# durbinWatsonTest(fit_lm_aj_1)
# ncvTest(fit_lm_aj_1)   
# qqPlot(fit_lm_aj_1,id.method='identify',simulate = TRUE,labels=row.names(house),main='Q-Q plot')
 
par(mfrow=c(2,2))
plot(fit_1)
 
 

#======================5  predict============================
pr.lm <- predict(fit_lm_aj_1,testtingData)
MSE.lm <- sum((pr.lm^2 - testtingData$totalPrice)^2)/nrow(testtingData)  

#---since y=sqrt(totalPrice)     predict price =pr.lm^2
deviation=((testtingData$totalPrice-pr.lm^2)/testtingData$totalPrice)
comparison=data.frame(predicted=pr.lm^2,actual=testtingData$totalPrice,deviation)
accuracy=abs(1-abs(mean(deviation)))
accuracy  #   0.6622622    





