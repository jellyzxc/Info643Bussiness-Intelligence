#################################################################### 
#### I tried to remove statistical outliers  to check the lm  #####
###################################################################
setwd("C:/Users/asus/Desktop/tasks/634BI/project/CodeingTest")
library(tidyverse)
library(readr)  
library(lubridate)   
library(dplyr)

#=======================1 load data======================
ModelData <- read.csv("ModelData.csv",header = TRUE )%>%select(-X)%>% 
  mutate(tradeyear = year(tradeTime),trademonth = month(tradeTime))%>%select(-tradeTime)
glimpse(ModelData)

#keep these nominal  varailbes as factors 
ModelData$buildingType = as.factor(ModelData$buildingType)
ModelData$renovationCondition = as.factor(ModelData$renovationCondition)
ModelData$buildingStructure = as.factor(ModelData$buildingStructure)
ModelData$HasElevator=as.factor(ModelData$HasElevator)
ModelData$OwnOverFiveYears = as.factor(ModelData$OwnOverFiveYears)
ModelData$HasSubway = as.factor(ModelData$HasSubway)
ModelData$district = as.factor(ModelData$district)

glimpse(ModelData)


continuous_vars<- subset(select_if(ModelData, is.numeric))
names(continuous_vars)

nominal_vars<- subset(select_if(ModelData, is.factor))
names(nominal_vars)


ncol=ncol(continuous_vars) 
df<-continuous_vars

for(i in 1:ncol) 
{
  print(i)
  x<-df[,i]
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA 
  df[,i]<-x
}
 
cleanData<-data.frame(nominal_vars,df)
cleanData<-na.omit(cleanData)


#=====================2 choose predictors  by  step================
library(car)

step(lm(cleanData$totalPrice ~.-UnitPrice ,data=cleanData))
 
fit<-lm(formula = cleanData$totalPrice ~ buildingType + renovationCondition + 
     buildingStructure + HasElevator + OwnOverFiveYears + HasSubway + 
     district + followers + PropertySize + NumBedroom + PropertyLevel + 
     constructionTime + ladderRatio + communityAverage + tradeyear + 
     trademonth, data = cleanData)

# predictors: buildingType + renovationCondition + 
            # buildingStructure + HasElevator + OwnOverFiveYears + HasSubway + 
            # district + followers + PropertySize + NumBedroom + PropertyLevel + 
            # constructionTime + ladderRatio + communityAverage + tradeyear + 
            # trademonth 
 
#---trainingData  and  testtingData----
trainingData <- read.csv("ModelData - Training Data.csv",header = TRUE )%>%select(-X)%>% 
  mutate(tradeyear = year(tradeTime),trademonth = month(tradeTime))%>%select(-tradeTime) 

testtingData<- read.csv("ModelData - Testing Data.csv",header = TRUE )%>%select(-X)%>% 
  mutate(tradeyear = year(tradeTime),trademonth = month(tradeTime))%>%select(-tradeTime) 


trainingData$buildingType = as.factor(trainingData$buildingType)
trainingData$renovationCondition = as.factor(trainingData$renovationCondition)
trainingData$buildingStructure = as.factor(trainingData$buildingStructure)
trainingData$HasElevator=as.factor(trainingData$HasElevator)
trainingData$OwnOverFiveYears = as.factor(trainingData$OwnOverFiveYears)
trainingData$HasSubway = as.factor(trainingData$HasSubway)
trainingData$district = as.factor(trainingData$district) 

testtingData$buildingType = as.factor(testtingData$buildingType)
testtingData$renovationCondition = as.factor(testtingData$renovationCondition)
testtingData$buildingStructure = as.factor(testtingData$buildingStructure)
testtingData$HasElevator=as.factor(testtingData$HasElevator)
testtingData$OwnOverFiveYears = as.factor(testtingData$OwnOverFiveYears)
testtingData$HasSubway = as.factor(testtingData$HasSubway)
testtingData$district = as.factor(testtingData$district) 




continuous_vars_t<- subset(select_if(trainingData, is.numeric))
names(continuous_vars_t)

nominal_vars_t<- subset(select_if(trainingData, is.factor))
names(nominal_vars_t)


ncol=ncol(continuous_vars_t) 
df<-continuous_vars_t

for(i in 1:ncol) 
{
  print(i)
  x<-df[,i]
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- NA
  x[x > (qnt[2] + H)] <- NA 
  df[,i]<-x
}

cleanData_t<-data.frame(nominal_vars_t,df)
cleanData_t<-na.omit(cleanData_t)

#cleanData_t: training data without outliers 


#=====================3   bulid lm  ================
#first  use predictors chosen by all modeldata without outliers   and  training data without outliers
fit_t<-lm(formula = cleanData_t$totalPrice ~ buildingType + renovationCondition + 
          buildingStructure + HasElevator + OwnOverFiveYears + HasSubway + 
          district + followers + PropertySize + NumBedroom + PropertyLevel + 
          constructionTime + ladderRatio + communityAverage + tradeyear + 
          trademonth, data = cleanData_t)
 
step(fit_t)

fit_t2<-lm(formula = cleanData_t$totalPrice ~ buildingType + renovationCondition + 
           buildingStructure + OwnOverFiveYears + HasSubway + district + 
           followers + PropertySize + NumBedroom + PropertyLevel + constructionTime + 
           ladderRatio + communityAverage + tradeyear + trademonth, 
          data = cleanData_t)
summary(fit_t2)
#Adjusted R-squared:  0.7808 

#=====================4   adjust model    ================
fit_aj=lm(cleanData_t$totalPrice ~ buildingType + renovationCondition + 
            buildingStructure + OwnOverFiveYears + HasSubway + district + 
            followers + PropertySize + NumBedroom + PropertyLevel + constructionTime + 
            ladderRatio + communityAverage + tradeyear + trademonth+PropertySize*communityAverage, data=cleanData_t)
summary(fit_aj)
# Adjusted R-squared:  0.7971    not better than  outliers contain 
 

vif(fit_aj)    
durbinWatsonTest(fit_aj)  
ncvTest(fit_aj) 
qqPlot(fit_aj,id.method='identify',simulate = TRUE,labels=row.names(house),main='Q-Q plot')
plot(fit_aj,which = 1)



#===========5 predict=============

pr.lm <- predict(fit_aj,testtingData)
MSE.lm <- sum((pr.lm - testtingData$totalPrice)^2)/nrow(testtingData)  

deviation=((testtingData$totalPrice-pr.lm)/testtingData$totalPrice)
comparison=data.frame(predicted=pr.lm,actual=testtingData$totalPrice,deviation)
accuracy=abs(1-abs(mean(deviation)))
accuracy  #  too low     0.04410168

