###################### Xiaocui Zhang #######################
setwd("C:/Users/asus/Desktop/tasks/634BI/project/CodeingTest")
library(tidyverse)
library(readr)  
library(lubridate)   
library(dplyr)

######################################### 
#only use part of the data ############## 
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
trainingData<-trainingData[1:3000,]
testtingData<-testtingData[1:1000,]

step(lm(totalPrice ~.-UnitPrice-HasElevator-OwnOverFiveYears-HasSubway,data=trainingData))

# ---when the min AIC=AIC: 68956.6   the lm like below 
fit_lm_0<-lm( totalPrice ~ followers + PropertySize + NumBedroom + 
                kitchen + bathRoom + constructionTime + renovationCondition + 
                buildingStructure + ladderRatio + district + communityAverage + 
                tradeyear + trademonth, data = trainingData) 
summary(fit_lm_0)

# Adjusted R-squared:   0.7907  


#remove  kitchen  constructionTime    buildingStructure   district          
fit_lm_1<-lm(totalPrice ~ followers + PropertySize + NumBedroom + 
               bathRoom  + renovationCondition + ladderRatio  + communityAverage + 
               tradeyear + trademonth,  data = trainingData) 
summary(fit_lm_1)

#Adjusted R-squared:    Adjusted R-squared:   0.78    

#remove NumBedroom  followers                       
fit_lm<-lm(  totalPrice ~  PropertySize +  bathRoom  + renovationCondition + ladderRatio  + communityAverage + 
             tradeyear + trademonth,  data = trainingData)
           
summary(fit_lm)

#Adjusted R-squared:  0.7796 

# SO the final preditors are    
##  PropertySize  bathRoom  renovationCondition  ladderRatio   communityAverage   tradeyear   trademonth    
fit_lm<-lm(formula = totalPrice ~  PropertySize +  bathRoom  + renovationCondition + ladderRatio  + communityAverage + 
             tradeyear + trademonth,  data = trainingData)
 
summary(fit_lm)

# Althoug the Adjusted R-squared is not the largest,  all pvaluse are small enough  and the number of the predictors  are the min 
# it is more interpretable

#==================3  adjust model 

#--- 3.1  include interaction term ============== 

fit_lm_aj<-lm(formula = totalPrice ~  PropertySize +  bathRoom  + renovationCondition + ladderRatio  + communityAverage + 
                tradeyear + trademonth +PropertySize*communityAverage, 
              data = trainingData) 
summary(fit_lm_aj) 

#   Adjusted R-squared:  0.8269 increse


hist(trainingData$totalPrice)  #right-skewed
hist(log(trainingData$totalPrice))  #Left-skewed
hist(sqrt(trainingData$totalPrice))  #  Approximate normal distribution

#----3.2  change  trainingData$totalPrice   to  sqrt(trainingData$totalPrice)

fit_lm_aj_1<-lm( sqrt(trainingData$totalPrice) ~ PropertySize +  bathRoom  + renovationCondition 
                  + ladderRatio  + communityAverage +  tradeyear + 
                    trademonth +PropertySize*communityAverage, 
                 data = trainingData) 
summary(fit_lm_aj_1) 

# Adjusted R-squared:  0.8302    increse again!

fit_lm_aj_2<-lm(sqrt(trainingData$totalPrice) ~ PropertySize +  bathRoom  + renovationCondition +
                   communityAverage +  tradeyear + trademonth +PropertySize*communityAverage, data = trainingData) 
summary(fit_lm_aj_2) 
# Adjusted R-squared:  0.8299 

#=====================4 regression test  ================
library(car)

 # vif(fit_lm_aj_1)
 # durbinWatsonTest(fit_lm_aj_1)
 # ncvTest(fit_lm_aj_1) --fail  
 # qqPlot(fit_lm_aj_1,id.method='identify',simulate = TRUE,labels=row.names(house),main='Q-Q plot')

par(mfrow=c(2,2))
plot(fit_lm_aj_1)
#do not  meet the   four assumptions
# There are four assumptions associated with a linear regression model:
# 1.	Linearity: The relationship between X and the mean of Y is linear.
# 2.	Homoscedasticity: The variance of residual is the same for any value of X.
# 3.	Independence: Observations are independent of each other.
# 4.	Normality: For any fixed value of X, Y is normally distributed.


#======================5  predict============================
pr.lm <- predict(fit_lm_aj_1,testtingData)
MSE.lm <- sum((pr.lm^2 - testtingData$totalPrice)^2)/nrow(testtingData)  #17061936331
 

#---since y=sqrt(totalPrice)     predict price =pr.lm^2
deviation=((testtingData$totalPrice-pr.lm^2)/testtingData$totalPrice)
comparison=data.frame(predicted=pr.lm^2,actual=testtingData$totalPrice,deviation)
accuracy=1-abs(mean(deviation))
accuracy  # 0.9034072
 