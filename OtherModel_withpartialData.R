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

trainingData<-trainingData[1:3000,]
testtingData_0<-testtingData[1:1000,]   

#====================2 GLM============================
library(MASS) 
glm.fit <- glm(totalPrice ~.-UnitPrice-HasElevator-OwnOverFiveYears-HasSubway,data=trainingData, family=gaussian)  
step(glm.fit)

#----AIC: 77472.23
glm.fit <-  glm(totalPrice ~ followers + PropertySize + NumBedroom + kitchen + 
                  bathRoom + constructionTime + renovationCondition + buildingStructure + 
                  ladderRatio + district + communityAverage + tradeyear + trademonth, 
                family = gaussian, data = trainingData)


summary(glm.fit)

# large pvales ,so remove   kitchen     constructionTime    buildingStructure      district    

glm.fit <- glm( totalPrice ~ followers + PropertySize + NumBedroom + bathRoom+ladderRatio
                    + renovationCondition +communityAverage + tradeyear + trademonth,   family = gaussian, data = trainingData )  
summary(glm.fit)
  
# large pvales ,so remove  followers   NumBedroom    # 
glm.fit <- glm( totalPrice ~ PropertySize  + bathRoom+ladderRatio
                + renovationCondition +communityAverage + tradeyear + trademonth,   family = gaussian, data = trainingData )  
summary(glm.fit)

# SO the final preditors are    
##  PropertySize  bathRoom  renovationCondition  communityAverage   ladderRatio  tradeyear   trademonth  
 

#  AIC: 77606  but less predictors 

pr.glm <- predict(glm.fit,testtingData_0)
MSE.glm <- sum((pr.glm - testtingData_0$totalPrice)^2)/1000


deviation=((testtingData_0$totalPrice-pr.glm)/testtingData_0$totalPrice)
comparison=data.frame(predicted=pr.glm,actual=testtingData_0$totalPrice,deviation)
accuracy=1-abs(mean(deviation))
accuracy    #0.8973083

 


## try to change    trainingData$totalPrice   to  sqrt(trainingData$totalPrice)

glm.fit_aj <- glm(formula = sqrt(trainingData$totalPrice) ~  PropertySize  + bathRoom+ladderRatio
                  + renovationCondition +communityAverage + tradeyear + trademonth,   family = gaussian, data = trainingData)
summary(glm.fit_aj)

#AIC: 34069
 
 
 
pr.glm <- predict(glm.fit_aj,testtingData_0)
MSE.glm <- sum((pr.glm^2 - testtingData_0$totalPrice)^2)/nrow(testtingData_0)

deviation=((testtingData_0$totalPrice-pr.glm^2)/testtingData_0$totalPrice)
comparison=data.frame(predicted=pr.glm^2,actual=testtingData_0$totalPrice,deviation)
accuracy=abs(1-abs(mean(deviation)))
accuracy    # 0.9040567    ---  the best in GLM  


#--add   PropertySize*communityAverage  
glm.fit_aj_1 <- glm(formula = sqrt(trainingData$totalPrice) ~   PropertySize  + bathRoom+ladderRatio
                    + renovationCondition +communityAverage + tradeyear + trademonth+PropertySize*communityAverage  ,   family = gaussian, data = trainingData)
summary(glm.fit_aj_1)

# AIC:  33775   decrease 
pr.glm <- predict(glm.fit_aj_1,testtingData_0)
MSE.glm <- sum((pr.glm^2 - testtingData_0$totalPrice)^2)/nrow(testtingData_0)


deviation=((testtingData_0$totalPrice-pr.glm^2)/testtingData_0$totalPrice)
comparison=data.frame(predicted=pr.glm^2,actual=testtingData_0$totalPrice,deviation)
accuracy=abs(1-abs(mean(deviation)))
accuracy    # 0.9034072     not  increase    


# #  remove the ladderRatio
# glm.fit_aj_2 <- glm(formula = sqrt(trainingData$totalPrice) ~   PropertySize  + bathRoom
#                     + renovationCondition +communityAverage + tradeyear + trademonth+PropertySize*communityAverage  ,   family = gaussian, data = trainingData)
# summary(glm.fit_aj_2)
# 
# #aic=   33779
# 
# pr.glm <- predict(glm.fit_aj_2,testtingData_0)
# MSE.glm <- sum((pr.glm^2 - testtingData_0$totalPrice)^2)/nrow(testtingData_0)
# 
# 
# deviation=((testtingData_0$totalPrice-pr.glm^2)/testtingData_0$totalPrice)
# comparison=data.frame(predicted=pr.glm^2,actual=testtingData_0$totalPrice,deviation)
# accuracy=abs(1-abs(mean(deviation)))
# accuracy    # 0.9017109


#so  the  final glm is glm.fit_aj   AND THE  accuracy  IS 0.9034072
glm.fit_aj <- glm(formula = sqrt(trainingData$totalPrice) ~   PropertySize  + bathRoom+ladderRatio
                  + renovationCondition +communityAverage + tradeyear +
                    trademonth+PropertySize*communityAverage,   
                  family = gaussian, data = trainingData)
 
             
#================3 decision  tree======================
library(rpart)
library(rpart.plot)

fit.r <- rpart(totalPrice~.-UnitPrice,data=trainingData)
rpart.plot(fit.r, type = 4)
pr.r <- predict(fit.r,newdata=testtingData_0)
MSE.r <- sum((pr.r - testtingData_0$totalPrice)^2)/nrow(testtingData_0)  
deviation=((testtingData_0$totalPrice-pr.r)/testtingData_0$totalPrice)
comparison=data.frame(predicted=pr.r,actual=testtingData_0$totalPrice,deviation)
accuracy=1-abs(mean(deviation))
accuracy   # 0.7942522

#the 3 predictors are PropertySize tradeyear  communityAverage    

#the cptable element of rpart object call tell us if the tree should be ¡°pruned¡±
plotcp(fit.r)
print(fit.r$cptable)

#we preserve the minimum xerror in opt(min  xerror to opt)
opt<-which.min(fit.r$cptable[,"xerror"])
#here we prune back the large initial tree: prune  
cp<-fit.r$cptable[opt,"CP"]


fit.r_prune<-prune(fit.r,cp=cp)
#then we plot the resulting pruned tree 
rpart.plot(fit.r_prune, type = 4)   # the same as before     so the   same accuracy
pr.r <- predict(fit.r_prune,newdata=testtingData_0)
MSE.r <- sum((pr.r - testtingData_0$totalPrice)^2)/nrow(testtingData_0)  
deviation=((testtingData_0$totalPrice-pr.r)/testtingData_0$totalPrice)
comparison=data.frame(predicted=pr.r,actual=testtingData_0$totalPrice,deviation)
accuracy=1-abs(mean(deviation))
accuracy   #0.7942522   becasue pruned tree is the same aa the oragial one 



#================4  random  Forest ======================
library("randomForest")

rf_train<-randomForest(totalPrice~.-UnitPrice,data=trainingData,ntree=500,importance=TRUE)
importance<-importance(rf_train) 
#ntree the number of decision trees£¬  default 500

# %IncMSE IncNodePurity
# followers           20.385592  4.572106e+12
# PropertySize        55.717414  3.150961e+13
# NumBedroom          24.648685  9.719166e+12
# NumLivingRoom       14.497484  1.863724e+12
# kitchen              2.345840  5.433792e+10
# bathRoom            16.234132  6.746617e+12
# PropertyLevel       12.419880  2.816069e+12
# buildingType         5.987090  6.957742e+11
# constructionTime    20.132178  3.245596e+12
# renovationCondition 19.272221  3.191123e+12
# buildingStructure   15.235314  1.820523e+12
# ladderRatio         10.735610  2.221239e+12
# HasElevator         11.460433  7.949164e+11
# OwnOverFiveYears     7.149584  5.005425e+11
# HasSubway            6.478981  4.566966e+11
# district            28.682720  1.043227e+13
# communityAverage    75.981844  2.918091e+13
# tradeyear           98.295399  2.016666e+13
# trademonth          18.554104  2.473865e+12
# sum_feather          8.545566  1.021889e+12



barplot(rf_train$importance[,1],main="the contrast of predictors's importance ")
box()                      
 

#set aside the first 1000 rows as a test set
                      
pr.r <- predict(rf_train,newdata=testtingData_0)
MSE.r <- sum((pr.r - testtingData_0$totalPrice)^2)/nrow(testtingData_0)  
deviation=((testtingData_0$totalPrice-pr.r)/testtingData_0$totalPrice)
comparison=data.frame(predicted=pr.r,actual=testtingData_0$totalPrice,deviation)
accuracy= 1-abs(mean(deviation)) 
accuracy    #  0.9151714

######
#randomForest  get the highest  accuracy
#the top 6 important  predictors are :tradeyear communityAverage  PropertySize  district  NumBedroom  followers
######





#set aside the second  1000 rows as a test set                      
testtingData_1<-testtingData[1001:2000,]                      
pr.r <- predict(rf_train,newdata=testtingData_1)
MSE.r <- sum((pr.r - testtingData_1$totalPrice)^2)/nrow(testtingData_1)  
deviation=((testtingData_1$totalPrice-pr.r)/testtingData_1$totalPrice)
comparison=data.frame(predicted=pr.r,actual=testtingData_1$totalPrice,deviation)
accuracy= 1-abs(mean(deviation)) 
accuracy    # 0.9467512                       

(0.9151714+0.9467512)/2   #0.9139995

           
                       
                       