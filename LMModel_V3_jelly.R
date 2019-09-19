###################### Xiaocui Zhang #######################
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
 
library(car)
 
 


#=====================2  Check the correlation between variables ================


####--------- nominal_vars 
nominal_vars<- subset(select_if(ModelData, is.factor))
names(nominal_vars)

summary(aov(ModelData$totalPrice ~.,data = nominal_vars))
#OwnOverFiveYears   Pr(>F)=0.00191 
  
#hit:The default option in R is to use the first level of the factor as a reference and interpret the remaining levels relative to this level.
nominal_fit<-lm(ModelData$totalPrice ~.,data=nominal_vars)
summary(nominal_fit)
# buildingType2  buildingStructure*   district8 are less  relative 
 
##----codify those multi level nominal variables as dummy variables--------------- 
tt <- model.matrix(~renovationCondition+buildingStructure+buildingType+district+HasElevator+OwnOverFiveYears+HasSubway,nominal_vars)
tt0 <- data.table::data.table(tt)
names(tt0)
coding_vars<-tt0%>%select(-1)
names(coding_vars)

coding_fit<-lm(ModelData$totalPrice ~.,data=coding_vars)
summary(coding_fit)
# Adjusted R-squared:  0.2234 
# the result is the same as above   #buildingType2  buildingStructure   district8  are less  relative 

#update the model
coding_fit1<-update(coding_fit, .~. -buildingType2-district8-buildingStructure2 -buildingStructure3-buildingStructure4-buildingStructure5-buildingStructure6 )
summary(coding_fit1)
#Adjusted R-squared:  0.2203  low


part1<-data.frame(ModelData$totalPrice,coding_vars) 
glimpse(part1)
cor_coding_vars<- cor(part1,use="all.obs",method="pearson")
write.csv(cor_coding_vars,"nominal_vars.csv")

# HasElevator1  0.209454174    
# buildingStructure6  0.200399446   
# renovationCondition4  0.217976683
  
####--------- continuous_vars 

continuous_vars<- subset(select_if(ModelData, is.numeric))
names(continuous_vars)
cor <- cor(continuous_vars,use="all.obs",method="pearson") 
#scatterplotMatrix(continuous_vars,lty.smooth=2,spread=FALSE,main='Scatter Plot Matrix')  
write.csv(cor,"continuous_vars.csv")
 
 


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
#=====================3   bulid lm  ================

t <- model.matrix(~renovationCondition+buildingStructure+buildingType+district+HasElevator+OwnOverFiveYears+HasSubway,trainingData)
t0 <- data.table::data.table(t)
t1<- subset(select_if(trainingData, is.numeric))
trainingData_t<-data.frame(t0,t1)%>%select(-1)
names(trainingData_t)

# PropertySize	
# NumBedroom   
# bathRoom   
# communityAverage	 
# tradeyear  
# HasElevator1    
# renovationCondition4  
# buildingStructure6 
# NumLivingRoom
 
fit1=lm(totalPrice~PropertySize+NumBedroom+bathRoom+communityAverage+tradeyear+HasElevator1+renovationCondition4+NumLivingRoom+buildingStructure6, data=trainingData_t)
summary(fit1)
#bathRoom  pvalue =0.9733     NumBedroom   pvalue = 0.2904      insignificant  need  remove 
#Adjusted R-squared:  0.7641 

#=====================3.1  step regression lm  ================
#对lm1模型做逐步回归 
step(fit1)


fit2=update(fit1,.~.-NumBedroom-bathRoom, data=trainingData_t)
summary(fit2)
#NumLivingRoom  0.0561        nsignificant
#Adjusted R-squared:  0.7641 
 
fit3=update(fit2,.~.-NumLivingRoom, data=trainingData_t)
summary(fit3)
# Adjusted R-squared:  0.7641 

#=====================3.2   regression test  ================
library(gvlma)
gvmodel <- gvlma(fit3)
summary(gvmodel)  #---fail---

###----A  collinearity test ----------pass--
#方差膨胀因子VIF(Variance Inflation Factor)来衡量自变量之间的共线性  不超过4或5   最大10   理想中的线性模型VIF=1，表完全不存在共线性
vif(fit3)

#---if linner is reasonable
crPlots(fit3)

###---B   independent------------pass---
#D-W检验独立性   p值大于0.05 不显著，说明因变量之间无自相关性，互相独立
durbinWatsonTest(fit3)     

 

###---C constant error variance-----Homoscedasticity---   
ncvTest(fit3)  #-------fail-----------
 


###---D Normality of Error Distribution------ 
#---0--
library(psych)
describe(fit3$residuals)   --##  not ideal
  #mean  0      sd  109237.4     skew  1.94  kurtosis   25.95
#---1 ---- sample size >5000  choose Kolmogorov-Smirnov test   
ks.test(jitter(fit3$residuals),'pnorm',0,1,alternative='two.sided') 
#---2---qqPlot--- 
qqPlot(fit3,id.method='identify',simulate = TRUE,labels=row.names(house),main='Q-Q plot')
# par(mfrow=c(2,2))
# plot(fit)
plot(fit3,which = 1)
###---E outlierTest------------
outlierTest(fit3)
plot(x=fitted(fit3),y=rstudent(fit3))
abline(h=3,col="red",lty=2)
abline(h=-3,col="red",lty=2)
 



#=====================4   adjust model    ================


fit_aj=lm(totalPrice~tradeyear+HasElevator1+renovationCondition4+buildingStructure6+PropertySize*communityAverage, data=trainingData_t)
summary(fit_aj)
#Adjusted R-squared:  0.8271   increase
step(fit_aj)   # donot need  remove any any since ACI 
 

 
gvmodel_aj <- gvlma(fit_aj)
summary(gvmodel_aj)

vif(fit_aj)   #--pass---
durbinWatsonTest(fit_aj) #---pass---
ncvTest(fit_aj)#----fail-------
qqPlot(fit_aj,id.method='identify',simulate = TRUE,labels=row.names(house),main='Q-Q plot')

plot(fit_aj,which = 1)


 

#--predict------
p <- model.matrix(~renovationCondition+buildingStructure+buildingType+district+HasElevator+OwnOverFiveYears+HasSubway,testtingData)
p0 <- data.table::data.table(p)
p1<- subset(select_if(testtingData, is.numeric))
testtingData_t<-data.frame(p0,p1)%>%select(-1)
names(testtingData_t)



pr.lm <- predict(fit_aj,testtingData_t)
MSE.lm <- sum((pr.lm - testtingData_t$totalPrice)^2)/nrow(testtingData_t)   #  8674632131
 
deviation=((testtingData_t$totalPrice-pr.lm)/testtingData_t$totalPrice)
comparison=data.frame(predicted=pr.lm,actual=testtingData_t$totalPrice,deviation)
accuracy=abs(1-abs(mean(deviation)))
accuracy  #  too low     -0.01913998
 