setwd("C:/Users/asus/Desktop/tasks/634BI/project/CodeingTest")
library(tidyverse)
library(readr)  
library(lubridate)   
library(dplyr)

#---1-----------load data and seperate the tradeTime 
ModelData <- read.csv("ModelData.csv",header = TRUE )%>%select(-X)%>% 
  mutate(tradeyear = year(tradeTime),trademonth = month(tradeTime))%>%select(-tradeTime)
glimpse(ModelData)



##---------------normalized data   --------
maxs <- apply(ModelData, 2, max)
mins <- apply(ModelData, 2, min)
scaled <- as.data.frame(scale(ModelData, center = mins, scale = maxs - mins))   
#(x - min(x)) / (max(x) - min(x)

set.seed(1000)
N <- nrow(ModelData)
trainingIndex <- sample(1:N,size=0.75*N)
trainingIndex <- sort(trainingIndex)
testtingIndex <- setdiff(1:N,trainingIndex)

#---divided into trainDATA  and testDATA
train_scaled <- scaled[trainingIndex,] 
test_scaled <- scaled[testtingIndex,]


#---2---------Check the correlation between variables 
library(car)
cor_scaled <- cor(scaled,use="all.obs",method="pearson") 
#scatterplotMatrix(ModelData,lty.smooth=2,spread=FALSE,main='Scatter Plot Matrix')   #timeconsuming
write.csv(cor_scaled,"cor_allVaribles_scaled.csv")
 
 

#------3------choose predictors  
# PropertySize	NumBedroom   bathRoom   communityAverage	 tradeyear  > 0.4  chosen
# NumLivingRoom  >0.3     alternative
#renovationCondition  HasElevator   >0.2   alternative 
 

# ----- try differen lm  the result is the same as without normalized data
 


#------build  final lm 
fit=lm(totalPrice~PropertySize+NumBedroom+communityAverage+tradeyear,data=train_scaled ) 


confint(fit) 
confint(fit,level = .68)
library(car)
Anova(fit, type="II") 

###---独立性------------
#D-W检验独立性   p值大于0.05 不显著，说明因变量之间无自相关性，互相独立
durbinWatsonTest(fit)    #----未通过----

# lag Autocorrelation D-W Statistic p-value
# 1       0.2743167      1.451357       0
# Alternative hypothesis: rho != 0

 
#--同方差性---   
ncvTest(fit)  #-------未通过-----------
# lag Autocorrelation D-W Statistic p-value
# 1       0.2743167      1.451357       0
# Alternative hypothesis: rho != 0


###----共线性----------通过----，不存在多重共线性----
vif(fit)


###----正态分布-------残差分析和异常点---- 

#---0----
library(psych)
describe(fit$residuals)    
  
#   vars      n mean   sd median trimmed  mad   min  max range skew kurtosis se
# X1    1 223275    0 0.02      0       0 0.02 -0.36 0.59  0.95 1.94    25.95  0

#---1----- 
ks.test(jitter(fit$residuals),'pnorm',0,1,alternative='two.sided')  

#2 --qqPlot ---------
qqPlot(fit,id.method='identify',simulate = TRUE,labels=row.names(house),main='Q-Q plot')



 
#Removing outliers

train_scaled$residuals <- fit$residuals 


#try to Removing outliers  but failed  since this lm cannot get  p.value < .05
st <- ks.test(jitter(train_scaled$residuals),'pnorm',0,1,alternative='two.sided')
while(st$p.value < .05) {  #if pvalue is less than 0.05    
  train_scaled <- train_scaled %>% filter(abs(residuals)< max(abs(residuals)))  #filter the residuals  
  mod <- lm(totalPrice~PropertySize+NumBedroom+communityAverage+tradeyear,data=train_scaled)
  st <-  ks.test(jitter(train_scaled$residuals),'pnorm',0,1,alternative='two.sided')
}
st
mod <- lm(totalPrice~PropertySize+NumBedroom+communityAverage+tradeyear,data=train_scaled)
summary(mod)

 


#--predict------
pr.lm <- predict(fit,test_scaled)
MSE.lm <- sum((pr.lm - test_scaled$totalPrice)^2)/nrow(test_scaled)

deviation=((test_scaled$totalPrice-pr.lm)/test_scaled$totalPrice)
comparison=data.frame(predicted=pr.lm,actual=test_scaled$totalPrice,deviation)
accuracy=1-abs(mean(deviation,na.rm=T))
accuracy  #  too low    0.009122645
 

pr.nn1.P <- pr.nn1$net.result*(max(ModelData$totalPrice)-min(ModelData$totalPrice))+min(ModelData$totalPrice) 
test.p <- test_scaled$totalPrice*(max(ModelData$totalPrice)-min(ModelData$totalPrice))+min(ModelData$totalPrice) 
MSE.nn1.p <- sum((test.p - pr.nn1.P)^2)/nrow(test_scaled)
