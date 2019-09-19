setwd("C:/Users/asus/Desktop/tasks/634BI/project/CodeingTest")
library(tidyverse)
library(readr)  
library(lubridate)   
library(dplyr)

#---1-----------load data and seperate the tradeTime 
ModelData <- read.csv("ModelData.csv",header = TRUE )%>%select(-X)%>% 
            mutate(tradeyear = year(tradeTime),trademonth = month(tradeTime))%>%select(-tradeTime)
glimpse(ModelData)



#---2---------Check the correlation between variables 
library(car)
cor <- cor(ModelData,use="all.obs",method="pearson") 
#scatterplotMatrix(ModelData,lty.smooth=2,spread=FALSE,main='Scatter Plot Matrix')   #timeconsuming
write.csv(cor,"cor_allVaribles.csv")
 
# library(MASS)7
# lm.fit=lm(totalPrice~.-totalPrice-communityAverage-UnitPrice-tradeTime, data=ModelData)
# summary(lm.fit)


#---训练样本和测试样本----
# set.seed(1000)
# N <- nrow(ModelData)
# trainingIndex <- sample(1:N,size=0.75*N)
# trainingIndex <- sort(trainingIndex)
# testtingIndex <- setdiff(1:N,trainingIndex)
# 
# 
# trainingData=ModelData[trainingIndex,]
# testtingData=ModelData[testtingIndex,]

trainingData <- read.csv("ModelData - Training Data.csv",header = TRUE )%>%select(-X)%>% 
        mutate(tradeyear = year(tradeTime),trademonth = month(tradeTime))%>%select(-tradeTime) 

testtingData<- read.csv("ModelData - Testing Data.csv",header = TRUE )%>%select(-X)%>% 
        mutate(tradeyear = year(tradeTime),trademonth = month(tradeTime))%>%select(-tradeTime) 

 

#------------自变量进行正态性检验--往后放

#------3------choose predictors
# PropertySize	NumBedroom   bathRoom   communityAverage	 tradeyear  > 0.4  chosen
# NumLivingRoom  >0.3     alternative
#renovationCondition  HasElevator   >0.2   alternative 

fit1=lm(totalPrice~PropertySize+NumBedroom+bathRoom+communityAverage+tradeyear, data=trainingData)
summary(fit1)
#bathRoom  pvalue =0.958      insignificant  need  remove 


fit2=lm(totalPrice~PropertySize+NumBedroom+bathRoom+communityAverage+tradeyear+NumLivingRoom, data=trainingData)
summary(fit2)
#bathRoom    0.985       NumLivingRoom  0.746             insignificant


fit3=lm(totalPrice~PropertySize+NumBedroom+bathRoom+communityAverage+tradeyear+NumLivingRoom+renovationCondition+HasElevator, data= trainingData)
summary(fit3)
#bathRoom  0.898642        NumLivingRoom 0.000473   NumBedroom 0.121422         

fit4=lm(totalPrice~PropertySize+NumBedroom+communityAverage+tradeyear, data=trainingData)
summary(fit4) 
#R-squared:  0.7615 

fit5=lm(totalPrice~PropertySize+NumBedroom+communityAverage+tradeyear+renovationCondition+HasElevator, data=trainingData)
summary(fit5)
#NumBedroom    pvalue = 0.159      

fit6=lm(totalPrice~PropertySize+communityAverage+tradeyear+renovationCondition+HasElevator, data=trainingData)  
summary(fit6)
#R-squared:  0.7651 
 

# -----compare  fit4 fit6  same R-squared  but fit4 has less variables ---choose fit4

 

#------build the new lm using chosen variables  
fit=lm(totalPrice~PropertySize+NumBedroom+communityAverage+tradeyear, data=trainingData) 


confint(fit) 
confint(fit,level = .68)
library(car)
Anova(fit, type="II")   #significant pvalue 

###---独立性------------
#D-W检验独立性   p值大于0.05 不显著，说明因变量之间无自相关性，互相独立
durbinWatsonTest(fit)    #----未通过----

#lag Autocorrelation D-W Statistic p-value
#1       0.2480452      1.503902       0
#Alternative hypothesis: rho != 0

#--同方差性---   
ncvTest(fit)  #-------未通过-----------
#result like below
# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 413160.5, Df = 1, p = < 2.22e-16

###----共线性----------通过----不存在多重共线性----
#方差膨胀因子VIF(Variance Inflation Factor)来衡量自变量之间的共线性  不超过4或5   最大10   理想中的线性模型VIF=1，表完全不存在共线性
vif(fit)
 


###----正态分布-------残差分析和异常点---- http://www.xiaofandajie.top/2018/01/10/%E6%AD%A3%E6%80%81%E6%80%A7%E6%A3%80%E9%AA%8C/
#0--
library(psych)
describe(fit$residuals)   --##  not ideal
#mean  0      sd  109237.4     skew  1.94  kurtosis   25.95
#1 ---- sample size >5000  choose Kolmogorov-Smirnov test   
ks.test(fit$residuals,'pnorm',0,1,alternative='two.sided')
ks.test(jitter(fit$residuals),'pnorm',0,1,alternative='two.sided') 



trainingData$residuals <- fit$residuals 
#try to Removing outliers  but failed  since this lm cannot get  p.value < .05
st <- ks.test(jitter(trainingData$residuals),'pnorm',0,1,alternative='two.sided')
while(st$p.value < .05) {  #if pvalue is less than 0.05    
        trainingData <- trainingData %>% filter(abs(residuals)< max(abs(residuals)))  #filter the residuals  
        mod <- lm(totalPrice~PropertySize+NumBedroom+communityAverage+tradeyear,data=trainingData)
        st <-  ks.test(jitter(trainingData$residuals),'pnorm',0,1,alternative='two.sided')
}
st
mod <- lm(totalPrice~PropertySize+NumBedroom+communityAverage+tradeyear,data=train_scaled)
summary(mod)





#2 --qqPlot 
qqPlot(fit,id.method='identify',simulate = TRUE,labels=row.names(house),main='Q-Q plot')

#2 做残差分析(预测值和实际值之间的差)，检验模型的正确性，残差必须服从正态分布 直接用plot()函数生成4种用于模型诊断的图形，进行直观地分析。
par(mfrow=c(2,2))
plot(fit)
#timeconsuming

 

#--predict------
pr.lm <- predict(fit,testtingData)
MSE.lm <- sum((pr.lm - testtingData$totalPrice)^2)/nrow(testtingData)   # 12119067483

deviation=((testtingData$totalPrice-pr.lm)/testtingData$totalPrice)
comparison=data.frame(predicted=pr.lm,actual=testtingData$totalPrice,deviation)
accuracy=1-abs(mean(deviation))
accuracy  #  too low     -0.001974145
 