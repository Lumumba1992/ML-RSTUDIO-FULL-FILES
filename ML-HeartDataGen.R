
### Set up the work directory
setwd("/Users/user/Desktop/Biostatistics WorkShop/")

### Load the following libraries
library(ISLR2)
library(MASS)
library(caret)
library(splines)
library(pROC)
library(rattle)


# HEART DISEASE DATA
HeartData  = read.csv("Heart.csv", header=TRUE)

HeartData = na.omit(HeartData)


attach(HeartData)

# View(HeartData)



par(mfrow=c(2,2))
boxplot(Age ~ as.factor(AHD))
boxplot(MaxHR ~ as.factor(AHD))
boxplot(Chol ~ as.factor(AHD))
boxplot(Age ~ as.factor(AHD))
par(mfrow=c(1,1))

pairs( cbind( Chol, MaxHR, RestBP,Age), pch=19, lower.panel=NULL, cex=.5)



HeartData$HD = as.factor(AHD)

# looking at classification based on p.hat = .5 cutoff
# 10-fold CV, repeated 5 times
train_model <- trainControl(method = "repeatedcv", number = 5, repeats=10)


model.cart <- train(
  HD ~ Age + as.factor(Sex) + as.factor(Thal)
        + Chol + MaxHR + RestBP + Fbs + RestECG + ExAng, 
  data = HeartData, 
  method = "rpart",
  trControl = train_model)

model.cart

model.cart$finalModel

confusionMatrix(predict(model.cart, HeartData), 
                reference=HeartData$HD, positive="Yes")


# summary(model.cart$finalModel)

fancyRpartPlot(model.cart$finalModel)

#cart model for under 50 vs over 50

#train on everyone
model.cartl50 <- train(
  HD ~ Age + as.factor(Sex) + as.factor(Thal)
  + Chol + MaxHR + RestBP + Fbs + RestECG + ExAng, 
  data = HeartData, 
  method = "rpart",
  trControl = train_model)

# Below we will examine the performance on the two age groups. Is there a disparity? In what cases does this matter?

#predict on under 50
  confusionMatrix(predict(model.cartl50, HeartData[HeartData$Age<50,]), 
                reference=HeartData[HeartData$Age<50,]$HD, positive="Yes")

#predict on over 50
confusionMatrix(predict(model.cartl50, HeartData[HeartData$Age>=50,]), 
                reference=HeartData[HeartData$Age>=50,]$HD, positive="Yes")

# To unpack why there is a difference, let's look at variable distribution differences by age
par(mfrow=c(2,2))
boxplot(MaxHR[HeartData$Age<50] ~ as.factor(AHD[HeartData$Age<50]),ylim=c(80,200))
boxplot(MaxHR[HeartData$Age>=50] ~ as.factor(AHD[HeartData$Age>=50]),ylim=c(80,200))
boxplot(Chol[HeartData$Age<50] ~ as.factor(AHD[HeartData$Age<50]),ylim=c(100,400))
boxplot(Chol[HeartData$Age>=50] ~ as.factor(AHD[HeartData$Age>=50]),ylim=c(100,400))

#categorical variable distribution differences
par(mfrow=c(1,2))
plot(as.factor(Thal[HeartData$Age<50]), xlab="Less than 50", ylab="Count")
plot(as.factor(Thal[HeartData$Age>=50]), xlab="50 and older", ylab="Count")

detach(HeartData)
