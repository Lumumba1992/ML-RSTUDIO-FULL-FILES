library(ISLR2)
library(MASS)
library(caret)
library(splines)
library(pROC)
library(rattle)


# HEART DISEASE DATA
HeartData  = read.csv("Heart.csv", header=TRUE)
View(HeartData)

HeartData

nrow(HeartData)
HeartData = na.omit(HeartData)
nrow(HeartData)

attach(HeartData)





par(mfrow=c(1,2))

boxplot(Age ~ as.factor(AHD))
boxplot(MaxHR ~ as.factor(AHD))
boxplot(Chol ~ as.factor(AHD))
boxplot(RestBP ~ as.factor(AHD))

par(mfrow=c(1,1))

pairs( cbind( Chol, MaxHR, RestBP,Age), pch=19, lower.panel=NULL, cex=.5)



HeartData$HD = as.factor(AHD)

# looking at classification based on p.hat = .5 cutoff
# 10-fold CV, repeated 5 times
train_model <- trainControl(method = "repeatedcv", number = 5, repeats=10)


model.cart <- train( HD ~ Age + as.factor(Sex) + as.factor(ChestPain)
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



model.rf <- train(
  HD ~ Age + as.factor(Sex) + as.factor(Thal)
  + Chol + MaxHR + RestBP + Fbs + RestECG + ExAng,
  data = HeartData, 
  method = "rf",
  trControl = train_model)
model.rf

#HD ~ Chol + MaxHR + RestBP + as.factor(Thal) + ExAng, 

summary(model.rf$finalModel)

model.rf$finalModel

plot(model.rf$finalModel)

varImp(model.rf$finalModel)
plot( varImp(model.rf) )

yhat = 1-predict(model.rf$finalModel, type="prob")[,1]
plot(HeartData$MaxHR, yhat)


scatter.smooth(HeartData$MaxHR, yhat, span=.4) 
scatter.smooth(HeartData$Age, yhat, span=.4) 
scatter.smooth(HeartData$RestBP, yhat, span=.4) 
scatter.smooth(HeartData$Chol, yhat, span=.4) 
boxplot(yhat ~ as.factor(HeartData$Thal))
boxplot(yhat ~ as.factor(HeartData$Sex))

confusionMatrix(predict(model.rf, HeartData), 
                reference=HeartData$HD, positive="Yes")

detach(HeartData)
