library(ISLR2)
library(MASS)
library(caret)
library(splines)
library(pROC)


attach(Wage)
head(Wage,5)

plot(age, wage, pch=19, cex=.2)
scatter.smooth(age, wage, span=.3, degree=2, pch=".")

train_model <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
train_CV <- trainControl(method = "cv", number = 10)
train_LOO <- trainControl(method = "LOOCV")


# Train the model
model.lm <- train(  wage ~ age, data = Wage, 
                  method = "lm", trControl = train_CV )
model.lm

model.poly2 <- train(  wage ~ poly(age, degree=2), data = Wage, 
                    method = "lm", trControl = train_CV )
model.poly2

model.poly3 <- train(  wage ~ poly(age, degree=3), data = Wage, 
                       method = "lm", trControl = train_CV )
model.poly3

model.poly4 <- train(  wage ~ poly(age, degree=4), data = Wage, 
                       method = "lm", trControl = train_CV )
model.poly4

model.ns4 <- train(  wage ~ ns(age, df=4), data = Wage, 
                       method = "lm", trControl = train_CV )
model.ns4

model.ns5 <- train(  wage ~ ns(age, df=5), data = Wage, 
                     method = "lm", trControl = train_CV )
model.ns5

summary(model.ns5$finalModel)

plot(age, wage, pch="")
points(age, wage, pch=19, col="grey")
lines(age[order(age)], predict(model.ns5$finalModel)[order(age)], lwd=3)
lines(age[order(age)], predict(model.lm$finalModel)[order(age)], lwd=3,
      col="blue", lty=3)





