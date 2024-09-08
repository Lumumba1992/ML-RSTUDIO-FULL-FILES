# install.packages("ISLR")
# install.packages("ISLR2")
# install.packages("caret")
# install.packages("splines")

setwd("/Users/jhogansc/My Drive/ACTIVE/CONF/Kenya2023/June+July/MLWorkshop-2023/Code/")

library(ISLR)
library(ISLR2)
library(MASS)
library(caret)
library(splines)
library(splines2)
library(pROC)




# DEFAULT DATA
head(Default,5)
View(Default)
attach(Default)
head(Default,5)
str(Default)

y = as.numeric( default=="Yes" )
mean(y)

# fit logistic regression for different variables
mod.0 = glm( y ~ balance, family=binomial)
summary(mod.0)

mod.1 = glm( y ~ as.factor(student), family=binomial)
summary(mod.1)

mod.2 = glm( y ~ as.factor(student) + balance, family=binomial)
summary(mod.2)

mod.3 = glm( y ~ as.factor(student) + balance + income, family=binomial)
summary(mod.3)
roc_curve = roc(default ~ predict(mod.3, type="response"))
plot.roc(roc_curve, print.auc=T, print.thres=T)



# looking at classification based on p.hat = .5 cutoff
# 10-fold CV, repeated 5 times
train_model <- trainControl(method = "repeatedcv", number = 5, repeats = 10)

# Train the model
model <- train(
  default ~ balance + income + student, 
  data = Default, 
  method = "glm",
  family = "binomial",
  trControl = train_model)
model

confusionMatrix(predict(model), default, positive="Yes")


