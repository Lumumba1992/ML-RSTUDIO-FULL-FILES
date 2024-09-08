# install.packages("ISLR")
# install.packages("ISLR2")
# install.packages("caret")
# install.packages("splines")

library(ISLR2)
library(MASS)
library(caret)
library(splines)
library(pROC)



# INCOME DATA

IncomeData  = read.csv("Income2.csv", header=TRUE)
attach(IncomeData)

IncomeData

plot(Education, Income)

# fit models and calculate MSE

mod.0 = lm(Income ~ Education)
summary(mod.0)
mse.0 = mean( (Income - predict(mod.0))^2 )
mse.0

plot(Education, Income)
lines( Education, predict(mod.0))

mod.1 = lm(Income ~ poly(Education, degree=3))
summary(mod.1)
mse.1 = mean( (Income - predict(mod.1))^2 )
mse.1

mod.2 = lm(Income ~ bs(Education, degree=3))
summary(mod.2)
plot(Education, Income)
lines( Education[order(Education)], predict(mod.2)[order(Education)] )
mse.2 = mean( (Income - predict(mod.2))^2 )
mse.2


detach(IncomeData)



