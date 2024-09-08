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
View(IncomeData)

plot(cbind(Education,Income)) 
lines( lowess(Income ~ Education, f=.5) )
scatter.smooth(Income ~ Education, span=.5, degree=2)
pairs(cbind(Income, Seniority, Education), pch=19, lower.panel=NULL, cex=.5)

detach(IncomeData)




# DEFAULT DATA
# This dataset is in the ISLR2 package
attach(Default)

View(Default)

Default
mean(default)
mean(income)
mean(balance)
boxplot(balance ~ as.factor(default))

detach(Default)




# HEART DISEASE DATA
HeartData  = read.csv("Heart.csv", header=TRUE)

attach(HeartData)

View(HeartData)

boxplot(Age ~ as.factor(AHD))
pairs( cbind( Chol, MaxHR, RestBP), pch=19, lower.panel=NULL, cex=.5)

detach(HeartData)




