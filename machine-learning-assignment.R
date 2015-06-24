library(MASS)
library(ISLR)
library(car)

fix(Boston)
names(Boston)
lm.fit = lm(medv~lstat,Boston)

attach(Boston)

lm.fit <- lm(medv ~ lstat)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval="confidence")

# plotting the least squares regression
plot(lstat,medv)
abline(lm.fit)

#alternative plots to explore non-linear relations
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)
par(mfrow=c(2,2))
plot(lm.fit)

#Residual plots
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))

# Compute leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Regression using multiple predictors
lm.fit <- lm(medv~lstat+age,Boston)
summary(lm.fit)

lm.fit <- lm(medv~.,Boston)
summary(lm.fit)

summary(lm.fit)$r.sq # R^2 summary
summary(lm.fit)$sigma # RSE summary
vif(lm.fit) # Variance Inflation factor (car package)

# Regression with all predictors on medv execept age
lm.fit1 <- lm(medv~. -age,Boston) 
summary(lm.fit1)

lm.fit1 <- update(lm.fit, ~. -age) #re-fit regression using Update()

#Adding interaction terms
summary(lm(medv~lstat*age,Boston))

#Non-linear transformations of the predictors
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

# Use anova to improve model
lm.fit <- lm(medv~lstat)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

# Using poly() to fit higher order fit
lm.fit5 <- lm(medv ~ poly(lstat,5))
summary(lm.fit5)

#fitting a model using a log transformation
summary(lm(medv~log(rm),Boston))

# Qualitative predictors
fix(Carseats)
names(Carseats)

lm.fit =lm(Sales~.+ Income : Advertising + Price : Age , data = Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc) 

# Machine learning assignment Random forest example
library(caret)
library(ggplot2)
dtrain <- read.table("pml-training.csv",sep=",",header=T,na.strings="NA")

inTrain <- createDataPartition(y=dtrain$user_name,p=0.7,list=FALSE)
training <- dtrain[inTrain,]
testing <- dtrain[-inTrain,]

modFit <- train(dtrain~ ., data=training,method="rf",prox=TRUE,na.rm=TRUE)
modFit

getTree(modFit$finalModel,k=2)
