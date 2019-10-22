library(MASS)
library(ISLR)
library(corrplot)
library(car)

Boston
Boston = Boston
summary(Boston)


Boston.cor = cor(Boston)

corrplot(Boston.cor)

# Plot
dev.new()
#par(mfrow=c(2,2))
plot(x = Boston$rm,y = Boston$medv,
     xlab = "Boston rm",
     ylab = "Boston Crim",
     main = "rm vs medv"
)

plot(x = Boston$rad,y = Boston$crim,
     xlab = "Boston rad",
     ylab = "Crim",
     main = "Rad vs Crim"
)

plot(x = Boston$nox,y = Boston$crim,
     xlab = "nox",
     ylab = "crim",
     main = "nox vs crim"
)

plot(x = Boston$lstat,y = Boston$crim,
     xlab = "Displacement",
     ylab = "Milage",
     main = "lstat vs crim"
)

hist(Boston$crim,breaks=30,xlab="crime rate"m)
hist(Boston$tax,breaks=30,xlab="tax rate")
hist(Boston$medv,breaks=30,xlab="medv")
Boston[399,]
summary(Boston)

rooms_greaterthan7=rep("No",nrow(Boston))
rooms_greaterthan7[Boston$rm >7]="Yes"
rooms_greaterthan7=as.factor(rooms_greaterthan7)
Boston_new=data.frame(Boston,rooms_greaterthan7)

#init a matrix
names(Boston) = list_columns

for i in list_columns
  print(i)

##REGRESSION PART OF LAB
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

predict(lm.fit,data.frame(lstat=c(1,2,3)), 
        interval ="confidence")

predict(lm.fit,data.frame(lstat=c(1,2,3)),
        interval = 'prediction')

plot(lstat ,medv)
abline(lm.fit)

abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))

plot(heatvalues(lm.fit))

##Multiple linear regression
lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

#fit all 13 predictors
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

lm(formula = medv~.,data=Boston)

?summary.lm

summary(lm.fit)$r.sq
summary(lm.fit)$sigma

#running regression excluding one variable
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)

#runnin regressino using interaction terms and indivdiual 
#predictors

lm.fit2=lm(medv~lstat*age,data=Boston)
summary(lm.fit2)

lm(formula = lm.fit2)

#non linear transformations of the predictor
lm.fit3 = lm(medv~lstat+I(lstat^2))
summary(lm.fit3)

lm(formula = lm.fit3)

lm.fit4 = lm(medv~lstat)
anova(lm.fit4,lm.fit3)

par(mfrow=c(2,2))
plot(lm.fit2)

#ploynomial fitting
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)
h = lm(medv~log(rm))































  
  
  
  
  