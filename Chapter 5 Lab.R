library(ISLR)
set.seed(1)
train = sample(396,192)
Auto=Auto
attach(Auto)

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
summary(lm.fit)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,3),data=Auto,subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

set.seed(2)
train = sample(392,214)
lm.fit3=lm(mpg~horsepower,data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#leave one-out-cross validation
#perform logistic regression using glm, but don't pass fam arg
glm.fit = glm(mpg~horsepower,data=Auto)
coef(glm.fit)

lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

#get cv errors for increasing polynomial orders
cv.error = rep(0,6)
for (i in 1:length(cv.error)){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

#k fold cross validation
set.seed(45)
cv.error.13 = rep(0,10)
for (i in 1:length(cv.error.13)){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.13[i] = cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.13


#the bootstrap
library(ISLR)
Portfolio=Portfolio

alpha.func=function(data,index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.func(Portfolio,1:40)

#get samples from porfolio with replacement
set.seed(1)
alpha.func(Portfolio,sample(100,100,replace = T))

boot(Portfolio,alpha.func,R=1000)

boot_function = function(data,index){
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
}

boot_function(Auto,1:392)

set.seed(1)
boot_function(Auto,sample(392,392,replace=T))

boot(Auto,boot_function,1000)

boot_function_2 =function(data,index){
  coef(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
}
set.seed(1)
boot(Auto,boot_function_2,1000)

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

  
  
  














