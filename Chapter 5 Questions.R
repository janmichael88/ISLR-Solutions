#Question 2
pr = function(n){
  return(1 - (1 - (1/n))^n)
}
x=1:1e+05
plot(x,pr(x))

store=rep(NA, 10000) 
for(i in 1:10000) {store[i]=sum(sample(1:100, rep=TRUE)==89)>0 }
mean(store)

#Question 3
library(ISLR)
summary(Default)

#fit regression model
attach(Default)
set.seed(1)
glm.fit = glm(default~income+balance,data=Default,
              family=binomial)
summary(glm.fit)

FiveB = function() {
  # i.
  train = sample(dim(Default)[1], dim(Default)[1]/2)
  # ii.
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                subset = train)
  # iii.
  glm.pred = rep("No", dim(Default)[1]/2)
  glm.probs = predict(glm.fit, Default[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # iv.
  return(mean(glm.pred != Default[-train, ]$default))
}

new_function = function(){
  # i.
  train = sample(dim(Default)[1], dim(Default)[1]/2)
  # ii.
  glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, 
                subset = train)
  # iii.
  glm.pred = rep("No", dim(Default)[1]/2)
  glm.probs = predict(glm.fit, Default[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # iv.
  return(mean(glm.pred != Default[-train, ]$default))
}

#Question 6
library(ISLR)
summary(Default)
attach(Default)

set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)
coef(glm.fit)

boot_function = function(data,index){
  return(coef(glm(default~income+balance,data=data,family=binomial,
             subset=index)))
}

library(boot)
boot(Default, boot_function, 50)

#Question 7
library(ISLR)
summary(Weekly)
attach(Weekly)
set.seed(1)

glm.fit = glm(Direction~Lag1+Lag2,data=Weekly,family = binomial)
summary(glm.fit)

#drop first two observations
glm.fit = glm(Direction~Lag1+Lag2,data=Weekly[-1,],family = binomial)
summary(glm.fit)

#predict the first observation
prediction = predict.glm(glm.fit,Weekly[1,],type = 'response') > 0.5
(prediction == Weekly[1,9])

#looping over samples to get errors
#dropping ith observation from model
count =rep(0,dim(Weekly)[1])
for (i in 1:(dim(Weekly)[1])){
  glm.fit=glm(Direction~Lag1+Lag2,data=Weekly[-i,],
              family=binomial)
  direction_up = predict(glm.fit,Weekly[i,],type='response') > 0.4
  direction_true_up = Weekly[i,]$Direction == 'Up'
  if (direction_up!=direction_true_up){
    count[i] = 1
  }
  
}
sum(count)
mean(count)

#Question 8
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2*x^2 + 1.1*rnorm(100)
plot(x,y)

library(boot)
Data_xy = data.frame(x,y)
set.seed(5)

powers_to_test = 6
results_mat = matrix(0,powers_to_test,2)

for (i in 1:powers_to_test){
  glm.fit=glm(y~poly(x,i))
  results_mat[i,1] = cv.glm(Data_xy,glm.fit)$delta[1]
  results_mat[i,2] = cv.glm(Data_xy,glm.fit)$delta[2]
}

glm.fit=glm(y~poly(x,1))
cv.glm(Data_xy,glm.fit)$delta

set.seed(56)

powers_to_test = 6
results_mat = matrix(0,powers_to_test,2)

for (i in 1:powers_to_test){
  glm.fit=glm(y~poly(x,i))
  results_mat[i,1] = cv.glm(Data_xy,glm.fit)$delta[1]
  results_mat[i,2] = cv.glm(Data_xy,glm.fit)$delta[2]
}
results_mat

summary(glm.fit)

#exercise 9
library(MASS)
summary(Boston)
attach(Boston)

medv.mean = mean(medv)
medv.mean

medv.err = sd(medv) / sqrt(length(medv))
medv.err

boot_function = function(data,variable){
  return(mean(data[variable]))
}

library(boot)
bootstrap = boot(medv,boot_function,2000)
bootstrap

t.test(medv)

c(bootstrap$t0 - 2 * 0.4119, bootstrap$t0 + 2 * 0.4119)

medv.med = median(medv)
medv.med

boot_function2 = function(data,variable){
  return(mean(data[variable]))
}

boot(medv,boot_function2,2000)

medv.tenth = quantile(medv,c(0.1))
medv.tenth

boot_function3=function(data,variable){
  return(quantile(data[index],c(0.1)))
}
boot(medv,boot_function3,2000)




































