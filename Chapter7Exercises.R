#chaper 7 lab, non-linear model fitting
library(ISLR)
attach(Wage)

fit<-lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

#check fit 2 for change of basis
fit2<-lm(wage~poly(age,4,raw=T),data=Wage)

fit2a<-lm(wage~age+I(age^2)+I(age^3)+I(age)^4,data=Wage)
coef(fit2a)
#same method but different syntax
fit2b<-lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
coef(fit2b)

#predict on certain ages
agelims<-range(Wage$age)
age.grid<-seq(from=agelims[1],to=agelims[2])
preds<-predict(fit,newdata = list(age=age.grid),se=TRUE)
#two times se
se.bands<-cbind(preds$fit+2*preds$se.fit,preds$fit - 2*preds$se.fit)
#plot the data with errors
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col='darkgrey')
title("Deg 4,poly regression",outer=T)
lines(age.grid,preds$fit,lwd2=2,col='blue')
matlines(age.grid,se.bands,lwd=1,cole='blue',lty=3)

preds2<-predict(fit2,newdata = list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))
#should be close to zero

#test which model is better
#seek whether simplest model is indeed a better fit
#fit multiple models
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
#fit is better with 3 or 4 order
#test with othet terms in the model
fit.1<-lm(wage~education+age,data=Wage)
fit.2<-lm(wage~education+poly(age,2),data=Wage)
fit.3<-lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)
#fit whether age is greater than 250
#glm model
fit<-glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
preds<-predict(fit,newdata = list(age=age.grid),se=T)

#get logit odds by transforming the predictions from the lienar model
pfit<-exp(preds$fit) / (1+exp(preds$fit))
se.bands.logit<-cbind(preds$fit + 2*preds$se.fit,preds$fit - 2*preds$se.fit)
se.bands<- exp(se.bands.logit) / (1+exp(se.bands.logit))
#plot it
plot(age,I(wage>250),xlim=agelims ,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),cex=.5,pch="|",
         col =" darkgrey ")
lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
#fit a step function
table(cut(age,4)) #gives counts of age bins
fit<-lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

#splinesssss
library(splines)
fit<-lm(wage~bs(age,knots = c(20,40,60)),data=Wage)
pred<-predict(fit,newdata = list(age=age.grid),se=TRUE)
plot(age,wage,col='gray')
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty='dashed')
lines(age.grid,pred$fit-2*pred$se,lty='dashed')
#cubic spline with 3 knots has seven degrees of freedom
bs(age,knots = c(25,40,60))
bs(age,df=6)
attr(bs(age,df=6),"knots")
#fitting natural spline
fit2<-lm(wage~ns(age,df=4),data=Wage)
pred2<-predict(fit2,newdata = list(age = age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)
#fit a smoothing spine
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit<-smooth.spline(age,wage,df=16)
fit2<-smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#local regression
plot(age,wage,xlim=agelims ,cex=.5,col="darkgrey")
title(" Local Regression ")
fit<-loess(wage~age,span=.2,data=Wage)
fit2<-loess(wage~age,span=.5,data=Wage)
fit3<-loess(wage~age,span=1,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)), col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)), col="blue",lwd=2)
lines(age.grid,predict(fit3,data.frame(age=age.grid)), col="black",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"), col=c("red","blue"),lty=1,lwd=2,cex=.8)

#GAM
gam1<-lm(wage~ns(year ,4)+ns(age ,5)+education ,data=Wage)
library(gam)
gam.m3<-gam(wage~s(year ,4)+s(age ,5)+education ,data=Wage)
par(mfrow(c(1,3)))
plot(gam.m3,se=TRUE,col='blue')

#compare models with ANOVA tests
gam.m1<-gam(wage~s(age ,5)+education ,data=Wage)
gam.m2<-gam(wage~year+s(age ,5)+education ,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
summary(gam.m3)
#large p value on (year,4), must be linear with year
preds<-predict(gam.m2,newdata = Wage)

#using lo
gam.lo<-gam(wage~s(year,df=4)+lo(age,span=0.7)+education, data=Wage)
plot(gam.lo, se=TRUE, col="green")

gam.lo.i<-gam(wage~lo(year,age,span=0.5)+education,data=Wage)
library(akima)
plot(gam.lo.i)

###
#chaper 7 lab, non-linear model fitting
library(ISLR)
library(boot)
attach(Wage)
set.seed(1)
#CV is 10 fold
all.deltas<-rep(NA,10)
for (i in 1:10){
  #fit a glm model
  glm.fit<-glm(wage~poly(age,i),data=Wage)
  #add to list deltas
  all.deltas[i]<-cv.glm(Wage,glm.fit,K=10)$delta[2]
}

plot(1:10, all.deltas, xlab="Degree", ylab="CV error", type="l", pch=20, lwd=2, ylim=c(1590, 1700))
min.point = min(all.deltas)
sd.points = sd(all.deltas)
abline(h=min.point + 0.2 * sd.points, col="red", lty="dashed")
abline(h=min.point - 0.2 * sd.points, col="red", lty="dashed")
legend("topright", "0.2-standard deviation lines", lty="dashed", col="red")
#degree 3 is the smallest degree of polynomia with reasonably small cv eroor
#noe use anva to find the best model
fit.1<-lm(wage~poly(age,1),  data=Wage)
fit.1<-lm(wage~poly(age, 1), data=Wage)
fit.2<-lm(wage~poly(age, 2), data=Wage)
fit.3<-lm(wage~poly(age, 3), data=Wage)
fit.4<-lm(wage~poly(age, 4), data=Wage)
fit.5<-lm(wage~poly(age, 5), data=Wage)
fit.6<-lm(wage~poly(age, 6), data=Wage)
fit.7<-lm(wage~poly(age, 7), data=Wage)
fit.8<-lm(wage~poly(age, 8), data=Wage)
fit.9<-lm(wage~poly(age, 9), data=Wage)
fit.10<-lm(wage~poly(age, 10), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)
#anything with degree four or more is not signigicant
plot(wage~age,data=Wage,col="darkgrey")
agelims<-range(Wage$age)
age.grid<-seq(from=agelims[1], to=agelims[2])
lm.fit<-lm(wage~poly(age,3),data=Wage)
lm.pred<-predict(lm.fit,data.frame(age=age.grid))
lines(age.grid,lm.pred,col='blue',lwd=2)

#bin, using step function
all.cvs<-rep(NA,10)
for (i in 2:10){
  #create new column to bin by i cuts
  Wage$age.cut<-cut(Wage$age,i)
  #fit the model
  lm.fit<-glm(wage~age.cut,data = Wage)
  all.cvs[i]<-cv.glm(Wage,lm.fit,K=10)$delta[2]
}

plot(2:10, all.cvs[-1], xlab="Number of cuts", ylab="CV error", type="l", pch=20, lwd=2)
#up to 8 cuts

lm.fit = glm(wage~cut(age, 8), data=Wage)
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.pred = predict(lm.fit, data.frame(age=age.grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, lm.pred, col="red", lwd=2)

#exercise 7
set.seed(1)
summary(Wage$maritl)
summary(Wage$jobclass)

par(mfrow=c(1,2))
plot(Wage$maritl,Wage$wage)
plot(Wage$jobclass,Wage$wage)
#married couples make more money and information related jobs make more money

#polynomial and step functions
fit<-lm(wage~maritl,data=Wage)
deviance(fit)
fit1<-lm(wage~jobclass,data=Wage)
deviance(fit1)
fit2<-lm(wage~maritl + jobclass,data=Wage)
deviance(fit2)

#splines cannot be fit with categorical
#GAMS
library(gam)
fit<-gam(wage ~ maritl + jobclass + s(age, 4), data = Wage)
deviance(fit)
#exercise 8
#pull the auto data
pairs(Auto)
#mpg appears inversley proporitanl to cylinders, displacemnet, horsepower, and weight
rss<-rep(NA,10)
fits<-list()
for (d in 1:10){
  fits[[d]]<-lm(mpg~poly(displacement,d),data=Auto)
  rss[d]<-deviance(fits[[d]])
}
rss
anova(fits[[1]],fits[[2]],fits[[3]])
library(glmnet)
library(boot)
#check errors for different models of varying increasing polynomial order
cv.errs<-rep(NA,15)
for (d in 1:15){
  fit<-glm(mpg~poly(displacement, d),data=Auto)
  cv.errs[d]<-cv.glm(Auto, fit,K=10)$delta[2]
}
plot(cv.errs)
#min at 10
#now for step functions
cv.errs<-rep(NA,10)
for (c in 2:10){
  #create new cut bin
  Auto$dis.cut<-cut(Auto$displacement,c)
  fit<-glm(mpg~dis.cut,data = Auto)
  cv.errs[c]<-cv.glm(Auto,fit,K=10)$delta[2]
}
plot(cv.errs)
#nine has the lowest vs score

#splines
library(splines)
cv.errs = rep(NA, 10)
for (df in 3:10) {
  fit = glm(mpg ~ ns(displacement, df = df), data = Auto)
  cv.errs[df] = cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(cv.errs)

fit = gam(mpg ~ s(displacement, 4) + s(horsepower, 4), data = Auto)
summary(fit)

#exercise 9
library(MASS)
attach(Boston)

lm.fit<-lm(nox~poly(dis,3),data=Boston)
summary(lm.fit)

dislim<-range(dis)
dis.grid<-seq(from=dislim[1],to=dislim[2],by=0.1)
lm.pred<-predict(lm.fit,list(dis=dis.grid))
plot(nox~dis,data=Boston,col='darkgrey')
lines(dis.grid,lm.pred,col="red",lwd=2)
#plot fits very well
#fit polynomials of degrees 1 to 10
all.rss<-rep(NA,10)
for (i in 1:10){
  lm.fit<-lm(nox~poly(dis,i),data=Boston)
  all.rss[i]<-sum(lm.fit$residuals^2)
}

plot(all.rss)

#use 10 fold cv to bick the best polynomial degree
library(boot)
all.delta<-rep(NA,10)
for (i in 1:10){
  glm.fit<-glm(nox~poly(dis,i),data=Boston)
  all.deltas[i]<-cv.glm(Boston,glm.fit,K=10)$delta[2]
}
plot(1:10, all.deltas, xlab = "Degree", ylab = "CV error", type = "l", pch = 20, 
     lwd = 2)

#regress nox using dis, but with splines, basis function df 4, and three knots
sp.fit<-lm(nox~bs(dis,df=4),knots=c(4,7,11),data=Boston)
summary(sp.fit)

sp.pred = predict(sp.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, sp.pred, col = "red", lwd = 2)

#all parameters are signigicant and points seem to touch the line very well

#fit regression splines from 3 to 16
all.cv<-rep(NA,16)
for (i in 3:16){
  lm.fit<-lm(nox~bs(dis,df=i),data=Boston)
  all.cv[i]<-sum(lm.fit$residuals^2)
}

plot(all.cv)
#decreases montonicaly until we get to 4
#use 10 fold cv to find the best model df
all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = glm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = cv.glm(Boston, lm.fit, K = 10)$delta[2]
}

plot(3:16, all.cv[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "CV error")

#exercise 10
set.seed(1)
library(ISLR)
library(leaps)
attach(College)
train<-sample(length(Outstate),length(Outstate)/2)
test<- -train
College.train<-College[train,]
College.test<-College[test,]
reg.fit<-regsubsets(Outstate~.,data=College.train,nvmax=17,method="forward")
reg.summary<-summary(reg.fit)

par(mfrow=c(1,3))
plot(reg.summary$cp,xlab="Number of Variables",ylab="cp",type="l")
min.cp<-min(reg.summary$cp)
std.cp<-sd(reg.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.2 * std.cp, col = "red", lty = 2)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic = min(reg.summary$bic)
std.bic = sd(reg.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.2 * std.bic, col = "red", lty = 2)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", 
     type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(reg.summary$adjr2)
std.adjr2 = sd(reg.summary$adjr2)
abline(h = max.adjr2 + 0.2 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "red", lty = 2)

#6 variables if the optimum
reg.fit<-regsubsets(Outstate~.,data=College,method = 'forward')
coefi<-coef(reg.fit,id=6)
names(coefi)

#fit gam on outofstate
gam.fit = gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) + 
                s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data = College.train)
par(mfrow = c(2, 3))
plot(gam.fit, se = T, col = "blue")

gam.pred = predict(gam.fit, College.test)
gam.err = mean((College.test$Outstate - gam.pred)^2)
gam.err

#exercise 11
#create variables Y = -2.1 + 1.3X1 + 0.54X2
set.seed(1)
X1<-rnorm(100)
X2<-rnorm(100)
eps<-rnorm(100,sd=0.1)
Y = -2.1 + 1.3*X1 + 0.54*X2 +eps

#create list of 1000 b0,b1,b2 and initilize the first b1 to 10
beta0<-rep(NA,1000)
beta1<-rep(NA,1000)
beta2<-rep(NA,1000)
beta1[1]<- 10

#accumulate results of 1000 iterations in the beta arrays
for (i in 1:1000){
  #create a, which is (beta0 + beta2 + eps)
  a<- Y - beta1[i]*X1
  #grab the coeficient of lin regress a on to X2, get the new X2
  beta2[i]<-lm(a~X2)$coef[2]
  #set a using X2
  a<- Y - beta2[i]*X2
  #fit a onto X1
  lm.fit<-lm(a~X1)
  if (i < 1000){
    beta1[i + 1]<-lm.fit$coef[2]
  }
  beta0[i]<-lm.fit$coef[1]
}

plot(1:1000, beta0, type = "l", xlab = "iteration", ylab = "betas", ylim = c(-2.2, 
                                                                             1.6), col = "green")
lines(1:1000, beta1, col = "red")
lines(1:1000, beta2, col = "blue")
legend("center", c("beta0", "beta1", "beta2"), lty = 1, col = c("green", "red", 
                                                                "blue"))
#coefficients quickly attain their least square values

#compare with multiple regression
lm.fit<-lm(Y~X1 + X2)

plot(1:1000, beta0, type = "l", xlab = "iteration", ylab = "betas", ylim = c(-2.2, 
                                                                             1.6), col = "green")
lines(1:1000, beta1, col = "red")
lines(1:1000, beta2, col = "blue")
abline(h = lm.fit$coef[1], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))
abline(h = lm.fit$coef[2], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))
abline(h = lm.fit$coef[3], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))
legend("center", c("beta0", "beta1", "beta2", "multiple regression"), lty = c(1, 
                                                                              1, 1, 2), col = c("green", "red", "blue", "black"))
#dotted lines show that the estimated regression coefficients match exactly
#with coefficients obtained using back testing, but convergence is faster

#when the relationship between x and y is linear, one iteration is enough
#when there a p predictors how many iterations does it take approximate coefficients
set.seed(1)
p<-100
n<-1000
x<-matrix(ncol =p,nrow=n)
coefi<-rep(NA,p)
#make matrix of p predictors with n values each
for (i in 1:p){
  x[,i]<-rnorm(n)
  coefi[i]<-rnorm(1)*100
}

#matrix multiplacation
y<-x%*%coefi + rnorm(n)

#numerically calculate betas
beta<-rep(0,p)
max_iterations<-1000
errors<-rep(NA,max_iterations +1)
iter<-2
errors[1]<-Inf
errors[2]<-sum((y-x%*%beta)^2)
threshold<-1e-04
while (iter < max_iterations && errors[iter-1] - errors[iter] > threshold){
  #loop through predictors
  for (i in 1:p){
    #define a
    a<-y-x%*%beta + beta[i]*x[,i]
    #regress a on the ith predictor, and update ith beta
    beta[i]<-lm(a~x[,i])$coef[2]
  }
  #advance
  iter<-iter+1
  #get new sum
  errors[iter]<-sum((y-x%*%beta)^2)
  print(c(iter - 2, errors[iter -1],errors[iter]))
  
}

#10 iterations seems to be a good approximation, the error increases on the 11th iteration
plot(1:11, errors[3:13])

  
  
  
  
  
  

























  
  
  
  
  
  
  









































    
    
    
    
    
































































