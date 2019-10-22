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

x<- -2:2
y<- 1 + x + -2*(x-1)*2*I(x>1)






























































