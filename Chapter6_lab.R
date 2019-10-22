library(ISLR)
library(leaps)
Hitters = Hitters

#check for Na
sum(is.na(Hitters))

#lin models, find best models given features
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)

regfit.full=regsubsets(Salary~., data = Hitters,nvmax = 19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq

#plot rsq, cp and BIC
par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")

which.max(reg.summary$adjr2)

points(11,reg.summary$adjr2[11],col='red',cex=2,pch=20)

#plot CP and BIC
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
which.min(reg.summary$cp)

points(10,reg.summary$cp [10],col="red",cex=2,pch=20)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",
     type="l")
points(6,reg.summary$bic [6],col="red",cex=2,pch=20)

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19, method ="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,
                      method ="backward")
summary(regfit.bwd)

coef(regfit.full,7)

#set training adn test
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test = (!train)
test.mat=model.matrix(Salary~.,data=Hitters[test,])

#use regsubsets on train
regfit.best = regsubsets(Salary~., data=Hitters[train,],nvmax=19)
val.errors = rep(NA,19)
for (i in 1:19){
  coefi = coef(regfit.best,id=1)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

predict.regsubsets = function(object,newdata,id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object,id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

  
Hitters=na.omit(Hitters)  
regfit.best = regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(regfit.best,10)

k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors = matrix(NA,k,19,dimnames = list(NULL,paste(1:19)))

for (j in 1:k){
  best.fit = regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
for (i in 1:19){
  pred=predict.regsubsets(best.fit,Hitters[folds==j,],id=i)
  cv.errors[j,i] = mean((Hitters$Salary[folds==j]-pred)^2)
}
}

#ridge regression and the lasso
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid,standardize = FALSE)

coef(ridge.mod)
#coefs should reach zero as lambda goes to infinity

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

#examine at lambda 60
ridge.mod$lambda[60]
coef(ridge.mod)[,60]

sqrt(sum(coef(ridge.mod)[-1,60]^2))

#predict to get ridge coefficnets
predict(ridge.mod,s=50,type="coefficients")[1:20,]

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test = (-train)
y.test = y[test]

#fit a ridge regression model
ridge.mod = glmnet(x[train,],y[train],alpha=0,lambda = grid,thresh = 1e-12)
ridge.pred = predict(ridge.mod,s=4,newx = x[test,])
mean((ridge.pred-y.test)^2)

#compare to mean of y 
mean((mean(y[train])-y.test)^2)
#mse much larger this time

#plot of mse for test as lambda increases
vec_mse_scores<-replicate(100,0)
for (i in 1:100){
  #get predictions at ith lambda
  predsz<-predict(ridge.mod,s=i,newx = x[test,])
  mse_preds<-mean((predsz-y.test)^2)
  vec_mse_scores[i]<-mse_preds
  
}

plot(vec_mse_scores)

#check mse for lambda of 10^10
ridge.pred = predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

#fit ridgmodel with 0
ridge.pred = predict(ridge.mod,s=0,newx = x[test,])
mean((ridge.pred-y.test)^2)

#an alternative
lm(y~x,subset=train)

predict(ridge.mod,s=0,type="coefficients")[1:20,]

#cv to find best lm
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

#what lambda give bestlam
ridge.pred = predict(ridge.mod,s=bestlam,newx = x[test,])
mean((ridge.pred-y.test)^2)

#refit ridge regression 
out = glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]


#the lasso
lasso.mod<-glmnet(x[train,],y[train],alpha=1,lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out=glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

bestlam=min(cv.out$lambda)
lasso.pred<-predict(lasso.mod,s=bestlam,newx = x[test,])
mean((lasso.pred-y.test)^2)

#some of the coefs in lasso drop to zero
out=glmnet(x,y,alpha=1,lamda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)

#PCR Regression and partial least squares
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")

#now for partial least squares
set.seed(1)
pls.fit = plsr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)



















  




















