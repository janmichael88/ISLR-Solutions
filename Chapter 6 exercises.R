#Chapter 6 exersices 
#for p=1, takes the form y-_beta^2 +_lambda*_beta^2
y<-2
lambda<-2
betas<-seq(-10,10,0.1)
func = (y-betas)^2 + lambda*betas^2
plot(betas,func,pch=20)
est.beta = y/(1+lambda)
est.func = (y-est.beta)^2 + lambda*est.beta^2
points(est.beta,est.func,col="red",pch=4,lwd=5,cex=est.beta)
#that was ridge
#now for lasso
y=2
lambda=2
betas = seq(-3,3,0.01)
func = (y-betas)^2 + lambda*abs(betas)
plot(betas, func)
#estaimte beta at which func is minimized
est.beta = y - lambda/2
est.func = (y-est.beta)^2 + lambda*abs(est.beta)
points(est.beta,est.fun,col="red")

#exercise 8
set.seed(1)
X= rnorm(100)
eps = rnorm(100)

#assigne betas
beta0 = 3
beta1 = 2
beta2= -3
beta3 = 0.3
Y=beta0 + beta1*X + beta2*X^2 + beta3*X^3 + eps

#use regsubsets to selct the ebst model having polynomia of X of degree 10
library(leaps)
#make into a datafram
data.full = data.frame(y=Y,x=X)
mod.full = regsubsets(y~poly(x,14,raw=T),data=data.full,nvmax=20)
#get summary of mod.full
mod.summary = summary(mod.full)
#find the model size fot he ebst cp, BIC, and adjr2
which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.min(mod.summary$adjr2)

#plot cp, BIC,and adjr2
plot(mod.summary$cp,xlab="subset size",ylab='cp',pch=20,type="l")
points(3, mod.summary$cp[3], pch = 4, col = "red", lwd = 7)

plot(mod.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(3, mod.summary$bic[3], pch = 4, col = "red", lwd = 7)

plot(mod.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, type = "l")
points(3, mod.summary$adjr2[3], pch = 4, col = "red", lwd = 7)

coefficients(mod.full,id=3)

#fit forward and backward
mod.fwd = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, 
                     method = "forward")
mod.bwd = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, 
                     method = "backward")
fwd.summary = summary(mod.fwd)
bwd.summary = summary(mod.bwd)
#check cp and bic
which.min(fwd.summary$bic)
which.min(bwd.summary$bic)

par(mfrow = c(3, 2))
plot(fwd.summary$cp, xlab = "Subset Size", ylab = "Forward Cp", pch = 20, type = "l")
points(3, fwd.summary$cp[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$cp, xlab = "Subset Size", ylab = "Backward Cp", pch = 20, type = "l")
points(3, bwd.summary$cp[3], pch = 4, col = "red", lwd = 7)
plot(fwd.summary$bic, xlab = "Subset Size", ylab = "Forward BIC", pch = 20, 
     type = "l")
points(3, fwd.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$bic, xlab = "Subset Size", ylab = "Backward BIC", pch = 20, 
     type = "l")
points(3, bwd.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(fwd.summary$adjr2, xlab = "Subset Size", ylab = "Forward Adjusted R2", 
     pch = 20, type = "l")
points(3, fwd.summary$adjr2[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$adjr2, xlab = "Subset Size", ylab = "Backward Adjusted R2", 
     pch = 20, type = "l")
points(4, bwd.summary$adjr2[4], pch = 4, col = "red", lwd = 7)
#most pick three variable models
coefficients(mod.fwd,id=3)

#fit lasso using X,X^2, and X^7
library(glmnet)
xmat = model.matrix(y~poly(x,10,raw=T),data=data.full)[,-1]
mod.lasso = cv.glmnet(xmat,Y,alpha=1)
best.lambda = mod.lasso$lambda.min
best.lambda
plot(mod.lasso)

#now fir the model on the entire data using the bast lambda
best.model = glmnet(xmat,Y,alpha=1)
predict(best.model,s=best.lambda,type="coefficients")

#problem 9
#load and split the college data
library(ISLR)
set.seed(1)
sum(is.na(College)) #no missing values

train.size = dim(College)[1]/2
train = sample(1:dim(College)[1],train.size)
#negate to get the values that weren't picked
test = -train
College.train = College[train,]
College.test = College[test,]

#nume of applications in the Apps vairablle
lm.fit = lm(Apps~.,data=College.train)
lm.pred = predict(lm.fit,College.test)
mean((College.test[,"Apps"] - lm.pred)^2)

#pick using the college.,train and report error in college.test
library(glmnet)
train.mat = model.matrix(Apps~., data=College.train)
test.mat = model.matrix(Apps~., data=College.test)
#set up grid of lambda vlaues
grid = 10^seq(4,-2,length=100)
mod.ridge = cv.glmnet(train.mat,College.train[,"Apps"],alpha=0,lambda=grid, 
                      thresh=1e-12)
lambda.best = mod.ridge$lambda.min

ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - ridge.pred)^2)

#pick lambda using trian and report error on college.test
mod.lasso = cv.glmnet(train.mat,College.train[,'Apps'],alpha=1,lambda = grid,
                      thresh = 1e-12)
lambda.best = mod.lasso$lambda.min

lasso.pred = predict(mod.lasso, newx=test.mat, s = lamda.best)
mean((College.test[,"Apps"] - lasso.pred)^2)
#what do the coefs look like
mod.lasso = glmnet(model.matrix(Apps~.,data=College),College[,"Apps"],alpha=1)
predict(mod.lasso,s=lambda.best,type="coefficients")

#use validation to fit pcr
library(pls)
pcr.fit = pcr(Apps~.,data=College.train,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")

pcr.pred = predict(pcr.fit, College.test,ncomp = 10)
mean((College.test[,"Apps"] - data.frame(pcr.pred)[,1])^2)

pls.fit = plsr(Apps~.,data=College.train,scale=T,validation = "CV")
validationplot(pls.fit,val.type = "MSEP")

pls.pred = predict(pls.fit,College.test,ncomp=10)
mean((College.test[,"Apps"]-data.frame(pls.pred)[,1])^2)

#exercise 10
set.seed(1)
p = 20 
n = 1000
x = matrix(rnorm(n*p),n,p)
B = rnorm(p)
#assign zeros
B[3] = 0
B[4] = 0
B[9] = 0
B[10] = 0
B[19] = 0
eps = rnorm(p)
y = x%*%B + eps

#split into train and test
train = sample(seq(1000),100,replace=FALSE)
y.train = y[train,]
y.test = y[-train,]
x.train = x[train,]
x.test = x[-train,]

#fit the model
library(leaps)
regfit.full = regsubsets(y~.,data=data.frame(x = x.train,y=y.train),nvmax = p)
val.erros = rep(NA,p)
x_cols = colnames(x,do.NULL = FALSE,prefix = "x.")
#get besstcoefs,preds, and val errors
for (i in 1:p){
  coefi = coef(regfit.full,id=i)
  pred = as.matrix(x.train[,x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% x_cols]
  val.errors[i] = mean((y.train - pred)^2)
}

plot(val.errors,ylab="TRAINING MSE",pch=17,type="b")

#plot test mse
val.erros = rep(NA,p)
x_cols = colnames(x,do.NULL = FALSE,prefix = "x.")
for (i in 1:p){
  coefi = coef(regfit.full,id=i)
  pred = as.matrix(x.test[,x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% x_cols]
  val.errors[i] = mean((y.test - pred)^2)
}

plot(val.errors,ylab="Test MSE",pch=17,type="b")
which.min(val.errors)
#20 paramter model has the smallest test MSe

a = coef(regfit.full, id =20)
str(a)
barplot(a)

#near zero coefficient at x.10
val.errors = rep(NA,p)
a = rep(NA,p)
b = rep(NA,p)
for (i in 1:p){
  coefi = coef(regfit.full,id=i)
  a[i] = length(coefi) - 1
  b[i] = sqrt((B[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) +
    sum(B[!(x_cols %in% names(coefi))]^2)
}
warnings()
plot(x=a,y=b)
which.min(b)

#skipped last exercise, just look at the answer key








  
  
  
  
  
  
  
  
  






















































