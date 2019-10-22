p<-seq(0,1,0.01)
gini<-p*(1-p)*2
entropy<- -(p*log(p) + (1-p)*log(1-p))
class.err<- 1  - pmax(p,1-p)
matplot(p, cbind(gini, entropy, class.err), col = c("red", "green", "blue"))

#exercise 5
p<- c(0.1, 0.15, 0.2, 0.2, 0.55, 0.6, 0.6, 0.65, 0.7, 0.75)
sum(p>=0.5) > sum(p<0.5)
#the number of predictions greater than 0.5
mean(p)
#the average of the probablilitie is less than 50%
#review alogrithm for regression tree
#use recurive binary splitting on a large tree on the training data, stoping only when
#each terminal node has fewere than some min obervations
#apply cost complexity pruning to the large tree in order to obtain best seq of subtree
#use k fold cv to chose alpha for each value of k
#repaeat steps 1 and 2 on all bu the kthfold of the training data
#evaluate the mean squared prediction on the data in the left out kth fold as a function of alpha
#average the results for each value of alpha and pick alpha to minimuze the average error
#return the subtree from step 2 that corresponds to the chosen value of alpha
l
#exercise 7
library(MASS)
library(randomForest)
#try ntrees from 1 to 5000 and p,p/2, sqrt(p),p=13
set.seed(1101)
#construct the train and test matrices
train<-sample(dim(Boston)[1],dim(Boston)[1]/2)
X.train<- Boston[train, -14]
X.test<- Boston[-train, -14]
Y.train<- Boston[train, 14]
Y.test<- Boston[-train, 14]

#set up ps
p<- dim(Boston)[2] - 1
p.2<- p/2
p.sq<-sqrt(p)
p.ln<-log(p)

rf.boston.p<- randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                           mtry = p, ntree = 500)
rf.boston.p.2<- randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                             mtry = p.2, ntree = 500)
rf.boston.p.sq<- randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                              mtry = p.sq, ntree = 500)
rf.boston.p.ln<-randomForest(X.train, Y.train,,xtest = X.test, ytest = Y.test,
                             mtry = p.ln,n.tree=500)

plot(1:500, rf.boston.p$test$mse, col = "green", type = "l", xlab = "Number of Trees", 
     ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.boston.p.2$test$mse, col = "red", type = "l")
lines(1:500, rf.boston.p.sq$test$mse, col = "blue", type = "l")
lines(1:500, rf.boston.p.ln$test$mse, col = "black", type = "l")
legend("topright", c("m=p", "m=p/2", "m=sqrt(p)","m=ln(p)"), col = c("green", "red", "blue","black"), 
       cex = 1, lty = 1)

#exercise 8
#regression tree on sales from carseats data
library(ISLR)
attach(Carseats)
set.seed(1)
train<-sample(dim(Carseats)[1],dim(Carseats)[1]/2)
Carseats.train<-Carseats[train,]
Carseats.test<-Carseats[-train,]

#fit a tree predicting Sales
library(tree)
tree.carseats<-tree(Sales~.,data=Carseats.train)
summary(tree.carseats)
#show tree
plot(tree.carseats)
text(tree.carseats,pretty=0)

#get the erros
pred.carseats<-predict(tree.carseats,Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)
#MSE is about 4.1488

#use CV to determine optimal level of tree complexity
cv.carseats<-cv.tree(tree.carseats,FUN=prune.tree)
par(mfrow =c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
#best sizes looks to be about 9
pruned.carseats<-prune.tree(tree.carseats, best=9)
par(mfrow = c(1,1))
plot(pruned.carseats)
text(pruned.carseats,pretty=0)
pred.pruned<-predict(pruned.carseats,Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)

library(randomForest)
#use bagging approach
bag.carseats<-randomForest(Sales~.,data=Carseats.train,mtry=10,ntree=500,
                           importance=TRUE)
bag.pred<-predict(bag.carseats, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)
#mean is about 2.586
#view importance of variables
importance(bag.carseats)
varImpPlot(bag.carseats)
#bagging improves mse to 2.58 and we see that price, shelveloc and age are the three 
#most importnat predictors of Sale

#try using a random forest but this time change the num predictors to try at each node to 5
rf.carseats<-randomForest(Sales~.,data=Carseats.train,mtry=5,ntree=500,
                          importance=TRUE)
rf.pred<-predict(rf.carseats,Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)
#MSE did not improve as much
importance(rf.carseats)

#exercise 9
library(ISLR)
attach(OJ)
set.seed(1013)

train<-sample(dim(OJ)[1],800)
OJ.train<- OJ[train,]
OJ.test<- OJ[-train,]

library(tree)
oj.tree<-tree(Purchase~., data = OJ.train)
summary(oj.tree)
#the tree only uses two variables LoyalCH and PriceDiff
#there is a misclassification rate of 0.155 and a mean deviance of .7517
#show tree diagram
oj.tree
#17 leafs but 10 terminal nodes, splitting variable is priceDiff
#splitting values is 0.05, there are 79 points in the subtree below this node
#deviance for this node is 80, .19 are CH and .81 are MM

#lets view the tree
plot(oj.tree)
text(oj.tree, pretty=0)
#loyal CH is the most important, the top 3 nodes contain loyal ch
#geneerate confusion matrix
oj.pred<-predict(oj.tree, OJ.test, type="class")
table(OJ.test$Purchase, oj.pred)

#prune tree with to with CV to find lowest cost complexity paramter
cv.oj<-cv.tree(oj.tree, FUN=prune.tree)
plot(cv.oj$size,cv.oj$dev,type="b",xlab="Tree Size", ylab = "Deviance")
#size 6 gives the lowest cross validation erros
#prune to 6
oj.pruned = prune.tree(oj.tree, best = 6)
summary(oj.pruned)
#did not change misclassification error rate  by much

#get missclassification rates for unpruned and pruned to see if there is a difference
sum(OJ.test$Purchase != predict(oj.tree,OJ.test,type="class")) /(length(predict(oj.tree,OJ.test,type="class")))
sum(OJ.test$Purchase != predict(oj.pruned,OJ.test,type="class")) / (length(predict(oj.pruned,OJ.test,type="class")))

#pruned and unpruned have the same test error rate

#exercise 10, use boosting to predict salary in the hitters
library(ISLR)
Hitters<-Hitters
sum(is.na(Hitters$Salary))
#getters where is.na is FALSE
Hitters1<-Hitters[-which(is.na(Hitters$Salary)),]
sum(is.na(Hitters$Salary))
#log transgorm the hitters salary
Hitters$Salary<-log(Hitters$Salary)
train<-1:200
Hitters.train<-Hitters1[train,]
Hitters.test<-Hitters1[-train,]

#perform boosting on the training set with 1000 tress fro a rand fo values in lambda
#product plot with different shrinkage values on the x-axis
library(gbm)
set.seed(103)
pows<-seq(-15,-0.5,by=0.1)
lambdas<-10^pows
length.lambdas<-length(lambdas)
train.errors<-rep(NA, length.lambdas)
test.errors<-rep(NA,length.lambdas)
for (i in 1:length.lambdas){
  #fit tree on salaray
  boost.hitters<-gbm(Salary~.,data=Hitters.train,distribution = "gaussian",
                     n.trees = 1000,shrinkage = lambdas[i])
  #get training and test predictions
  train.pred<-predict(boost.hitters,Hitters.train,n.trees=1000)
  test.pred<- predict(boost.hitters,Hitters.test,n.trees=1000)
  #add to errors
  train.errors[i]<-mean((Hitters.train$Salary - train.pred)^2)
  test.errors[i]<-mean((Hitters.test$Salary - test.pred)^2)
}

plot(lambdas, train.errors, type = "b", xlab = "Shrinkage", ylab = "Train MSE", 
     col = "blue", pch = 20)
plot(lambdas, test.errors, type = "b", xlab = "Shrinkage", ylab = "Test MSE", 
     col = "red", pch = 20)
#get the min of test errors and the lamda at min
min(test.errors)
lambdas[which.min(test.errors)]
#minimum test error at lambda of 0.03

#compare with regression (lasso)
lm.fit<-lm(Salary~.,data = Hitters.train)
lm.pred<-predict(lm.fit,Hitters.test)
mean((Hitters.test$Salary - lm.pred)^2)

library(glmnet)
set.seed(134)
x<-model.matrix(Salary~.,data=Hitters.train)
y<-Hitters.train$Salary
x.test<-model.matrix(Salary~.,data=Hitters.test)
lasso.fit<-glmnet(x,y,alpha = 1)
lasso.pred<-predict(lasso.fit,s=0.01,newx = x.test)
mean((Hitters.test$Salary-lasso.pred)^2)
#both linear and regularization have higher tst MSE than boosting

boost.best<- gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", 
                 n.trees = 1000, shrinkage = lambdas[which.min(test.errors)])
summary(boost.best)
#top three most important variables are CHmRun, Walkd, CatBat

library(randomForest)
set.seed(21)
rf.hitters<-randomForest(Salary~.,data=Hitters.train,ntree=500,mtry=19)
rf.pred<-predict(rf.hitters,Hitters.test)
mean((Hitters.test$Salary - rf.pred)^2)
#MSE for boosting less than the best test MSE for boosting

#exercise 11
library(ISLR)
train<-1:1000
Caravan$Purchase<-ifelse(Caravan$Purchase == "Yes",1,0)
Caravan.train<-Caravan[train,]
Caravan.test<-Caravan[-train,]

library(glm)
set.seed(342)
boost.caravan<-gbm(Purchase~., data=Caravan.train, n.trees = 1000,
                   shrinkage = 0.01, distribution = "bernoulli")
summary(boost.caravan)

#generate confusion matrix for boosted tree
boost.prob<-predict(boost.caravan,Caravan.test,n.trees = 1000, type="response")
boost.pred<-ifelse(boost.prob > 0.2,1,0)
table(Caravan.test$Purchase,boost.pred)
#of a total 137+34 predicted 1, only 34 actually were 1

#git a glm model
lm.caravan<-glm(Purchase~., data=Caravan.train,family = binomial)
#get probabililites or likelihoods
lm.pred<-predict(lm.caravan,Caravan.test,type="response")
lm.prob<-ifelse(lm.prob > 0.2,1,0)
table(Caravan.test$Purchase,lm.prob)
#of a total of 350+48 predicted to buy, onlt 58 actually bought
#this is lower than boosting

#Exercise 12
set.seed(1)
library(ISLR)
summary(Weekly)
train<-sample(nrow(Weekly),2/3*nrow(Weekly))
test<- -train

#fit logistic regression
glm.fit<-glm(Direction~. -Year - Today,data = Weekly[train,],family = "binomial")
glm.probs<-predict(glm.fit,newdata = Weekly[test,],type="response")
glm.pred<-rep("Down",length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred,Weekly$Direction[test])
mean(glm.pred != Weekly$Direction[test])
#misclassfiication rate is 49%

#boosting
library(gbm)
#create new binomial column
Weekly$BinomialDirection<-ifelse(Weekly$Direction == "Up",1,0)
boost.weekly<-gbm(BinomialDirection~. -Year -Today -Direction,data=Weekly[train,],
                  distribution = "bernoulli",n.trees=5000)
yhat.boost<-predict(boost.weekly,newdata = Weekly[test,],n.trees = 5000)
yhat.pred<-rep(0,length(yhat.boost))
yhat.pred[yhat.boost>0.5] = 1
table(yhat.pred,Weekly$BinomialDirection[test])
#get missclassification rate 
mean(yhat.pred != Weekly$BinomialDirection[test])

#bagging
Weekly<-Weekly[,!(names(Weekly) %in% c("BinomialDirection"))]
library(randomForest)
bag.weekly = randomForest(Direction ~ . - Year - Today, data = Weekly, subset = train, 
                          mtry = 6)
yhat.bag = predict(bag.weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])
mean(yhat.bag != Weekly$Direction[test])

#random forests
rf.weekly = randomForest(Direction ~ . - Year - Today, data = Weekly, subset = train, 
                         mtry = 2)
yhat.bag = predict(rf.weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])
mean(yhat.bag != Weekly$Direction[test])

#most models do no better than random guessing
#with boosting have the lowest misclassification rate
















 























  
  
  
  
  
  

























































