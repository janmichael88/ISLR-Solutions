library(tree)
library(ISLR)
attach(Carseats)
#bin sales into high and low
High<-ifelse(Sales<=8,"No","Yes")
#convert to datagram
Carseats<- data.frame(Carseats,High)
#fit tree classifier to identify if high or not
tree.carseats =tree(High~. -Sales ,data=Carseats )
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

#test tree
set.seed(2)
train<-sample(1:nrow(Carseats),200)
Carseats.test<-Carseats[-train,]
High.test<-High[-train]
#fit tree to test
tree.carseats<-tree(High~.,-Sales,Carseats,subset = train)
tree.pred<-predict(tree.carseats,Carseats.test,type='class')
cont_table<-table(tree.pred,High.test)
#get accuracy rate
accuracy<-(cont_table[1,1] + cont_table[2,2]) / (200)
accuracy

#prune the tree
set.seed(3)
cv.carseats<-cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats

#plot error rate as a function of terminal nodes and k, a cost complexity parameter
par(mfrow =c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

#prune tree to 15 terminal nodes
prune.carseats<-prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)

#get predictions from pruned tree
tree.pred<-predict(prune.carseats,Carseats.test,type='class')
table(tree.pred,High.test)


#fitting regression trees
library(MASS)
attach(Boston)
set.seed(1)
#index of train samples
train<-sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston<-tree(medv~.,Boston,subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)
#the tree predicts 46k for med value house with high econ status (rm>7.4 and lstat < 9.7)

cv.boston<-cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type="b")
#most complex tree is chose, prune tree
prune.boston<-prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
#use unpruned tree to make predictions on the test set
yhat<-predict(prune.boston,newdata = Boston[-train,])
boston.test<-Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
#get mse
(mean((yhat-boston.test)^2))^.5
#on average the esimation if off by $5k

#baggin and random forest
#baggin is same as random forest with m=p
library(randomForest)
set.seed(1)
bag.boston<-randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
#how well does bagged model work
yhat.bag<-predict(bag.boston,newdata = Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)
hist(yhat.bag-boston.test)

#try changing the number of trees used
bag.boston<-randomForest(medv~., data=Boston, subset = train, mtry=13,ntree=25)
yhat.bag<-predict(bag.boston,newdata = Boston[-train,])
mean((yhat.bag-boston.test)^2)
#try using a different number of variables at each split
set.seed(1)
rf.boston<-randomForest(medv~., data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf<-predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

#check how each variable is important
importance(rf.boston)
varImpPlot(rf.boston)

#boosting
library(gbm)
set.seed(1)
boost.boston<-gbm(medv~.,data=Boston[train,],distribution = "gaussian",n.trees = 5000,
                  interaction.depth = 4)
summary(boost.boston)
#lstat and rm are the two most important variables
#partial dependence plots show marginal effect of the slected variables on the response
#after integrating out other variables
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

#predictions
yhat.boost<-predict(boost.boston,newdata=Boston[-train,],n.trees = 5000)
mean((yhat.boost - boston.test)^2)

#change lambda value
boost.boston<-gbm(medv~.,data=Boston[train,],distribution = "gaussian",n.trees = 5000,
                  interaction.depth = 4,shrinkage = 0.19)
yhat.boost<-predict(boost.boston,newdata=Boston[-train,],n.trees = 5000)
mean((yhat.boost-boston.test)^2)
















































































