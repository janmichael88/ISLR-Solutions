library(ISLR)
smarket = Smarket

dim(smarket)
summary(smarket)

cor(smarket) #error
cor(smarket[,-9]) #drop the direction, which is the last column

attach(smarket)
plot(Volume)

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
            data=smarket,family=binomial)
summary(glm.fit)

coef(glm.fit) #access the coeffcients
summary(glm.fit)$coef

glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]

contrasts(Direction)

glm.pred = rep('Down',1250)
glm.pred[glm.probs>0.5] = 'Up'

table(glm.pred,Direction)
correct_predictions = (507+145) / (1250)
#another way
mean(glm.pred == Direction)

train = (Year==2005)
smartket2005 = smarket[train,]
Direction.2005 = Direction
dim(smartket2005)


glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
            data=smarket,family=binomial,subset = (Year<2005))
glm.probs=predict(glm.fit,smartket2005,type='response')

glm.pred =rep('Down',252)
glm.pred[glm.probs>0.5] = 'Up'
table(glm.pred,Direction[Year==2005])
mean(glm.pred == Direction[Year==2005])
mean(glm.pred != Direction[Year==2005])

glm.fit=glm(Direction~Lag1+Lag2, 
            data=smarket,family=binomial,subset = (Year<2005))
glm.probs = predict(glm.fit,smartket2005,type = 'response')
glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction[Year==2005])
mean(glm.pred == Direction[Year==2005])
mean(glm.pred != Direction[Year==2005])

predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5), 
                    Lag2=c(1.1,-0.8)),type="response")



#Linear Discriminant analysis
library(ISLR)
library(MASS)
smarket = Smarket

lda.fit=lda(Direction~Lag1+Lag2,data=smarket,
            subset=(Year<2005))
lda.fit
coef(lda.fit)
plot(coef(lda.fit)[1]*Lag1-coef(lda.fit)[2]*Lag2) #does nothing

train = (Year==2005)
smartket2005 = smarket[train,]

lda.pred = predict(lda.fit,smartket2005)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class,Direction[Year==2005])
mean(lda.class == Direction[Year==2005])

#assiging posterior probability of 50%
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

#Quadratice Discriminant Analysis
library(ISLR)
library(MASS)
smarket = Smarket

qda.fit=qda(Direction~Lag1+Lag2,data=smarket,
            subset=(Year<2005))
qda.fit

qda.class=predict(qda.fit,smartket2005)$class
table(qda.class,Direction[Year==2005])
mean(qda.class == Direction[Year==2005])

#kNN
library(class)
train.X=cbind(Lag1 ,Lag2)[(Year<2005),]
test.X=cbind(Lag1,Lag2)[(Year==2005),]
train.Direction = Direction[Year==2005]

set.seed(1)
knn.pred = knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction[Year<2005])

set.seed(1)
knn.pred = knn(train.X,test.X,train.Direction,k=4)
table(knn.pred,Direction[Year<2005])

#apply kNN to caravan data
library(ISLR)
Caravan
dim(Caravan)
attach(Caravan)

summary(Purchase)

standardized.X=scale(Caravan [,-86]) #dropped purchase column
var(Caravan[,1])
var(Caravan[,2])

var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X,test.X,train.Y,k=1)
mean(test.Y == knn.pred)
mean(test.Y != knn.pred)
mean(test.Y != 'No')

table(knn.pred,test.Y)

knn.pred = knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)















































