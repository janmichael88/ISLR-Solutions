library(ISLR)
carseats = Carseats
lm.fit = lm(Sales~CompPrice+Urban+US, data=carseats)
summary(lm.fit)

attach(carseats)
contrasts(ShelveLoc)

#creating new function
LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print('The libraries have been loaded')
}

lm.fit2 = lm(Sales~US, data=carseats)

predict(lm.fit2,data.frame(lstat=c(1,2,3)), 
        interval ="confidence")