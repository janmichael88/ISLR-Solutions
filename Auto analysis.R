library(corrplot)
library(dplyr)
library(readr)
dataset <- read_csv("Documents/R/R Data/Auto.csv", 
                    col_types = cols(horsepower = col_integer()))
View(dataset)

Auto = read.csv('Auto.csv')
first10Auto = Auto[1:10,]
lastAuto = Auto[85:397,]
newAuto = rbind(first10Auto,lastAuto)
names(newAuto)

# Get the input values.
input <- Auto[,c('wt','mpg')]

# Give the chart file a name.
png(file = "scatterplot.png")


par(mfrow=c(2,2))
# Plot 
plot(x = Auto$mpg,y = Auto$weight,
     xlab = "Weight",
     ylab = "Milage",
     main = "Weight vs Milage"
)

plot(x = Auto$mpg,y = Auto$horsepower,
     xlab = "Horsepower",
     ylab = "Milage",
     main = "Horsepower vs Milage"
)

plot(x = Auto$mpg,y = Auto$acceleration,
     xlab = "Acceleration",
     ylab = "Milage",
     main = "Acceleration vs Milage"
)

plot(x = Auto$mpg,y = Auto$displacement,
     xlab = "Displacement",
     ylab = "Milage",
     main = "Diplacement vs Milage"
)

#Regression
lm.fit=lm(mpg~displacement,data=Auto)

plot(displacement,mpg)
abline(lm.fit)

plot(predict(lm.fit),residuals(lm.fit))
lm.fit2 = lm(residuals(lm.fit)~predict(lm.fit))
abline(lm.fit2)

##drop the names column
new_Auto = select(Auto, -c(name))
new_Auto = data.frame(new_Auto)

#correlation plot
Auto.cor = cor(new_Auto)

corrplot(Auto.cor)


dev.new()
pairs(new_Auto,lower.panel = NULL)

Auto = data.frame(Auto)
Auto[is.na(Auto)] = 100

#computing linear regression
lm.fit1 = lm.fit=lm(mpg~displacement+cylinders+
                      horsepower+weight+acceleration+
                      year+origin,data=new_Auto)


plot(lm.fit1)

#interaction terms
lm.fit2 = lm(mpg~displacement+horsepower+
               displacement*horsepower,data=new_Auto)

lm.fit3 = lm(mpg~displacement+I(displacement^2))

lm.fit1 = lm(mpg~.-name, data=Auto)
summary(lm.fit1)

par(mfrow=c(2,2))
plot(lm.fit1)

plot(predict(lm.fit1), rstudent(lm.fit1))


lm.fit2 = lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit2)

lm(formula = mpg ~ log(weight) + sqrt(horsepower) 
   + acceleration + I(acceleration^2))




  
  
  
  
  
  
  
  
  






