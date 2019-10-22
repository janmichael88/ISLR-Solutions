# Plot the chart.
boxplot(Outstate ~ Private, data = college_1, xlab = "Private",
        ylab = "OutState", main = "College")

Elite=rep("No",nrow(college_1))
Elite[college_1$Top10perc >50]="Yes"
Elite=as.factor(Elite)
college_1=data.frame(college_1 ,Elite)

#box plots for elite
boxplot(Outstate ~ Elite, data = college_1, xlab = "Elite",
        ylab = "OutState", main = "College")

#example hist
par(mfrow=c(2,2)) 
hist(college_1$Expend,breaks=50)
hist(college_1$Accept,breaks=50)
hist(college_1$Enroll,breaks=50)
hist(college_1$Room.Board,breaks=50)