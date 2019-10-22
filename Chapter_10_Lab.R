#PCA
USArrests<-USArrests
states<-row.names(USArrests)

#second argument 2 for column wise
apply(USArrests,2,mean)

apply(USArrests , 2, var)

pr.out=prcomp(USArrests, scale=TRUE)

names(pr.out)

pr.out$center
pr.out$scale

#rotation matrix provides PC component loadings
#loadings are eigenvectors*eigenvalues^1.5
#as many PC's as there are ps
#(n x p)(p x p) = score vectors
pr.out$rotation
pr.out$x

biplot(pr.out, scale=0)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

pr.out$sdev
pr.var<-pr.out$sdev^2

pve=pr.var/sum(pr.var)
pve

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')

plot(cumsum(pve), xlab="Principal Component", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type='b')

#clustering
set.seed (2)
x=matrix(rnorm(50*2), ncol=2)
#shift column 1,first 25 by 3
#shift column 2, first 25, by 4
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

km.out=kmeans(x,2,nstart=20)

km.out$cluster

plot(x, col=(km.out$cluster +1), main="K-Means Clustering Results with K=2", 
     xlab="", ylab="", pch=20, cex=2)

set.seed (4)
km.out=kmeans(x,3,nstart=20)
km.out

set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss

km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss

array_tot_withinns = replicate(20,0)
for (i in 1:20){
  km.out<-kmeans(x,3,nstart = i)
  array_tot_withinns[i]<-km.out$tot.withinss
  
}

#heirarchcical clustering
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="",
       cex =.9)
plot(hc.average , main="Average Linkage", xlab="", sub="",
       cex =.9)
plot(hc.single , main="Single Linkage", xlab="", sub="",
       cex =.9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single , 4)

xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), 
     main="Hierarchical Clustering with Scaled Features ")

x = matrix(rnorm(30*3),ncol = 3)
dd = as.dist(1- cor(t(x)))

plot(hclust(dd, method="complete"), main="Complete Linkagewith Correlation -Based Distance", 
     xlab="", sub="")

#lab 3 nci60
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data

dim(nci.data)

nci.labs[1:4]
table(nci.labs)

#scale the gene expressions
pr.out=prcomp(nci.data, scale=TRUE)

#define function that assings color toe ach of the 64 cell lines based on the cancer type
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z3")

summary(pr.out)

pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component",
     col =" blue ")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")

#get eigenvalues from pca per componnet
summary(pr.out)$importance[2,]

#get cusum values
summary(pr.out)$importance[3,]

#clustering the observations of NCI60
sd.data = scale(nci.data)
par(mfrow = c(1,3))
data.dist = dist(sd.data)

plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", 
     xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs,
     main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,
     main="Single Linkage", xlab="", sub="",ylab="")

hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out,4)
table(hc.clusters,nci.labs)


par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs) > abline(h=139, col="red")

hc.out

set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters ,hc.clusters )

#clusteing on the first few prinipal components
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors ")
table(cutree(hc.out,4), nci.labs)

#chapter 10, exercise 3
set.seed(1)
x = cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
x
plot(x[,1],x[,2])

labels = sample(2, nrow(x), replace=T)
labels

centroid1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
centroid1
centroid2

plot(x[,1], x[,2], col=(labels+1), pch=20, cex=2)
points(centroid1[1], centroid1[2], col=2, pch=4)
points(centroid2[1], centroid2[2], col=3, pch=4)

#get euclidean distance
euclid = function(a, b) {
  return(sqrt((a[1] - b[1])^2 + (a[2]-b[2])^2))
}
#largest distance assign to 2, then 1
assign_labels = function(x, centroid1, centroid2) {
  labels = rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    if (euclid(x[i,], centroid1) < euclid(x[i,], centroid2)) {
      labels[i] = 1
    } else {
      labels[i] = 2
    }
  }
  return(labels)
}
labels = assign_labels(x, centroid1, centroid2)
labels

last_labels = rep(-1, 6)
while (!all(last_labels == labels)) {
  last_labels = labels
  centroid1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
  centroid2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
  print(centroid1)
  print(centroid2)
  labels = assign_labels(x, centroid1, centroid2)
}

plot(x[,1], x[,2], col=(labels+1), pch=20, cex=2)
points(centroid1[1], centroid1[2], col=2, pch=4)
points(centroid2[1], centroid2[2], col=3, pch=4)

set.seed(1)
Control = matrix(rnorm(50*1000),ncol=50)
Treatment = matrix(rnorm(50*1000),ncol = 50)
X = cbind(Control,Treatment)
#impose linear depdence in one row
X[1,] = seq(-18,18-.36,.36)

pr.out = prcomp(scale(X))
summary.pr.out = summary(pr.out)
summary.pr.out$importance[,1]
#9.911 variance explained by the first PC
#lets at in B and via 10 vs 0 enconding

#new X
X = rbind(X,c(rep(10,50),rep(0,50)))
pr.out = prcomp(scale(X))
summary(pr.out)$importance[,1]

#applied 7
library(ISLR)
set.seed(1)
USArrests

dsc = scale(USArrests)
#sqaure the distance
a = dist(dsc)^2
b = as.dist(1 - cor(t(dsc)))
summary(b/a)

pr.out = prcomp(USArrests,center=T,scale=T)
pr.var = pr.out$sdev^2
pve = pr.var / sum(pr.var)
pve

#get the loadings for each PC
loadings = pr.out$rotation
pve2 = rep(NA,4)
#get mean of each column
dmean = apply(USArrests,2,mean)
dsdev = sqrt(apply(USArrests,2,var))
#subtract mean from each
dsc = sweep(USArrests,MARGIN = 2,dmean,"-")
#divide dsdev from each
dsc = sweep(USArrests,MARGIN = 2,dsdev,"/")
for (i in 1:4){
  proto_x = sweep(dsc, MARGIN = 2,loadings[,i],"*")
  pc_x = apply(proto_x,1,sum)
  pve2[i] = sum(pc_x^2)
}
pve2 = pve2/sum(dsc^2)
pve2


#excercise 9
set.seed(2)
hc.complete = hclust(dist(USArrests),method = "complete")
plot(hc.complete)

cutree(hc.complete,3)

table(cutree(hc.complete,3))

#scale the arrests
dsc = scale(USArrests)
hc.s.complete = hclust(dist(dsc),method = 'complete')
plot(hc.s.complete)

cutree(hc.s.complete, 3)

table(cutree(hc.s.complete, 3))

table(cutree(hc.s.complete, 3), cutree(hc.complete, 3))

#exercise 10
set.seed(2)
x  = matrix(rnorm(20*3*50,mean=0,sd=0.001),ncol=50)
#seperate three classes among two dimensions
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1

pca.out = prcomp(x)
summary(pca.out)

#grab the scores from the first two PC 
pca.out$x[,1:2]

plot(pca.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 

km.out = kmeans(x, 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

km.out = kmeans(x, 2, nstart=20)
km.out$cluster














































