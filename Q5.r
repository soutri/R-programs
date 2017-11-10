#import_data
#MyDat <- read.csv(file="dat.csv", header=TRUE, sep=",")
MyDat <- read.csv(file="ionosphere.csv",header=TRUE,sep=",")
#class(MyData)

#
MyDat<-as.data.frame(unclass(MyDat))
summary(MyDat)
dim(MyDat)
MyDat_clean<-na.omit(MyDat)
dim(MyDat_clean)
summary(MyDat_clean)

#scaled_dat<-as.matrix(scale(MyDat_clean))
?kmeans

#kmm <- kmeans(MyDat_clean,3,nstart = 50,iter.max = 15)
#kmm

set.seed(123)
k.max<-15
?sapply
wss <- sapply(1:k.max,function(k){kmeans(MyDat_clean, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


