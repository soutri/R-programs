#add_data
MyData_raw <- read.csv(file="ionosphere.csv",header=FALSE, sep=",")
class(MyData_raw)
#setwd("D:/R-progrms")
b <-dim(MyData_raw)
new_mydata<-subset(MyData_raw[b[2]])
q=b[2]-1
mydata<-subset(MyData_raw[1:q])

str(mydata)
summary(mydata)

set.seed(4)
km1 = kmeans(mydata, 2, nstart=100)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10)
{
  wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
}
  

plot(1:10, wss, type="b", xlab="K",
     ylab="WSS",
     main="Optimal Clusters for ionosphere dataset using Elbow method")

