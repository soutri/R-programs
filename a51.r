#add_data
MyData_raw <- read.csv(file="ringnorm_data.csv",header=FALSE, sep=",")
class(MyData_raw)
#setwd("D:/R-progrms")
b <-dim(MyData_raw)
#b
?subset
new_mydata<-subset(MyData_raw[1])
#q=b[2]-1
#q
mydata<-subset(MyData_raw[2:b[2]])

str(mydata)
summary(mydata)

set.seed(4)
km1 = kmeans(mydata, 2, nstart=100)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) 
  
  wss[i] <- sum(kmeans(mydata,centers=i,iter.max = 200,algorithm = "Lloyd")$withinss)

plot(1:10, wss, type="b", xlab="K",
     ylab="WSS",
     main="Optimal clusters for Ringnorm dataset using Elbow Method")

