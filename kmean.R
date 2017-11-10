#plotting points
Obs <- c('1','2','3','4','5','6')
X1 <- c(1,1,0,5,6,4)
X2 <- c(4,3,4,1,2,0)

km.data <- data.frame(Obs,X1,X2)
km.data

plot(km.data[,2],km.data[,3],las=1,main="K-Means")
?sample
?nrow

#cluster forming
set.seed(1)
labels<-sample((1:2),6,replace = TRUE)
labels

plot(km.data[,2],km.data[,3],col=c(2,3))

#compute the centroid

?mean
?subset

group_a_x <- km.data[labels==1,2]
group_a_y <- km.data[labels==1,3]

group_b_x <-km.data[labels==2,2]
group_b_y <-km.data[labels==2,3]

centroid_x <-c(mean(group_a_x),mean(group_b_x))
centroid_y <-c(mean(group_a_y),mean(group_b_y))
points(centroid_x,centroid_y,col=c(2,3), pch=15)


getwd()
setwd("D:/R-progrms")
