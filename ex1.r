#a
dat<- cbind(c(1,1,0,5,6,4),c(4,3,4,1,2,0))
plot(dat[,1],dat[,2],pch=20)

#b
set.seed(1)
grp<- sample((1:2),6,replace=TRUE)
grp
plot(dat[,1],dat[,2],col=grp,pch=15)

#C
#assigning two clusters groups
x1<-dat[grp==1,1]
y1<-dat[grp==1,2]
x2<-dat[grp==2,1]
y2<-dat[grp==2,2]

#calculating centroids
centroid_1 <- c(mean(x1),mean(y1))
centroid_1
centroid_2 <- c(mean(x2),mean(y2))
plot(dat[,1],dat[,2],col=grp,pch=15)
points(centroid_1[1],centroid_1[2],pch=10,col=1)
points(centroid_2[1],centroid_2[2],pch=10,col=2)

?dist

#assign clusters on euclieadian distance
grp <- c(1, 1, 1, 2, 2, 2)
x1<-dat[grp==1,1]
y1<-dat[grp==1,2]
x2<-dat[grp==2,1]
y2<-dat[grp==2,2]
centroid_1 <- c(mean(x1),mean(y1))
centroid_2 <- c(mean(x2),mean(y2))
plot(dat[,1], dat[,2], col = grp, pch = 15)
points(centroid_1[1], centroid_1[2], col = 2, pch = 10)
points(centroid_2[1], centroid_2[2], col = 3, pch = 10)


  