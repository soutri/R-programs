#plotting points
Obs <- c('1','2','3','4','5','6')
X1 <- c(1,1,0,5,6,4)
X2 <- c(4,3,4,1,2,0)

km.data <- data.frame(Obs,X1,X2)
km.data

plot(km.data[,2],km.data[,3],las=1,main="K-Means")


