#install.packages('ISLR')
require(ISLR)
#install.packages('MASS')
library(MASS)
library(ggplot2)

#MyData_raw <-read.csv(file="univ_data.csv",header=FALSE, sep=",")
MyData <-cbind(1,Auto$horsepower,Auto$mpg)
#class(MyData_raw)
#setwd("D:/R-progrms")
#b<-dim(MyData_raw)
#x0<-matrix(1,nrow=b[1],ncol = 1)
#MyData<-cbind(x0,MyData_raw)
#line

a<-dim(MyData)
z <- 1
cost <- c()
#nrow[MyData]
#ncol[MyData]
m=a[1]
j=a[2]
iter=1
theta_matrix<-matrix(0,nrow=1,ncol=2)
theta_matrix1<-matrix(0,nrow=1,ncol=2)
hypothesis_eq_matrix=matrix(0,ncol=2,nrow=m)
hypothesis_matrix=matrix(0,ncol=1,nrow=m)
#cost_matrix=matrix(0,ncol=2 ,nrow=m )
cost_matrix <- c()
#cost_matrix1=matrix(0,ncol=2 ,nrow=m )
iter=150000
theta_plot=matrix(0,nrow=iter,ncol=2)
cost_plot=matrix(0,nrow = iter,ncol=2)

for(e in 1:iter)
{
  hypothesis_eq_matrix[,1] = theta_matrix[,1] * MyData[,1]
  hypothesis_eq_matrix[,2] = theta_matrix[,2] * MyData[,2]
  hypothesis_matrix = hypothesis_eq_matrix[,1] + hypothesis_eq_matrix[,2]
  cost_matrix = (1/(2*m)) * sum((hypothesis_matrix - MyData[,3])^2)
  #cost_matrix1[x:x,1:1]=((hypothesis_matrix[x:x,1:1]-MyData[x:x,a[2]:a[2]])^2)

# cost[z] <- cost_matrix[x:x,1:1] + cost_matrix1[x:x,1:1]
# cost[z] = cost[z]/ (2*m)
# #cost_plot[e:e,1:1]=cost_matrix[1:1,1:1]
# #cost_plot[e:e,2:2]=cost_matrix[2:2,1:1]
# z = z + 1
#update_theta

theta_matrix[,1] = theta_matrix[,1] - (0.0001 * cost_matrix)
theta_matrix[,2] = theta_matrix[,2] - (0.0001 * cost_matrix)
theta_plot[e,1] = theta_matrix[,1]
theta_plot[e,2] = theta_matrix[,2]

#contour(theta_plot[,2],theta_plot[,1],cost_plot[,1], method = "edge")
#blah <- data.frame(theta_plot[,2],theta_plot[,1],cost_plot[,1])

#ggplot(blah, aes(x=theta_plot[,1], y=theta_plot[,2], fill=cost_plot[,1])) + geom_raster() + scale_fill_distiller(palette="RdYlBu")

#contour(theta_plot[,1],theta_plot[,2],cost_plot[,2])

plot(MyData[,2],MyData[,3],xlab = "horsepower",ylab = "mpg")
abline(theta_matrix[,1],theta_matrix[,2])
print(e)
}


#1.3
dat<-cbind(1,220)
hypothesis<-dat[1]*33.29+dat[2]*(-0.102)
print(hypothesis)

#1.5
theta_matrix1=(ginv((t(MyData_raw) %*% MyData_raw))) %*% t(MyData_raw) %*% MyData[,a[2]]
abline(theta_matrix1[1,],theta_matrix1[2,])

plot(x=iter.final, y= cost)

iter.final <-seq(1,iter,1) 
