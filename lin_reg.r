MyData <- read.csv(file="lin_dat.csv",header=FALSE, sep=",")
class(MyData)
setwd("D:/R-progrms")

a<-dim(MyData)
#number of training examples 
m<-a[1]
#number of features
n<-a[2]-1

#initialize theta
theta <- rnorm(n)
theta_matrix<-matrix(theta,nrow=1,ncol=n)

#initiaize hypothesis_matrix
hypothesis_matrix=matrix(0,ncol=n,nrow=m)

#initialize cost_matrix
cost_matrix=matrix(0,ncol= ,nrol= )

#hypothesis model
for(y in 1:m)
{
for(x in 1:n)
{
  hypothesis_matrix[y:y,x:x]=hypothesis_matrix[y:y,x:x]+(MyData[y:y,x:x]*theta_matrix[1:1,x:x])
}
}

for(c in 1:n)
{
  for(d in 1:m)
  {
    cost_matrix[]=
  }
}
  























