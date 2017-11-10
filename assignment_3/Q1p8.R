require(ISLR)
library(MASS)
library(ggplot2)

MyData_raw<-scale(cbind(Auto[2:7]))
MyData<-as.matrix(cbind(1,MyData_raw,Auto[1]))
#MyData<-as.matrix(cbind(1,4, 300, 200, 3500, 11, 70, 2))
e <- 1
m <- nrow(MyData)

cost_matrix <- matrix(0,nrow = 7,ncol=1)
theta_matrix <- matrix(0,nrow=7,ncol=1)
hypothesis_matrix <- matrix(0,ncol=1,nrow=m)
hypothesis_matrix1 <- matrix(0,ncol=1,nrow=m)


iter <- 100

SSE_plot<-matrix(0,ncol=2,nrow=iter)


for (e in 1:iter) {
  hypothesis_matrix = MyData[,1:7] %*% theta_matrix
  SSE =(1/(2*m)) * colSums((hypothesis_matrix - MyData[,8])^2)
  SSE_plot[e,1]=e
  SSE_plot[e,2]=SSE
  print(SSE)
                         
  for(i in 1:7)
  {
    cost_matrix[i,1]=(1/m) * colSums(((hypothesis_matrix - MyData[,8]) * MyData[,i]))
  }

  
  for(j in 7)
  {
    theta_matrix[j,] = theta_matrix[j,] - (0.00003 * cost_matrix[j,])
  }
  
  #print(e)
  plot(SSE_plot[,1],SSE_plot[,2],xlab = "iterations",ylab = "cost",main="alpha=0.00003")
}





