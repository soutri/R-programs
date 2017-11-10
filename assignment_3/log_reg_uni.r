require(ISLR)
library(MASS)
library(ggplot2)

MyData <-cbind(1,Auto$horsepower,Auto$mpg)

e <- 1
SSE<-0
cost1 <- c()
cost2 <- c()
m <- nrow(MyData)

theta_matrix <- matrix(0,nrow=2,ncol=1)
#hypothesis_eq_matrix <- matrix(0,ncol=2,nrow=2)
hypothesis_matrix <- matrix(0,ncol=1,nrow=392)
hypothesis_matrix1 <- matrix(0,ncol=1,nrow=392)
iter <- 100
theta_plot <- matrix(0,nrow=iter,ncol=2)
cost_plot <- matrix(0,nrow = iter,ncol=3)

for (e in 1:iter) {
  hypothesis_matrix = 1/(1+exp(-(MyData[,1:2] %*% theta_matrix)))
  hypothesis_matrix1 =MyData[,1:2] %*% theta_matrix
  #SSE =(1/m) * colSums((hypothesis_matrix - MyData[,3])^2)
  SSE= (1/m) *(-MyData[,3]*log(hypothesis_matrix)-(1-MyData[,3])*log(1-hypothesis_matrix))
  print(SSE)
  
  #hypothesis_eq_matrix[,2] = theta_matrix[,2] * MyData[,2]
  #hypothesis_matrix = hypothesis_eq_matrix[,1] + hypothesis_eq_matrix[,2]
  cost1 = (1/m) * colSums(((hypothesis_matrix - MyData[,3]) * MyData[,1]))
  cost2 = (1/m) * colSums(((hypothesis_matrix - MyData[,3]) * MyData[,2]))
  
  # cost.lms <- (1/(2*m)) * sum((hypothesis_matrix - MyData[,3])^2)
  cost_plot[e,1] <- e
  cost_plot[e,2] <- cost1
  cost_plot[e,3] <- cost2
  
  theta_matrix[1,] = theta_matrix[1,] - (0.000001 * cost1)
  theta_matrix[2,] = theta_matrix[2,] - (0.000001 * cost2)
  theta_plot[e,1] = theta_matrix[1,]
  theta_plot[e,2] = theta_matrix[2,]
  
  #print(e)
  # e <- e + 1
}

plot(MyData[,2],MyData[,3],xlab = "horsepower",ylab = "mpg")
abline(theta_matrix[1,], theta_matrix[2,])
plot(hypothesis_matrix1,hypothesis_matrix,main="simoid function")
# plot(MyData[,2], hypothesis_matrix)
cost.df <- data.frame(cost_plot)
# ggplot(cost.df, aes(x=X1, y=X2))
# ggplot(cost.df, aes(x=X1, y=X3))
#contour(theta_plot[,2],theta_plot[,1],abs(cost_plot[,2]), method = "edge")
#blah <- data.frame(theta_plot[1:80,1],theta_plot[1:80,2],abs(cost_plot[1:80,2]))

#ggplot(blah, aes(x=theta_plot[1:80,1], y=theta_plot[1:80,2], z=abs(cost_plot[1:80,2]))) + 
# geom_raster() + scale_fill_distiller(palette="RdYlBu") + geom_contour()

#contour(theta_plot[,1],theta_plot[,2],cost_plot[,2])
