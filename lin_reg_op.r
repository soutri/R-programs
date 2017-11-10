require(ISLR)
library(MASS)
library(ggplot2)

MyData <-cbind(1,Auto$horsepower,Auto$mpg)

e <- 1
cost1 <- c()
cost2 <- c()
m <- nrow(MyData)

theta_matrix <- matrix(0,nrow=1,ncol=2)
hypothesis_eq_matrix <- matrix(0,ncol=2,nrow=m)
hypothesis_matrix <- matrix(0,ncol=1,nrow=m)
iter <- 1000
theta_plot <- matrix(0,nrow=iter,ncol=2)
cost_plot <- matrix(0,nrow = iter,ncol=3)

for (e in 1:iter) {
  hypothesis_eq_matrix[,1] = theta_matrix[,1] * MyData[,1]
  hypothesis_eq_matrix[,2] = theta_matrix[,2] * MyData[,2]
  hypothesis_matrix = hypothesis_eq_matrix[,1] + hypothesis_eq_matrix[,2]
  cost1 = (1/m) * sum((hypothesis_matrix - MyData[,3]) * MyData[,1])
  cost2 = (1/m) * sum((hypothesis_matrix - MyData[,3]) * MyData[,2])
  
  # cost.lms <- (1/(2*m)) * sum((hypothesis_matrix - MyData[,3])^2)
  cost_plot[e,1] <- e
  cost_plot[e,2] <- cost1
  cost_plot[e,3] <- cost2
  
  theta_matrix[,1] = (1/m)*sum(theta_matrix[,1] - (0.00001 * cost1))
  theta_matrix[,2] = (1/m)*sum(theta_matrix[,2] - (0.00001 * cost2))
  # theta_plot[e,1] = theta_matrix[,1]
  # theta_plot[e,2] = theta_matrix[,2]
               
  print(e)
  # e <- e + 1
}

plot(MyData[,2],MyData[,3],xlab = "horsepower",ylab = "mpg")
abline(theta_matrix[,1], theta_matrix[,2])
# plot(MyData[,2], hypothesis_matrix)
cost.df <- data.frame(cost_plot)
# ggplot(cost.df, aes(x=X1, y=X2))
# ggplot(cost.df, aes(x=X1, y=X3))
