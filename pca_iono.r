MyData_raw<- read.csv(file="ionosphere.csv",header=FALSE, sep=",")
class(MyData_raw)
#setwd("D:/R-progrms")
g<-dim(MyData_raw)
new_mydata<-subset(MyData_raw[g[2]])
q=g[2]-1
#q
MyData<-subset(MyData_raw[3:q])
a <-dim(MyData)
?pr

#4.1 
ir.pca <-prcomp(MyData,center = FALSE,scale. = FALSE)$rotation[,1]
ir.pcb <-prcomp(MyData,center = FALSE,scale. = FALSE)$rotation[,2]
plot(ir.pca,ir.pcb,main="PC1 and PC2",xlab="x", ylab="y", pch=19)

#4.2
ir.pcaa <-(prcomp(MyData,center = FALSE,scale. = FALSE)$sdev)^2
plot(ir.pcaa,type='o',xlab = "Principal_Component" ,ylab = "Variance")

#4.3
ir.pcab <-princomp(MyData,cor = FALSE)$loadings
ir.pcac <-prcomp(MyData,center = FALSE)$rotation
#biplot(ir.pcab)

#4.4
ir.pcad <-(prcomp(MyData,center = FALSE,scale. = FALSE)$sdev)^2/sum(ir.pcad)*100
plot(ir.pcad,type='o',xlab = "Principal_Component",ylab = "Variance")
#from the graph and data we see that after the 15 iterations the variance barely differs. 


