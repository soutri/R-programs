FindError = function(cluster_lbl,true_lbl){
  TotalError = 0
  for(j in 1:2){
    g = 0
    b = 0
    for(i in 1:length(cluster_lbl)){
      if(cluster_lbl[i] == j){
        if(isTRUE(all.equal.character(as.character(true_lbl[i]),'g'))){
          g = g + 1
        }
        else if(isTRUE(all.equal.character(as.character(true_lbl[i]),'b'))){
          b = b + 1
        }
      }
    }
    if(g>b){
      error = b/(g+b)
      TotalError = error + TotalError
    }else if(b>g){
      error = g/(g+b)
      TotalError = error + TotalError
    }
    
  }
  
  return(TotalError)
  
}





MyData_raw<- read.csv(file="ionosphere.csv",header=FALSE, sep=",")
class(MyData_raw)
#setwd("D:/R-progrms")
MyDat<-(subset(MyData_raw[3:52,]))
I50<-dist(MyDat)
dim(MyDat)
#5.1 and 5.2
k<-hclust(I50,method="complete")
plot(k,main = "Cluster Dendogram for Ionosphere dataset")

cluster_lbl = cutree(tree = k,k = 2)

BforePCAError = FindError(cluster_lbl,MyDat[,35])

#5.3 and 5.4
ir.pcad <-(prcomp(I50,center = FALSE,scale. = FALSE)$sdev)^2/sum(ir.pcad)*100
plot(ir.pcad,type='o',xlab = "Principal_Component",ylab = "Variance")
#from the graph and data we see that after the 15 principal components the variance barely differs. 

red_data = MyDat[,1:34]
temp = prcomp(red_data)

reduced_dataset<-dist(prcomp(red_data)$x[,1:11])
j<-hclust(reduced_dataset,method="complete")
plot(j,main = "Cluster ")
clustLbl = cutree(j,2)
afterPCAError = FindError(clustLbl, MyDat[,35])
afterPCAError