#install.packages("pROC")
library(pROC)
?pROC

MyData<- read.csv(file="bonus.csv",header=FALSE, sep=",")

M1<-MyData[,1:3]
M2<-cbind(MyData[,1:2],MyData[,4])

simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing =TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

M1.pred<-simple_roc(M1[,2],M1[,3])
M2.pred<-simple_roc(M2[,2],M2[,3])

plot(M1.pred[,2],M1.pred[,1],main="M1 and M2",col="blue",xlab = "FPR",ylab="TPR",type="l")
axis(side =1, at=M1.pred[,2], labels=FALSE)
par(new=TRUE)
lines(M2.pred[,2],M2.pred[,1],col="red")
axis(side =2, at=M1.pred[,1], labels=FALSE)
legend(0,1,legend = c("M1","M2"),col=c("blue","red"),lty = 1:1,cex=0.8)
lines(c(0,0.2,0.4,0.6,0.8,1),c(0,0.2,0.4,0.6,0.8,1),col="azure4")





#plot(M1.pred[,2],M1.pred[,1],main="M1 and M2",col="blue",xlab = "FPR",ylab="TPR")

#with(M2.pred[,2],M2.pred[,1],col="red")
#legend(0,1,legend = c("M1","M2"),col=c("blue","red"),lty = 1:1,cex=0.8)



#plot(roc(M1.pred[,2],M1.pred[,1],decreasing= TRUE),
    #scol="blue", main="M1 and M2",xlab="FPR",ylab="TPR")

## 
## Call:
## roc.default(response = test_set$bad_widget, predictor = glm_response_scores,     direction = "<")
## 
## Data: glm_response_scores in 59 controls (test_set$bad_widget FALSE) < 66 cases (test_set$bad_widget TRUE).
## Area under the curve: 0.9037
#with(glm_simple_roc, lines(1 - FPR, TPR, col=1 + labels))
#glm_simple_roc1 <- simple_roc(M2[,2]=="1", M2[,3])
#with(M2.pred[,2:1],lines(1 - FPR, TPR,col="red"))



