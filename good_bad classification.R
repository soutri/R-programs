MyData_error<- read.csv(file="ionosphere.csv",header=FALSE, sep=",")
a<-dim(MyData_raw)

g_data<-as.matrix(subset(MyData_error[3:(a[2]-1)],MyData_error[a[2]]=='g'))
g_mean<-apply(g_data,2,mean)
b_data<-as.matrix(subset(MyData_error,MyData_error[a[2]]=='b'))
b_mean<-apply(b_data,2,mean)




