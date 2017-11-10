sample_mat<- as.dist(matrix(c(0,0.3,0.4,0.7,0.3,0,0.5,0.8,0.4,0.5,0,0.45,0.7,0.8,0.45,0),nrow=4))
sample_mat
h<-hclust(sample_mat,method="single")

plot(h)


k<-hclust(sample_mat,method="complete")
plot(k)

plot(hclust(sample_mat, method = "complete"), labels = c(2,1,4,3))