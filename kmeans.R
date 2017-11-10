#week 2
#Hasan Kurban
# K-Means Clustering, textbook
set.seed(2) #random_number_series
?kmeans
x=matrix(rnorm(50*2), ncol=2) 
cor(x[,1],x[,2
            ])
View(x)
plot(x[,1],x[,2
             ])
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
plot(x[,1],x[,2])
View(x)
km.out=kmeans(x,2,nstart=20)
km.out
km.out$cluster
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)
km.out$tot.withinss
set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)
set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20) #nstart: 20 configurations for intialization 
km.out$withinss #SSE in each cluster
km.out$tot.withinss # total SSE


#eigen values and vectors:
A <- matrix(c(0,-2,1,-3),ncol=2) #create a matrix
A.eigen <-  eigen(A) 
A.eigen