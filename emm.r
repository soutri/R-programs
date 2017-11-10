#add_data
MyData_raw<- read.csv(file="ionosphere.csv",header=FALSE, sep=",")
class(MyData_raw)
setwd("D:/R-progrms")
g<-dim(MyData_raw)
new_mydata<-subset(MyData_raw[g[2]])
q=g[2]-1
#q
MyData<-subset(MyData_raw[3:q])

#install.packages("mvtnorm")
library("mvtnorm")
#install.packages("pbdDMAT")
#library("pbdDMAT")

#number of clusters you require
k=5

MyData2<-as.matrix(MyData)

#initialize_clusture matrix
a <-dim(MyData)
clust_mat <-matrix(0,nrow =k,ncol =a[2])#mu_matrix
clust_mat2 <-matrix(0,nrow =k,ncol =a[2])

for(c in 1:k)
{
  for(b in 1:a[2])
  {
    clust_mat[c:c,b:b]=MyData[c:c,b:b]
  }
}

#initialize covariance matrix
cov_matrix <- matrix(0,nrow=a[2],ncol=a[2])
diag(cov_matrix) <- 1

cov_list<-array(replicate(k,cov_matrix),dim=c(a[2],a[2],k))
#cov_list

#initialize p
p_matrix <- matrix(1/k,nrow=1,ncol=k)

#initialize likelihood matrix
likelihood_matrix <- matrix(0,nrow=k,ncol=a[1])

#initialize diff_matrix
diff_matrix <-matrix(-10,nrow=k,ncol = 1)

stop_crit=0.001

#rand_matrix initialization
rand_matrix<-matrix(0,nrow=k,ncol=a[1])
mat <- matrix(0,nrow=a[2],ncol=a[2])
psum<-matrix(0,nrow=k,ncol=a[2])
qsum<-matrix(0,nrow = 1,ncol=k)
variance_matrix<-matrix(0,nrow=a[1],ncol=a[2])
#msum<-matrix(0,nrow=k,ncol=1)
wsum<-matrix(0,nrow=1,ncol=k)
mat1<-matrix(0,nrow =k,ncol =a[2])

for(r in 1:20)
{
  for(i in 1:k) #iterating through each cluster
  {
    for(j in 1:a[1])#iterating though each observation
    {
      rand_matrix[i:i,j:j]<-dmvnorm(x=MyData[j:j,],mean=clust_mat[i:i,],sigma=cov_list[,,i:i])*p_matrix[1:1,i:i]
      tsum=0
      for(p in 1:k)
      {
        tsum<-tsum+dmvnorm(x=MyData[j:j,],mean=clust_mat[p:p,],sigma=cov_list[,,p:p])*p_matrix[1:1,p:p]
        #print("tsum")
        #print(tsum)
      }
      likelihood_matrix[i:i,j:j]=rand_matrix[i:i,j:j]/tsum
      #print("enter1")
      #print(likelihood_matrix[i:i,j:j])
    }
  }
  #copying previous clust_matrix
  for(f in 1:k)
  {
    for(s in 1:a[2])
    {
      clust_mat2[f:f,s:s]=clust_mat[f:f,s:s]
      
    }
  }
  
  #updation
  for(l in 1:k)#iteration through each cluster
  { 
    mat1=(likelihood_matrix %*% MyData2)[l]
    qsum=0
    qsum=apply(likelihood_matrix,1,sum)[l]
    
    if(qsum == 0 || is.na(qsum))
    {
      clust_mat[l:l,]<-mat1
    }
    else
    {
      clust_mat[l:l,]<-mat1/qsum
    }
    
  }
  #updating covariance_matrix
  for(m in 1:k)
  {
    #msum=0
    cov_list[,,m:m]=cov.wt(MyData,wt=likelihood_matrix[m:m,],center = clust_mat[m:m,])$cov
  }
  ?cov.wt
  
  #updating prior
  
  for(l in 1:k)#iteration through each cluster
  {
    wsum[1:1,l:l]<-apply(likelihood_matrix,1,sum)[l]
    p_matrix[1:1,l:l]<-wsum[1:1,l:l]/a[1]
    #print("update 3")
  }
  
  for(e in 1:k)
  {
    
    diff_matrix[e:e]=dist(rbind(clust_mat2[e:e,],clust_mat[e:e,]))^2
    
    #print("calculating error")
  }
  
  stop_criteria=sum(diff_matrix)
  print(stop_criteria)
  
  if(stop_criteria<stop_crit || is.na(stop_criteria))
  {
    print("Reached stopping criteria.Breaking now..")
    break;
  }
}




