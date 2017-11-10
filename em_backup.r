#add_data
#MyData<- read.csv(file="dat.csv",header=FALSE, sep=",")
#setwd("D:/R-progrms")
#add_data
MyData_raw<- read.csv(file="ionosphere.csv",header=FALSE, sep=",")
class(MyData_raw)
setwd("D:/R-progrms")
g<-dim(MyData_raw)
new_mydata<-subset(MyData_raw[g[2]])
q=g[2]-1
q
MyData<-subset(MyData_raw[3:q])

#install.packages("mvtnorm")
library("mvtnorm")
#install.packages("pbdDMAT")
#library("pbdDMAT")

#number of clusters you require
k=3

MyData2<-as.matrix(MyData)

#initialize_clusture matrix
a <-dim(MyData)
clust_mat <-matrix(0,nrow =k,ncol =a[2])
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

stop_crit=0.00001

#rand_matrix initialization
rand_matrix<-matrix(0,nrow=k,ncol=a[1])
mat <- matrix(0,nrow=a[2],ncol=a[2])
psum<-matrix(0,nrow=k,ncol=a[2])
#qsum<-matrix(0,nrow = k,ncol=1)
variance_matrix<-matrix(0,nrow=a[1],ncol=a[2])
#msum<-matrix(0,nrow=k,ncol=1)
wsum<-matrix(0,nrow=k,ncol=1)
mat1<-matrix(0,nrow =k,ncol =a[2])

for(r in 1:50)
{
  
  for(i in 1:k) #iterating through each cluster
  {
    for(j in 1:a[1])#iterating though each observation
    {
      rand_matrix[i:i,j:j]<-dmvnorm(x=MyData[j:j,],mean=clust_mat[i:i,],sigma=cov_list[,,i:i])*p_matrix[1:1,i:i]
      tsum=0
      #print("rand")
      #print(rand_matrix[i:i,j:j])
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
    #print("copying")
  }
  
  #updation
  for(l in 1:k)#iteration through each cluster
  {
    qsum=0
    for(z in 1:a[1])#iteration through each observation
    {
      mat1=mat1+(likelihood_matrix %*% MyData2)
      qsum=qsum+likelihood_matrix[l:l,z:z]
      #print(qsum)
      
      if(qsum == 0 || is.na(qsum))
      {
        clust_mat[l:l,]<-mat1[l:l,]
      }
      else
      {
        clust_mat[l:l,]<-mat1[l:l,]/qsum
      }
    }
    
  }
  #updating covariance_matrix
  for(m in 1:k)
  {
    msum=0
    for(n in 1:a[1])
    {
      
      mat=mat+(likelihood_matrix[m:m,n:n] * t(MyData[n:n,]-clust_mat[m:m,]) %*% as.matrix(MyData[n:n,]-clust_mat[m:m,]))
      msum=msum+likelihood_matrix[m:m,n:n]
      #print("msum")
      #print(mat)
      
      if(msum==0 || is.na(msum))
      {
        cov_list[,,m:m]<-mat
      }
      else
      {
        cov_list[,,m:m]<-mat/msum
        
      }
    }
  }
  
  #updating prior
  
  for(l in 1:k)#iteration through each cluster
  {
    for(z in 1:a[1])#iteration through each observation
    {
      wsum[l:l,1:1]<-wsum[l:l,1:1]+likelihood_matrix[l:l,z:z]
    }
    p_matrix[1:1,l:l]<-wsum[l:l,1:1]/a[1]
    #print("update 3")
  }
  
  for(e in 1:k)
  {
    
    diff_matrix[e:e]=dist(rbind(clust_mat2[e:e,],clust_mat[e:e,]))
    
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



















#for(m in 1:a[2])
#{
#  for(n in 1:a[2])
# {
#  mat[m:m,n:n]=cov_list[m:m,n:n,k:k]
#}
#}
#rand_matrix[i:i,j:j]<-dmvnorm(x=MyData[j:j,],mean=clust_mat[i:i,],sigma=mat)*p_matrix[1:1,i:i]

