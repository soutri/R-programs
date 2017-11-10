#add_data
MyData_raw<- read.csv(file="ionosphere.csv",header=FALSE, sep=",")
class(MyData_raw)
setwd("D:/R-progrms")
g<-dim(MyData_raw)
new_mydata<-subset(MyData_raw[g[2]])
q=g[2]-1
#q
MyData<-subset(MyData_raw[3:q])
avg_error<-matrix(0,nrow=5,ncol=1)

#install.packages("mvtnorm")
library("mvtnorm")
#install.packages("pbdDMAT")
#library("pbdDMAT")



#number of clusters you require
k=2

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
#rand_matrix initialization
rand_matrix<-matrix(0,nrow=k,ncol=a[1])
mat <- matrix(-1,nrow=a[2],ncol=a[2])
#sum_matrix <- matrix(0,nrow=1,ncol=k)
#tsum=0
#cov_list[,,1]
#mat
#mew_matrix<-matrix(0,nrow=k,ncol=a[2])
psum<-matrix(0,nrow=k,ncol=a[2])
qsum<-matrix(0,nrow = k,ncol=1)
variance_matrix<-matrix(0,nrow=a[1],ncol=a[2])
msum<-matrix(0,nrow=k,ncol=1)
wsum<-matrix(0,nrow=k,ncol=1)
mat1<-matrix(0,nrow =k,ncol =a[2])

stop_crit=0.00001

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
      tsum
    }
    
    likelihood_matrix[i:i,j:j]=rand_matrix[i:i,j:j]/tsum
  }
  print("finishing E")
}

#copying previous clust_matrix
for(f in 1:k)
{
  for(s in 1:a[2])
  {
    clust_mat2[f:f,s:s]=clust_mat[f:f,s:s]
    
  }
  print("copying cluster")
}

#updation
for(l in 1:k)#iteration through each cluster
{
  for(z in 1:a[1])#iteration through each observation
  {
    mat1=likelihood_matrix %*% as.matrix(MyData)
    for(w in 1:a[2])# iteration through each variable in each observation
    {
      psum[l:l,w:w]=psum[l:l,w:w]+mat1[l:l,w:w]
    }
    qsum[l:l,1:1]=qsum[l:l,1:1]+likelihood_matrix[l:l,z:z]
  }
  
  for(y in 1:a[2])  #updating cluster matrix
  {
    clust_mat[l:l,y:y]=psum[l:l,y:y]/qsum[l:l]
  }
  print("update1")
}

#updating covariance_matrix
for(m in 1:k)
{
  for(n in 1:a[1])
  {
    for(o in 1:a[2])
    {
      variance_matrix[n:n,o:o]<-MyData[n:n,o:o]-clust_mat[m:m,o:o]
      mat<-likelihood_matrix[m:m,n:n]*t(variance_matrix)%*%variance_matrix
      #cov_list[o:o,o:o,m:m]=cov_list[o:o,o:o,m:m]+likelihood_matrix[m:m,n:n]*variance_matrix[n:n,o:o]*t(variance_matrix[n:n,o:o])
    }
    
    for(d in 1:a[2])
    {
      for(e in 1:a[2])
     {
        cov_list[d:d,e:e,m:m]=cov_list[d:d,e:e,m:m]+mat[d:d,e:e]
    }
    }
    msum[m:m,1:1]=msum[m:m,1:1]+likelihood_matrix[m:m,n:n]
    
  }
  
  for(y in 1:a[2])  #updating cluster matrix
  {
    cov_list[y:y,y:y,m:m]=cov_list[y:y,y:y,m:m]/msum[m:m]
    
  }
  print("update 2")
  
}

#updating prior

for(l in 1:k)#iteration through each cluster
{
  for(z in 1:a[1])#iteration through each observation
  {
    wsum[l:l,1:1]<-wsum[l:l,1:1]+likelihood_matrix[l:l,z:z]
  }
  p_matrix[1:1,l:l]<-wsum[l:l,1:1]/a[1]
  print("update 3")
  
}


for(e in 1:k)
{
  
  diff_matrix[e:e]=dist(rbind(clust_mat2[e:e,],clust_mat[e:e,]))^2
  diff
  
}

stop_criteria=sum(diff_matrix)
print(stop_criteria)

if(stop_criteria<stop_crit)
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

