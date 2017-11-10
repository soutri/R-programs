#add_data
MyData_raw<- read.csv(file="ionosphere.csv",header=FALSE, sep=",")
class(MyData_raw)
setwd("D:/R-progrms")
g<-dim(MyData_raw)
new_mydata<-subset(MyData_raw[g[2]])
q=g[2]-1
q
MyData<-subset(MyData_raw[3:q])
a <-dim(MyData)

#install.packages("mvtnorm")
library("mvtnorm")
#install.packages("pbdDMAT")
#library("pbdDMAT")

avg_error2<-matrix(0,nrow = 5,ncol=21)
#MyData
tot_error<-matrix(0,nrow = 5,ncol=20)
for(xx in 1:4)
{der=der*0.107
avg_error<-matrix(0,nrow=5,ncol=1)
for(s in 2:5)
{
#number of clusters you require
k=s

MyData2<-as.matrix(MyData)

#initialize_clusture matrix

clust_mat <-matrix(0,nrow =k,ncol =a[2])#mu_matrix
clust_mat2 <-matrix(0,nrow =k,ncol =a[2])

#clust_mat<-sample(nrow(MyData),size=k,replace = TRUE)



  for(c in 1:k)
  { 
    for(b in 1:a[2])
    {gg=c
      clust_mat[c:c,b:b]=MyData[gg:gg,b:b]
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
    mat1=(likelihood_matrix %*% MyData2)[l]+0.005
    qsum=0
    qsum=apply(likelihood_matrix,1,sum)[l]+0.005
    #for(z in 1:a[1])#iteration through each observation
    ##{
      ##qsum=qsum+likelihood_matrix[l:l,z:z]
      # print(qsum)

        if(qsum == 0 || is.na(qsum))
        {
          clust_mat[l:l,]<-mat1
        }
        else
        {
          clust_mat[l:l,]<-mat1/qsum
        }
   # }
      
    }
  #updating covariance_matrix
  for(m in 1:k)
  {
    #msum=0
    cov_list[,,m:m]=cov.wt(MyData,wt=likelihood_matrix[m:m,],center = clust_mat[m:m,])$cov
    cov_list[,,m:m]=cov_list[,,m:m]+0.005
 }
  ?cov.wt
  
  #updating prior
  
 for(l in 1:k)#iteration through each cluster
  {
   # for(z in 1:a[1])#iteration through each observation
   # {
      #wsum[l:l,1:1]<-wsum[l:l,1:1]+likelihood_matrix[l:l,z:z]
   # }
  wsum[1:1,l:l]<-apply(likelihood_matrix,1,sum)[l]+0.005
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
if(k==2)
{
  gob_label_matrix<-matrix(0,nrow=a[1],ncol=1)
  
  #labeling the clusters
  for(s in 1:a[1])
  {
    for(t in 1:k)
    {
      if(likelihood_matrix[t:t,s:s]==max(likelihood_matrix[,s:s]))
      {
        gob_label_matrix[s:s,1:1]=t
      }
      
    }
  }
  #finding out good and bad for each gaussian
  dat<-cbind(gob_label_matrix,new_mydata)
  
  gob<-as.data.frame(table(dat))
  c<-dim(gob)
  
  good<-subset(gob[c[2]],gob[,2]=='g')
  names(good)[1]<-"good_freq"
  
  bad<-subset(gob[c[2]],gob[,2]=='b')
  names(bad)[1]<-"bad_freq"
  
  gob_new<-as.data.frame(rbind(cbind(bad,good)))
  newlab<-matrix('n',nrow =k,ncol=1)
  error_dat<-matrix(0,nrow = k,ncol=1)
  print("check")
  
  for(l in 1:k)
  {
    if(gob_new[l:l,1]>gob_new[l:l,2])
    {
      newlab[l:l,1]='g'
      error_dat[l:l,1]<-gob_new[l:l,1]/sum(cbind(gob_new[l:l,1],gob_new[l:l,2]))
    }
    else
    {
      newlab[l:l,1]='b'
      error_dat[l:l,1]<-gob_new[l:l,1]/sum(cbind(gob_new[l:l,1],gob_new[l:l,2]))
    }
    
  }
  d<-dim(error_dat)
  
  p=0
  for(p in 1:d[1])
  {
    avg_error[k:k,1]<-colSums(rbind(error_dat[p:p,],avg_error[k:k,1]),na.rm= TRUE)
  }
  print("avg error")
  
  
  
}

else
  {
  g_mean<-matrix(0,nrow=1,ncol=a[2])
  b_mean<-matrix(0,nrow=1,ncol=a[2])
  dist_g_b<-matrix(0,nrow = k,ncol=2)
  g_data<-as.matrix(subset(MyData_raw[3:(g[2]-1)],MyData_raw[g[2]]=='g'))
  g_mean[1:1,]<-apply(g_data,2,mean)
  b_data<-as.matrix(subset(MyData_raw[3:(g[2]-1)],MyData_raw[g[2]]=='b'))
  b_mean[1:1,]<-apply(b_data,2,mean)
  
  for(p in 1:k)
  {
    dist_g_b[p:p,1:1]<-dist(rbind(g_mean[1:1,],clust_mat[p:p,]))
    dist_g_b[p:p,2:2]<-dist(rbind(b_mean[1:1,],clust_mat[p:p,]))
  }
  
  g_b_lab<-matrix('n',nrow =k,ncol=1)
  for(l in 1:k)
  {
    if(dist_g_b[l:l,1]>dist_g_b[l:l,2])
    {
      g_b_lab[l:l,1]='g'
    }
    else
    {
      g_b_lab[l:l,1]='b'
      
    }
    
  }
  
  gob<-as.data.frame(table(g_b_lab))
  c<-dim(g_b_lab)

  newlab<-matrix('n',nrow =k,ncol=1)
  error_dat<-matrix(0,nrow = k,ncol=1)
  print("check")
  
    if(gob[1,2]>gob[2,2])
    {
      newlab[k,1]='b'
      error_dat[k,1]<-gob[1,2]/sum(cbind(gob[1,2],gob[2,2]))
    }
    else
    {
      newlab[k,1]='g'
      error_dat[k,1]<-gob[2,2]/sum(cbind(gob[1,2],gob[2,2]))
    }
    
  d<-dim(error_dat)
  
  p=0
  for(p in 1:d[1])
  {
    avg_error[k:k,1]<-colSums(rbind(error_dat[p:p,],avg_error[k:k,1]),na.rm= TRUE)
  }
  print("avg error")
  
  
}



}
for(z in 2:k)
{
  tot_error[z:z,xx:xx]=avg_error[z:z,1:1]+der
}
}
clust<-as.matrix(c(1,2,3,4,5))
avg_error2<-cbind(clust,tot_error)

boxplot(avg_error2[,5]~avg_error2[,1],pch=20,xlab ="K" ,ylab = "Total Error",cex=0.5, main="K-Means on Ionosphere dataset")



#plot(avg_error[,1],pch=20,xlab ="K" ,ylab = "Total Error",type='b',cex=0.5, main="E-M on Ionosphere dataset")





