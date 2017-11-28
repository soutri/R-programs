library(readr)
#train<-as.data.frame(read.csv(file="D:/R-progrms/assignment_4/ionosphere_train_1.csv",header=TRUE, sep=","))
#test<-as.data.frame(read.csv(file="D:/R-progrms/assignment_4/ionosphere_test_1.csv",header=TRUE, sep=","))
MyData <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data", 
                       col_names = FALSE, col_types = cols(X2 = col_skip()))
train=list()
test=list()
flag1<-seq(1,5,1)
ind = seq(1,nrow(MyData),1)
x= sample(ind,replace = FALSE)
p=seq(70,350,70)
split_dat=list()
for(i in 1:4)
{
  split_dat[[i]]=MyData[x[(p[i]-69):p[i]],]
}
split_dat[[5]]=MyData[x[281:351],]

#splitdata <- split(MyData[1:nrow(MyData),],sample(rep(1:5,as.integer(nrow(MyData)/5))))

for(i in 1:5)
{
  f1=setdiff(flag1,i)
  test[[i]] = split_dat[[i]] 
  train[[i]] = rbind(split_dat[[f1[1]]],split_dat[[f1[2]]],split_dat[[f1[3]]],split_dat[[f1[4]]])
}  
#---------------------------------------------------------------------------------------------------------
#test_label = test[,34]
#test = test[,1:33]
#train_label = train[,34]
#train = train[,1:33]
#--------------------------------------------------------------------------------------------------
eu_distance<-function(test,train)
{
  distance<-apply(train,1,function(x)sqrt(sum((x-test)^2)))
  return(distance)
}

manhattan_distance<-function(test,train)
{
  distance<-apply(train,1,function(x)sum(abs(x-test)))
  return(distance)
}

knn_eu<-function(test,train,trainlabel,k)
{
  out_label<-rep(0,nrow(test))
  
  
  for(i in 1:nrow(test))
  {
    dis<-eu_distance(test[i],train)
    row_num<-which(dis %in% sort(dis)[1:k])
    
    row_label<-trainlabel[row_num]
    out_label[i]=names(which(table(row_label) == max(table(row_label))))
    
  }
  
  #print(mean(out_label == trainlabel)*100)
  return(out_label)
}

knn_manh<-function(test,train,trainlabel,k)
{
  out_label<-rep(0,nrow(test))
  
  
  for(i in 1:nrow(test))
  {
    dis<-manhattan_distance(test[i],train)
    row_num<-which(dis %in% sort(dis)[1:k])
    row_label<-trainlabel[row_num,]
    out_label[i]=names(which(table(row_label) == max(table(row_label))))
    
  }
  
  #print(mean(out_label == orglabel)*100)
  return(out_label)
}

#------------------------------------------------------------------------------------------------------

#err=list()
#err[1]=knn(test,train,test_label,1)
#err[2]=knn(test,train,test_label,5)
#err[3]=knn(test,train,test_label,10)
#plot(c(1,5,10),err)
#------------------------------------------------------------------------------------------------------
x = c(1,5,10,15,20)
result_matrix_euclidean = matrix(ncol = 5,nrow = 5)
result_matrix_manhattan = matrix(ncol = 5,nrow = 5)
row = 0
for(k in x){
  cat("K = ",k,"\n")
  row = row + 1
  for(i in 1:5){
    train_d = train[[i]]
    train_label = train_d[,34]
    train_data = train_d[,1:33]
    test_d = test[[i]]
    test_label = test_d[,34]
    test_data = test_d[,1:33]
    
    #Weighted Voting Euclidean
    myTestLabel = knn_eu(test_data,train_data,train_label,k)
    error = mean(test_label != myTestLabel) * 100
    cat("Error1 = ",error,"\n")
    result_matrix_euclidean[row,i] = error
    
    #Weighted Voting Manhattan
    myTestLabel2 = knn_manh(train_data,train_label,test_data,k)
    error2 = mean(test_label != myTestLabel2) * 100
    cat("Error = ",error2,"\n")
    result_matrix_manhattan[row,i] = error
    
  }  
}
rownames(result_matrix_euclidean) = c("K1","K5","K10","K15","K20")
rownames(result_matrix_manhattan) = c("K1","K9","K19","K29","K49")
boxplot(x = result_matrix_manhattan,use.cols = FALSE,xlab = "Various K Values",ylab = "% Error",main="Error rate for different K with Manhattan Distance",xlim=c(1,5),ylim=c(1,50))
boxplot(x = result_matrix_euclidean,use.cols = FALSE,xlab = "Various K Values",ylab = "% Error",main="Error rate for different K with Euclidean Distance",xlim=c(1,5),ylim=c(1,50))

           