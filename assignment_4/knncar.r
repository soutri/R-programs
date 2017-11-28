MyData<-read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", 
                 col_names = FALSE)
a<-dim(MyData)
flag1<-seq(1,5,1)
ind = seq(1,nrow(MyData),1)
x= sample(ind,replace = FALSE)
p=seq(345,1380,345)
split_dat=list()
for(i in 1:4)
{
  split_dat[[i]]=MyData[x[(p[i]-344):p[i]],]
}
split_dat[[5]]=MyData[x[1381:1728],]
train = list()
test= list()
for(j in 1:5)
{
  f1=setdiff(flag1,j)
  test[[j]] = split_dat[[j]] 
  train[[j]] = rbind(split_dat[[f1[1]]],split_dat[[f1[2]]],split_dat[[f1[3]]],split_dat[[f1[4]]])
}