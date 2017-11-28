library(readr)
library(ggplot2)
library("magrittr")
library("dplyr")
setwd("D:/R-progrms/term project")

train <- read_csv("train.csv", 
                  col_types = cols(id = col_skip()))
test <- read_csv("test.csv", 
                 col_types = cols(id = col_skip()))
temp.train = train
temp.test = test

temp.train[temp.train == -1] = NA
temp.test[temp.test == -1] = NA

sum(is.na(temp.train))
sum(is.na(temp.test))

colSums(is.na(temp.train))
colSums(is.na(temp.test))

train.missing = as.data.frame(sort(colSums(is.na(temp.train)),decreasing = TRUE))
test.missing = as.data.frame(sort(colSums(is.na(temp.test)),decreasing = TRUE))

train.missing=head(train.missing)
test.missing=head(test.missing)



remove_column_train=as.matrix(colnames(t(train.missing)))
remove_column_test=as.matrix(colnames(t(test.missing)))

new_train=as.data.frame(train[,!(names(train)%in% remove_column_train)])
new_test=as.data.frame(test[,!(names(test)%in% remove_column_test)])

train.missing = train.missing[train.missing > 0]
test.missing = test.missing[test.missing > 0]

x11(width = 9,height = 8,pointsize = 15)
barplot(train.missing,main = "Missing Values per feature Training Data",ylab = "Number of Missing Values",las = 2,mgp = c(2.5,0,0))
barplot(test.missing,main = "Missing Values per feature Testing Data",ylab = "Number of Missing Values",las = 2,mgp = c(2.5,0,0))

temp.train[,c(22,3)]

Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

p=dim(new_train)
r=dim(new_test)
q=c( )
s=c()

for(i in 1:p[2])
{
  q[i]<- Mode(new_train[,i])
  
}
for(i in 1:r[2])
{
  s[i]<- Mode(new_test[,i])
  
}

m <- as.matrix(new_train)
for(ii in 1:p[2])
{
  m[is.na(m[,ii])] <- q[ii]
}



n <- as.matrix(new_test)
for(jj in 1:r[2])
{
  n[is.na(n[,jj])] <- s[jj]
}


