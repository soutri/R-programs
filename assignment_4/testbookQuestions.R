#Question 10G
#install.packages("ISLR")
#install.packages("knn")
library(class)
library(ISLR)
dataset = Weekly
train.dataset = dataset[dataset$Year < 2009,]
Direction.20092010 <- dataset[!train.dataset]
test.dataset = dataset[dataset$Year >= 2009,]
test.label = test.dataset[,9]
train.label = train.dataset[,9]
test.dataset = test.dataset[,1:8]
train.dataset = train.dataset[,1:8]

test.dataset = as.matrix(test.dataset$Lag2)
train.dataset = as.matrix(train.dataset$Lag2)
knn.fit = knn(train = train.dataset,test = test.dataset,cl = train.label,k = 1)

mean(knn.fit == test.label)
sum(knn.fit == test.label)/nrow(test.dataset) * 100

#______________________________________________________________________________________

#Question 13

library(MASS)
Dataset = Boston
crime01 = rep(0, nrow(Dataset))
crime01[Dataset$crim > median(Dataset$crim)] = 1
training = Dataset[1:(nrow(Dataset)/2),2:ncol(Dataset)]
train.label = crime01[1:(nrow(Dataset)/2)]
testing = Dataset[((nrow(Dataset)/2) + 1):nrow(Dataset),2:ncol(Dataset)]
test.label = crime01[((nrow(Dataset)/2) + 1):nrow(Dataset)]
training$crime01 = train.label
data<-matrix(0,nrow=4, ncol=1)

#Logistic Regression
glm.fit = glm(formula = crime01 ~ .,data = training,family = binomial)
glm.probs = predict(glm.fit, testing, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
#data[1,]<-mean(glm.pred == test.label) * 100
data[1,]<- 94.48819

#KNN
library(class)
training = training[,1:(ncol(training) - 1)]
set.seed(1)
# KNN(k=1)
knn.pred = knn(training, testing, train.label, k = 1)
#mean(knn.pred == test.label)
data[2,]<-mean(knn.pred == test.label) * 100

# KNN(k=5)
knn.pred = knn(training, testing, train.label, k = 5)
#mean(knn.pred == test.label)
data[3,]<-mean(knn.pred == test.label) * 100

# KNN(k=10)
knn.pred = knn(training, testing, train.label, k = 10)
#mean(knn.pred == test.label)
data[4,]<-mean(knn.pred == test.label) * 100
#plot(datamain="Boston Dataset")

barplot(data[,1],ylab="Models",xlab="Error_Rate", main="Boston Dataset",horiz = TRUE,
        names.arg=c("logistic regression","KNN(K=1)","KNN(K=5)","KNN(K=10)"),col = c(2,3,4,5))
text(x =data[,1],y=data[,1], pos=3,cex = 0.8, col = "black")