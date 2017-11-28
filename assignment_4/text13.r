library(MASS)
attach(Boston)
crim01<-rep(0,length(crim))
crim01[crim > median(crim)] = 1

crim01

Boston <- data.frame(Boston,crim01)


train <- 1:(length(crim)/2)
test <- (length(crim) / 2 + 1):length(crim)
Boston.train<- Boston[train,]
Boston.test<-Boston[test,]
crim01.test<-crim01[test]

fit.glm<-glm(crim01 ~ .-crim01-crim,data=Boston,family=binomial,subset=train)


probs <- predict(fit.glm,Boston.test,type="response")
pred.glm<-rep(0,length(probs))
pred.glm[probs>0.5]<-1
table(pred.glm, crim01.test)

mean(pred.glm != crim01.test)


fit.glm<-glm(crim01~.-crim01-crim-chas-nox,data = Boston,family = binomial, subset=train)


probs <- predict(fit.glm,Boston.test,type="response")
pred.glm<-rep(0,length(probs))
pred.glm[probs>0.5]<-1
table(pred.glm, crim01.test)


mean(pred.glm != crim01.test)



