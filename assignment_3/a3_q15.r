library(MASS)
data("Boston")
#a
coefs <- data.frame("predictor"=character(0), "Estimate"=numeric(0), "Std.Error"=numeric(0), "t.value"=numeric(0), "Pr.t"=numeric(0), "r.squared"=numeric(0), stringsAsFactors = FALSE)

j<-1
for(i in names(Boston))
{
  if(i !="crim")
  {
    lm_1=lm(crim ~ eval(parse(text=i)), data=Boston)
    coefs[j,]=c(i,summary(lm_1)$coefficients[2,],summary(lm_1)$r.squared)
    j<-j+1
    
  }
}

coefs[,1]<-lapply(coefs[,-1],FUN=function(x)as.numeric(x))
coefs <- coefs[order(coefs$r.squared, decreasing = T),]
print(coefs)


#b
lm_1 <-lm(crim~.,data=Boston)
summary(lm_1)


#c
MyData = data.frame("mult"=summary(lm.fit.b)$coefficients[-1,1])
MyData$simple <- NA
for(i in row.names(MyData)){
  MyData[row.names(MyData)==i, "simple"] = coefs[coefs[,1]==i, "Estimate"]
}

plot(MyData$simple, MyData$mult, xlab="Coef for Simple Linear Regression", ylab="Coef for Multiple Linear Regression")
text(x=MyData$simple, y=MyData$mult, labels=row.names(MyData), cex=.7, col="blue", pos=4)

Myd = MyData[!(row.names(MyData)%in%"nox"),]
plot(Myd$simple, Myd$mult, xlab="Coef for Simple Linear Regression", ylab="Coef for Multiple Linear Regression")
text(x=Myd$simple, y=Myd$mult, labels=row.names(Myd), cex=.7, col="blue", pos=4)


#d
coefs.poly <- data.frame("predictor"=character(0), "Estimate"=numeric(0), "Std.Error"=numeric(0), "t.value"=numeric(0), "Pr.t"=numeric(0), "r.squared"=numeric(0), stringsAsFactors = FALSE)
j <- 1
for(i in names(Boston)){
  if(!(i %in% c("crim", "chas"))){
    summ.lm.fit <- summary(lm(crim ~ poly(eval(parse(text=i)),3), data=Boston))
    coefs.poly[j,] = c(i, summ.lm.fit$coefficients[2,], summ.lm.fit$r.squared)
    j <- j+1
  }
}

coefs.poly[,-1] <- lapply(coefs.poly[,-1], FUN=function(x) as.numeric(x))
coefs.poly <- coefs.poly[order(coefs.poly$r.squared, decreasing = T),]
print(coefs.poly)


df = data.frame("simple"=coefs[,2])
row.names(df) <- coefs[, 1]
df$poly <- NA
for(i in coefs.poly[,1]){
  df[row.names(df)==i, "poly"] <- coefs.poly[coefs.poly[,1]==i, "Estimate"]
}

plot(df$simple, df$poly, xlab="Coef for Simple Linear Regression", ylab="Coef for Poly Linear Regression")
text(x=df$simple, y=df$poly, labels=row.names(df), cex=.7, col="blue", pos=4)

df.clean = df[!(row.names(df)%in%"nox"),]
plot(df.clean$simple, df.clean$mult, xlab="Coef for Simple Linear Regression", ylab="Coef for Poly Linear Regression")
text(x=df.clean$simple, y=df.clean$mult, labels=row.names(df.clean), cex=.7, col="blue", pos=4)