#a
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)

#b
cor(x1, x2)
plot(x1,x2)

#c
fit13<-lm(y~x1+x2)
fit13$coefficients

#d
fit14<-lm(y~x1)
summary(fit14)$coefficients

#e
fit15<-lm(y ~ x2)
summary(fit15)$coefficients

#g
x1<-c(x1,0.1)
x2<-c(x2,0.8)
y<-c(y,6)

fit16<-lm(y~x1+x2)
fit17<-lm(y~x1)
fit18<-lm(y~x2)
summary(fit16)$coefficients
summary(fit17)$coefficients
summary(fit18)$coefficients

plot(fit16)
plot(fit17)
plot(fit18)

cor(x1,x2)
plot(x1,x2)

par(mfrow=c(2,2))
plot(fit16)








