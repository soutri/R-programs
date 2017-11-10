#a
set.seed(1)
x<-rnorm(100,mean=0,sd=1)

#b
eps<-rnorm(100,sd= sqrt(0.25))

#c
y<- -1+0.5*x+eps
length(y)

#d
plot(x,y)

#e
least_square <- lm(y ~ x)
least_square$coefficients

#f
plot(x, y)
abline(least_square, col = "blue")
abline(-1, 0.5, col = "red")
legend("topleft",c("Least square", "Regression"), col = c("blue", "red"), lty = c(1,1))


#g
fit<- lm(y~x+ I(x^2))
fit$coefficients
anova(least_square, fit)


#h
eps1 <- rnorm(100, sd=0.125)
x<- rnorm(100)
y<- -1+0.5*x +eps1
plot(x,y)
fit2 <- lm(y ~ x)
summary(fit2)
abline(fit2, col = "red")
abline(-1, 0.5, col = "blue")
legend("topleft", c("our model", "true model"), col = c("red", "blue"), lty = c(1, 1))


#i
set.seed(1)
eps <- rnorm(100, sd = 0.5)
x <- rnorm(100)
y <- -1 + 0.5 * x + eps
plot(x, y)
fit3 <- lm(y ~ x)
abline(fit3, col = "red")
abline(-1, 0.5, col = "blue")
summary(fit3)$coefficients

#j
confint(least_square)
confint(fit)
confint(fit2)
confint(fit3)


















