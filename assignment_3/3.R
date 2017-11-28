library(MASS)
attach(Boston)
lm1 <- lm(crim ~ zn)
par(mfrow=c(2,2))
plot(lm1)

lm2 <- lm(crim ~ indus)
par(mfrow=c(2,2))
plot(lm2)

## F-statistic: 99.82 on 1 and 504 DF,  p-value: < 2.2e-16
chas <- as.factor(chas)
fit.chas <- lm(crim ~ chas)
par(mfrow=c(2,2))


fit.medv <- lm(crim ~ medv)
par(mfrow=c(2,2))
plot(fit.medv)

fit.lstat <- lm(crim ~ lstat)
par(mfrow=c(2,2))
plot(fit.lstat)

fit.black <- lm(crim ~ black)
par(mfrow=c(2,2))
plot(fit.black)

fit.ptratio <- lm(crim ~ ptratio)
par(mfrow=c(2,2))
plot(fit.ptratio)



fit.tax <- lm(crim ~ tax)
par(mfrow=c(2,2))
plot(fit.tax)

fit.rad <- lm(crim ~ rad)
par(mfrow=c(2,2))
plot(fit.rad)

fit.dis <- lm(crim ~ dis)
par(mfrow=c(2,2))
plot(fit.dis)

fit.nox <- lm(crim ~ nox)
par(mfrow=c(2,2))
plot(fit.nox)

fit.rm <- lm(crim ~ rm)
par(mfrow=c(2,2))
plot(fit.rm)


fit.age <- lm(crim ~ age)
par(mfrow=c(2,2))
plot(fit.age)


summary(fit.chas)








simple.reg <- vector("numeric",0)
simple.reg <- c(simple.reg,  lm1$coefficient[2])
simple.reg <- c(simple.reg, lm2$coefficient[2])
simple.reg <- c(simple.reg, chas$coefficient[2])
simple.reg <- c(simple.reg, fit.nox$coefficient[2])
simple.reg <- c(simple.reg, fit.rm$coefficient[2])
simple.reg <- c(simple.reg, fit.age$coefficient[2])
simple.reg <- c(simple.reg, fit.dis$coefficient[2])
simple.reg <- c(simple.reg, fit.rad$coefficient[2])
simple.reg <- c(simple.reg, fit.tax$coefficient[2])
simple.reg <- c(simple.reg, fit.ptratio$coefficient[2])
simple.reg <- c(simple.reg, fit.black$coefficient[2])
simple.reg <- c(simple.reg, fit.lstat$coefficient[2])
simple.reg <- c(simple.reg, fit.medv$coefficient[2])
mult.reg <- vector("numeric", 0)
mult.reg <- c(mult.reg, fit.all$coefficients)
mult.reg <- mult.reg[-1]
plot(simple.reg, mult.reg, col = "red")


