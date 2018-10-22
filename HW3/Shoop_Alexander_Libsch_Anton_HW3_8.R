# MA 543/DS 502 HW3 - Problem 3/question 8
# ISLR library
library(ISLR)

# a
set.seed(1)
X <- rnorm(100)
eps <- rnorm(100)

# b
beta0 = 3
beta1 = 2
beta2 = -2
beta3 = 0.8
Y = beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps

# c
library(leaps)
df = data.frame(Y = Y, X = X)
regfit.best = regsubsets(Y~poly(X,10, raw = T), data = df, nvmax = 10)
bestsubset.summary = summary(regfit.best)
# best model based on best values
which.min(bestsubset.summary$cp)
which.min(bestsubset.summary$bic)
which.max(bestsubset.summary$adjr2)
# plotting
par(mfrow=c(2,2))
plot(bestsubset.summary$rss, xlab = "Number of Variables", ylab = "RSS", type="l")
plot(bestsubset.summary$adjr2, xlab = "Number of Variables", ylab="Adjusted RSq", type="l")
points(which.max(bestsubset.summary$adjr2),
       bestsubset.summary$adjr2[which.max(bestsubset.summary$adjr2)], 
       col="red",cex=2,pch=20)
plot(bestsubset.summary$cp, xlab = "Number of Variables", ylab = "Cp", type="l")
points(which.min(bestsubset.summary$cp),
       bestsubset.summary$cp[which.min(bestsubset.summary$cp)],
       col="red",cex=2,pch=20)
plot(bestsubset.summary$bic , xlab = "Number of Variables", ylab = "BIC", type="l")
points(which.min(bestsubset.summary$bic),
       bestsubset.summary$bic[which.min(bestsubset.summary$bic)],
       col="red",cex=2,pch=20)
# coeffs
# BIC
coef(regfit.best, 3)
# Cp / adjusted R2
coef(regfit.best, 4)

# d
regfit.fwd = regsubsets(Y~poly(X,10, raw = T), data = df, nvmax = 10, method="forward")
fwd.summary = summary(regfit.fwd)
# best model based on smallest values
which.min(fwd.summary$cp)
which.min(fwd.summary$bic)
which.max(fwd.summary$adjr2)
# plotting
plot(fwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type="l")
plot(fwd.summary$adjr2, xlab = "Number of Variables", ylab="Adjusted RSq", type="l")
points(which.max(fwd.summary$adjr2),
       fwd.summary$adjr2[which.max(fwd.summary$adjr2)], 
       col="red",cex=2,pch=20)
plot(fwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type="l")
points(which.min(fwd.summary$cp),
       fwd.summary$cp[which.min(fwd.summary$cp)],
       col="red",cex=2,pch=20)
plot(fwd.summary$bic , xlab = "Number of Variables", ylab = "BIC", type="l")
points(which.min(fwd.summary$bic),
       fwd.summary$bic[which.min(fwd.summary$bic)],
       col="red",cex=2,pch=20)
# coeffs
# BIC
coef(regfit.fwd, 3)
# Cp / adjusted R2
coef(regfit.fwd, 4)


regfit.bwd = regsubsets(Y~poly(X,10, raw = T), data = df, nvmax = 10, method="backward")
bwd.summary = summary(regfit.bwd)
# best model based on smallest values
which.min(bwd.summary$cp)
which.min(bwd.summary$bic)
which.max(bwd.summary$adjr2)
# plotting
plot(bwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type="l")
plot(bwd.summary$adjr2, xlab = "Number of Variables", ylab="Adjusted RSq", type="l")
points(which.max(bwd.summary$adjr2),
       bwd.summary$adjr2[which.max(bwd.summary$adjr2)], 
       col="red",cex=2,pch=20)
plot(bwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type="l")
points(which.min(bwd.summary$cp),
       bwd.summary$cp[which.min(bwd.summary$cp)],
       col="red",cex=2,pch=20)
plot(bwd.summary$bic , xlab = "Number of Variables", ylab = "BIC", type="l")
points(which.min(bwd.summary$bic),
       bwd.summary$bic[which.min(bwd.summary$bic)],
       col="red",cex=2,pch=20)
# coeffs
# BIC / Cp
coef(regfit.bwd, 3)
# adjusted R2
coef(regfit.bwd, 4)

# e
# lasso
library(glmnet)
dmat = model.matrix(Y~poly(X,10, raw = T), data = df)[,-1]
lasso.mod = glmnet(dmat, Y, alpha = 1)
plot(lasso.mod)
bestlambda = min(lasso.mod$lambda)
bestlambda
bestlasso = glmnet(dmat, Y, alpha = 1)
predict(bestlasso, s = bestlambda, type="coefficients")

# f
beta7 = -1.7
Y2 = beta0 + beta7 * X^7 + eps
df2 = data.frame(Y = Y2, X = X)

# best subset
regfit.best2 = regsubsets(Y2~poly(X,10, raw = T), data = df2, nvmax = 10)
bestsubset2.summary = summary(regfit.best2)
# best model based on smallest values
which.min(bestsubset2.summary$cp)
which.min(bestsubset2.summary$bic)
which.max(bestsubset2.summary$adjr2)
# plotting
plot(bestsubset2.summary$rss, xlab = "Number of Variables", ylab = "RSS", type="l")
plot(bestsubset2.summary$adjr2, xlab = "Number of Variables", ylab="Adjusted RSq", type="l")
points(which.max(bestsubset2.summary$adjr2),
       bestsubset2.summary$adjr2[which.max(bestsubset2.summary$adjr2)], 
       col="red",cex=2,pch=20)
plot(bestsubset2.summary$cp, xlab = "Number of Variables", ylab = "Cp", type="l")
points(which.min(bestsubset2.summary$cp),
       bestsubset2.summary$cp[which.min(bestsubset2.summary$cp)],
       col="red",cex=2,pch=20)
plot(bestsubset2.summary$bic , xlab = "Number of Variables", ylab = "BIC", type="l")
points(which.min(bestsubset2.summary$bic),
       bestsubset2.summary$bic[which.min(bestsubset2.summary$bic)],
       col="red",cex=2,pch=20)
# coeffs
# Adj R2
coef(regfit.best2, 4)
# Cp
coef(regfit.best2, 2)
# BIC
coef(regfit.best2, 1)


# lasso
dmat2 = model.matrix(Y2~poly(X,10, raw = T), data = df2)[,-1]
lasso.mod2 = glmnet(dmat2, Y2, alpha = 1)
plot(lasso.mod2)
bestlambda2 = min(lasso.mod2$lambda)
bestlambda2
bestlasso2 = glmnet(dmat2, Y2, alpha = 1)
predict(bestlasso2, s = bestlambda2, type="coefficients")

