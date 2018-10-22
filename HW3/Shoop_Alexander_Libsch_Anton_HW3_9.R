# MA 543/DS 502 HW3 - Problem 4/question 9
# ISLR library
library(ISLR)

# a
set.seed(1)
train = sample(c(TRUE,FALSE), nrow(College), rep=TRUE)
test = (!train)
College.train = College[train,]
College.test = College[test,]

# b
# number of college apps is Apps, target
# test average
test.avg = mean((College.test$Apps))
lm.fit = lm(Apps~., data = College.train)
summary(lm.fit)
lm.pred = predict(lm.fit, College.test)
mean((lm.pred - College.test$Apps)^2)
lm.testr2 = 1 - mean((lm.pred - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
  
# c
library(glmnet)
trainmatrix = model.matrix(Apps~., data = College.train)
testmatrix = model.matrix(Apps~., data = College.test)
grid = 10^seq(4,-2,length=100)
cv.ridge = cv.glmnet(trainmatrix.c, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
bestlambda1 = min(cv.ridge$lambda)
bestlambda1
ridge.pred = predict(cv.ridge, s = bestlambda1, newx = testmatrix)
mean((ridge.pred - College.test$Apps)^2)

# d
lasso.mod = glmnet(trainmatrix, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
bestlambda2 = min(lasso.mod$lambda)
bestlambda2
lasso.pred = predict(lasso.mod, s = bestlambda2, newx = testmatrix)
mean((lasso.pred - College.test$Apps)^2)
# number of non-zero coeff
nrow(predict(lasso.mod, s = bestlambda2, newx = testmatrix, type = "coefficients"))

# e
library(pls)
pcr.fit = pcr(Apps~., data = College.train, scale = TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred = predict(pcr.fit, College.test, ncomp = 17)
mean((pcr.pred - College.test$Apps)^2)

# f
pls.fit = plsr(Apps~., data = College.train, scale = TRUE, validation="CV")
validationplot(pls.fit, val.type="MSEP")
pls.pred = predict(pls.fit, College.test, ncomp = 10)
mean((pls.pred - College.test$Apps)^2)

