# MA 543/DS 502 HW4 - Problem 2/question 11
# Boston data
# "crim" is the per capita crime rate response var
library('MASS')
library('ISLR')
library('leaps')
library('glmnet')
library('pls')

#There are no NA entries, sum(is.na(Boston)) = 0
#Basic summary and scatter plots
summary(Boston)
pairs(Boston)

#Performing best subset now
#regfit.full = regsubsets(crim~., data=Boston, nvmax=13)
#reg.summary=summary(regfit.full)
#The above indicated vars to use, which is ncol(Boston)-1 = 13

# Best subset selection
#Using 10-fold CV model selection
k = 10
set.seed(1)
folds = sample(1:k, nrow(Boston), replace=TRUE)
cv.errors = matrix(NA, k, 13, dimnames=list(NULL, paste(1:13)))
#predict function from textbook
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}
#for loop
for (j in 1:k) {
  best.fit = regsubsets(crim~., data = Boston[folds!=j,], nvmax = 13)
  for (i in 1:13) {
    pred = predict(best.fit, Boston[folds==j,], id = i)
    cv.errors[j,i] = mean( (Boston$crim[folds==j] - pred)^2 )
  }
}
#average of CV errors
mean.cv.errors = apply(cv.errors, 2, mean)
par(mfrow=c(1,1))
plot(mean.cv.errors, type = 'b')
#based on the work above, the best subset selection model tells us
#that 9 parameters seem to be the most optimal model
mean(mean.cv.errors)

# Lasso, with CV
boston.x = model.matrix(crim~., data = Boston)
boston.y = Boston$crim
cv.lasso = cv.glmnet(boston.x, boston.y, alpha = 1)
plot(cv.lasso)
coef(cv.lasso)
mean(cv.lasso$cvm)

# Ridge, with CV
cv.ridge = cv.glmnet(boston.x, boston.y, alpha = 0)
plot(cv.ridge)
coef(cv.ridge)
mean(cv.ridge$cvm)

# PCR
pcr.fit = pcr(crim~., data = Boston, scale = TRUE, validation = "CV")
summary(pcr.fit)
#we can square the RMSE (ie, root mean square errors),
#or we can plot the validationplot
validationplot(pcr.fit, val.type = "MSEP")
