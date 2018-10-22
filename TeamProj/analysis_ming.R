data = read.csv("analysis_data.csv")
data = data[-c(1,2,3)]
############## with coupon
indicator = c()
for(i in 1:nrow(data)){
  if(data[i, "CPN_FLAG"]==0){
    
    indicator[i] = FALSE
  } else {indicator[i] = TRUE}
}
data = data[indicator,-5]
##### without coupon
#data = data[-indicator, -5]
##################
set.seed(1)
training = sample(nrow(data), nrow(data)/2)
train = data[training, ]
test = data[-training, ]

# lasso
library(glmnet)
y.train = train$SPENDSNEXT
x.train = model.matrix(SPENDSNEXT~., train)[, -1]
x.test = model.matrix(SPENDSNEXT~., test)[, -1]
y.test = test$SPENDSNEXT
x = model.matrix(SPENDSNEXT~., data)[, -1]
y = data$SPENDSNEXT
grid=10^seq(10,-2,length=100)
lasso.mod = glmnet(x.train, y.train, alpha=1, lambda = grid)
cv.out = cv.glmnet(x.train, y.train,alpha = 1, lambda = grid)
plot(cv.out)
best.lam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s=best.lam, newx=x.test)
print("This is our test error with lasso")
mean((lasso.pred-y.test)^2)

# use whole data set to train our model
out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s=best.lam)[1:6, ]
print(lasso.coef)
# we can plot the residuals to see if linear regression is a proper method
yhat = predict(out, s=best.lam, newx=x)
residuals = yhat - y
plot(yhat,residuals)
# it turns out not, we'll try the log because of the shape of residuals plot

# try subset selection
library(leaps)
regfit.full = regsubsets(SPENDSNEXT~., data)
reg.summary = summary(regfit.full)
print(reg.summary)
print(reg.summary$adjr2)
# From the adjusted R2, is not a very good model




##################################
# coupon takes value of only 0&1, so we just try log(Spends)
data$LogSpends = log(data$SPENDS)
# seperate the data into train and test part
set.seed(1)
training = sample(nrow(data), nrow(data)/2)
train = data[training, ]
test = data[-training, ]
# lasso
library(glmnet)
y.train = train$SPENDSNEXT
x.train = model.matrix(SPENDSNEXT~., train)[, -1]
x.test = model.matrix(SPENDSNEXT~., test)[, -1]
y.test = test$SPENDSNEXT
x = model.matrix(SPENDSNEXT~., data)[, -1]
y = data$SPENDSNEXT
grid=10^seq(10,-2,length=100)
lasso.mod = glmnet(x.train, y.train, alpha=1, lambda = grid)
cv.out = cv.glmnet(x.train, y.train,alpha = 1, lambda = grid)
plot(cv.out)
best.lam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s=best.lam, newx=x.test)
print("This is our test error with lasso")
mean((lasso.pred-y.test)^2)

# use whole data set to train our model
out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s=best.lam)
print(lasso.coef)
# we can plot the residuals to see if linear regression is a proper method
yhat = predict(out, s=best.lam, newx=x)
residuals = -yhat + y

plot(x[,'LogSpends'],residuals)
plot(x[, 'SPENDS'], residuals)

# try subset selection
library(leaps)
regfit.full = regsubsets(SPENDSNEXT~., data)
reg.summary = summary(regfit.full)
print(reg.summary)
print(reg.summary$adjr2)

