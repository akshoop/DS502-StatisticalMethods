# MA 543/DS 502 HW5 - Problem 5/question 5
library('e1071')
set.seed(123)
# 500 random obs
x1 = runif(500) - 0.7
x2 = runif(500) - 0.3
y = 1*(x1^2 - x2^2 > 0)

# b
plot(x1[y == 0], x2[y == 0], col = 'red', xlab="X1", ylab="X2", pch="+")
points(x1[y == 1], x2[y == 1], col = 'blue',pch=4)

# c
logit.fit = glm(y ~ x1 + x2, family = 'binomial')
summary(logit.fit)

# d
DF = data.frame(x1 = x1, x2 = x2, y = y)
logit.probs = predict(logit.fit, DF, type = 'response')
pred = rep(0,500)
pred[logit.probs > 0.51] <- 1
DF.pos = DF[pred == 1,]
DF.neg = DF[pred == 0,]
plot(DF.pos$x1, DF.pos$x2, col = 'blue', xlab = 'X1', ylab = 'X2', pch = '+')
points(DF.neg$x1, DF.neg$x2, col = 'red', pch = 4)

# e
logit2.fit = glm(y ~ poly(x1,2) + poly(x2,2), family = 'binomial')
summary(logit2.fit)

# f
logit2.probs = predict(logit2.fit, DF, type = 'response')
pred = rep(0,500)
pred[logit2.probs > 0.51] <- 1
DF2.pos = DF[pred == 1, ]
DF2.neg = DF[pred == 0, ]
plot(DF2.pos$x1, DF2.pos$x2, col = 'blue', xlab = 'X1', ylab = 'X2', pch = '+')
points(DF2.neg$x1, DF2.neg$x2, col = 'red', pch = 4)

# g
svm.fit = svm(y ~ x1 + x2, data = DF, kernel = "linear", cost = 0.1)
svm.preds = rep(0,500)
svm.preds = predict(svm.fit, DF)
svm.preds[svm.preds > 0.51] <- 1
DF3.pos = DF[svm.preds == 1,]
DF3.neg = DF[svm.preds == 0,]
plot(DF3.pos$x1, DF3.pos$x2, col = 'blue', xlab = 'X1', ylab = 'X2', pch = '+')
points(DF3.neg$x1, DF3.neg$x2, col = 'red', pch = 4)

# h
svmnonLin.fit = svm(y ~ x1 + x2, data = DF, kernel = "radial")
svmnonLin.preds = rep(0,500)
svmnonLin.preds = predict(svmnonLin.fit, DF)
svmnonLin.preds[svmnonLin.preds > 0.51] <- 1
DF4.pos = DF[svmnonLin.preds == 1,]
DF4.neg = DF[svmnonLin.preds < 1,]
plot(DF4.pos$x1, DF4.pos$x2, col = 'blue', xlab = 'X1', ylab = 'X2', pch = '+')
points(DF4.neg$x1, DF4.neg$x2, col = 'red', pch = 4)
