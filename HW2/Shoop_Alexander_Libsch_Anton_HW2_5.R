# MA 543/DS 502 HW2 - Problem 5
# ISLR library
library('ISLR')

# a, setup
mpg01 = rep(0,nrow(Auto))  
mpg01[Auto$mpg > median(Auto$mpg)] = 1
Autompg01.df = data.frame(Auto,mpg01)

# b
pairs(Autompg01.df)
boxplot(Auto$cylinders~Auto$mpg01, xlab="mpg01",ylab="Cylinders")
boxplot(Auto$displacement~Auto$mpg01, xlab="mpg01",ylab="Displacement")
boxplot(Auto$horsepower~Auto$mpg01, xlab="mpg01",ylab="Horsepower")
boxplot(Auto$weight~Auto$mpg01, xlab="mpg01",ylab="Weight")
boxplot(Auto$acceleration~Auto$mpg01, xlab="mpg01",ylab="Acceleration")
boxplot(Auto$year~Auto$mpg01, xlab="mpg01",ylab="Year")
boxplot(Auto$origin~Auto$mpg01, xlab="mpg01",ylab="Origin")

# c
# year split
# training data: years 1970 to 1976, test data: years 1977 to 1980
train.c.old = (Autompg01.df$year <= 76)
train.c.new <- Autompg01.df[!train.c.old,]
mpg01.c.new = Autompg01.df$mpg01[!train.c.old]

# d
library(MASS)
lda.mpg01.fit = lda(mpg01 ~ cylinders + displacement + horsepower + weight, data = Autompg01.df, subset = train.c.old)
lda.mpg01.fit
lda.mpg01.pred = predict(lda.mpg01.fit, train.c.new)
lda.class = lda.mpg01.pred$class
table(lda.class, mpg01.c.new)
mean(lda.class == mpg01.c.new)

# e
qda.mpg01.fit = qda(mpg01 ~ cylinders + displacement + horsepower + weight, data = Autompg01.df, subset = train.c.old)
qda.mpg01.fit
qda.class = predict(qda.mpg01.fit, train.c.new)$class
table(qda.class, mpg01.c.new)
mean(qda.class == mpg01.c.new)

# f
logit.mpg01.fits <- glm(mpg01 ~ cylinders + displacement + horsepower + weight, data = Autompg01.df, subset = train.c.old)
logit.mpg01.probs = predict(logit.mpg01.fits, train.c.new, type="response")
logit.mpg01.pred = rep(0, 178)
logit.mpg01.pred[logit.mpg01.probs > .5] = 1
table(logit.mpg01.pred, mpg01.c.new)
mean(logit.mpg01.pred == mpg01.c.new)
summary(logit.mpg01.fits)

# g
library(class)
train.g.X = cbind(Autompg01.df$cylinders, Autompg01.df$displacement, Autompg01.df$horsepower, Autompg01.df$weight)[train.c.old,]
test.g.X = cbind(Autompg01.df$cylinders, Autompg01.df$displacement, Autompg01.df$horsepower, Autompg01.df$weight)[!train.c.old,]
train.g.mpg01 = Autompg01.df$mpg01[train.c.old]
set.seed(1)
knn.pred = knn(train.g.X, test.g.X, train.g.mpg01, k = 100)
table(knn.pred, mpg01.c.new)
mean(knn.pred == mpg01.c.new)