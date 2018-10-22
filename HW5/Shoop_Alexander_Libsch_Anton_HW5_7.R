# MA 543/DS 502 HW5 - Problem 7/question 10
library('e1071')
set.seed(123)
# 60 random obs, 3 classes, 50 variables
# a
x = rnorm(3*20*50, mean = 0, sd = 0.1)
x = matrix(x + 0.5, ncol = 50)
x[1:20,2] = 1   # rows 1:20 are class1
x[21:40, 1] = 3 # rows 21:40 are class2
x[21:40, 2] = 3 
x[41:60, 1] = 1 # rows 41:60 are class3

# b
# PCA
pr.out = prcomp(x)
summary(pr.out)
plot(pr.out$x[,1:2], col = 4:6, xlab = 'Z1', ylab = 'Z2', pch = 19)

# c
# K = 3
km.out = kmeans(x, 3, nstart=20)
table(km.out$cluster, labels)

# d
# K = 2
km2.out = kmeans(x, 2, nstart=20)
table(km2.out$cluster, labels)

# e
# K = 4
km3.out = kmeans(x, 4, nstart=20)
table(km3.out$cluster, labels)

# f
# K = 3
km4.out = kmeans(x[,1:2], 3, nstart=20)
table(km4.out$cluster, labels)

# g
# default of scale() is sd=1
km5.out = kmeans(scale(x), 3, nstart=20)
table(km5.out$cluster, labels)