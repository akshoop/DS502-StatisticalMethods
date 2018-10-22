# MA 543/DS 502 HW5 - Problem 2/question 9
# ISLR, for OJ dataset
library('ISLR')
library('tree')
attach(OJ)
set.seed(123)

#Basic summary and scatter plots
summary(OJ)
pairs(OJ)

# a
# training and test datasets
training = sample(nrow(OJ), 800, replace=FALSE)
train.OJ = OJ[training,]
test.OJ = OJ[-training,]

# b
# fitting a tree
tree.OJ = tree(Purchase ~ ., data = train.OJ)
summary(tree.OJ)

# c
tree.OJ

# d
plot(tree.OJ)
text(tree.OJ)

# e
pred.OJ = predict(tree.OJ, test.OJ, type="class")
table(test.OJ$Purchase, pred.OJ)

# f
cv.OJ = cv.tree(tree.OJ)

# g
plot(cv.OJ$size, cv.OJ$dev, type = 'b', xlab = "Tree Size", ylab = "CV error rate")

# i
pruned.OJ = prune.tree(tree.OJ, best = 6)
summary(pruned.OJ)

# k
pred.orig = predict(tree.OJ, test.OJ, type = "class")
testError.orig = sum(test.OJ$Purchase != pred.orig) / length(pred.orig)
testError.orgi
pred.pruned = predict(pruned.OJ, test.OJ, type = "class")
testError.pruned = sum(test.OJ$Purchase != pred.pruned) / length(pred.pruned)
testError.pruned