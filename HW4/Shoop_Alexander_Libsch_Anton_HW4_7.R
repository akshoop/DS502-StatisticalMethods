# MA 543/DS 502 HW4 - Problem 7/question 8
library('ISLR')
library('tree')
library('randomForest')
attach(Carseats)
summary(Carseats)

# a
set.seed(1)
training = sample(nrow(Carseats), nrow(Carseats)/2)
carseats.train = Carseats[training,]
carseats.test = Carseats[-training,]

# b
tree.carseats = tree(Sales~., data = carseats.train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
pred.carseats = predict(tree.carseats, carseats.test)
mean((carseats.test$Sales - pred.carseats)^2)

# c
cv.carseats = cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
bestSize = which.min(cv.carseats$dev)
#pruning
prune.carseats = prune.tree(tree.carseats, best = bestSize)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
pred2.carseats = predict(prune.carseats, carseats.test)
mean((carseats.test$Sales - pred2.carseats)^2)

# d
bag.carseats = randomForest(Sales~., data = carseats.train, mtry=10, importance =TRUE)
bag.pred = predict(bag.carseats, carseats.test)
mean((carseats.test$Sales - bag.pred)^2)
importance(bag.carseats)

# e
rf.carseats = randomForest(Sales~., data = carseats.train, mtry = 5, importance = TRUE)
rf.pred = predict(rf.carseats, carseats.test)
mean((carseats.test$Sales - rf.pred)^2)
importance(rf.carseats)
