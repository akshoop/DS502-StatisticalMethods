# MA 543/DS 502 HW5 - Problem 4/question 4
library('e1071')
set.seed(123)
# 100 random obs
x = rnorm(100)
y = 6*x^2 + 3 + rnorm(100)
classSeqX = seq(min(x), max(x), length.out = 100)
classSeqY = classSeqX^2 + 5
factr = rep("Pink", 100)
factr[y < classSeqY] = "Teal"
factr <- as.factor(factr)

DF = data.frame(x, y, factr)
training = sample(100, 50)
train.DF = DF[training,]
test.DF = DF[-training,]

svmfit = svm(factr ~ ., data = train.DF, kernel = "linear", cost = 10)
plot(svmfit, train.DF)

table(pred = predict(svmfit, train.DF), actual = train.DF$factr)

svmfitPoly = svm(factr ~ ., data = train.DF, kernel = "polynomial", cost = 10)
plot(svmfitPoly, train.DF)

table(predict = predict(svmfit, test.DF), truth = test.DF$factr)