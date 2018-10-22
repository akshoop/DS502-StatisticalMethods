# MA 543/DS 502 HW2 - Problem 8
# ISLR library
library('ISLR')

# a
summary(Default)
set.seed(1)
logit.default.fit <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(logit.default.fit)

# b, validation set approach
train.b = sample(nrow(Default), nrow(Default)/2)
test.b <- Default[-train.b,]
default.b.actual <- Default[-train.b,]$default
logit.b.fit <- glm(default ~ income + balance, data = Default, family = "binomial", subset = train.b)
logit.b.probs = predict(logit.b.fit, test.b, type = "response")
logit.b.pred = rep("No",nrow(Default)/2)
logit.b.pred[logit.b.probs > .5] = "Yes"
table(logit.b.pred, default.b.actual)
(1 - mean(logit.b.pred == default.b.actual)) * 100 # test error rate
# run the above block multiple times (for part c)

# d
# dummy variable, student
# not necessary to create 0/1 variable of student
train.d = sample(nrow(Default), nrow(Default)/2)
test.d <- Default[-train.d,]
default.d.actual <- Default[-train.d,]$default
logit.d.fit <- glm(default ~ income + balance + student, data = Default, family = "binomial", subset = train.d)
logit.d.probs = predict(logit.d.fit, test.d, type = "response")
logit.d.pred = rep("No",nrow(Default)/2)
logit.d.pred[logit.d.probs > .5] = "Yes"
table(logit.d.pred, default.d.actual)
(1 - mean(logit.d.pred == default.d.actual)) * 100 # test error rate

