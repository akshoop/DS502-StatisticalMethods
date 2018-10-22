# MA 543/DS 502 HW2 - Problem 4
# ISLR library
library('ISLR')

# a
summary(Weekly)
boxplot(Weekly[,2:6],ylab="Value")
boxplot(Weekly[,1:1],ylab="Year")
boxplot(Weekly[,7:7],ylab="Volume")
boxplot(Weekly[,8:8],ylab="Today")
counts <- table(Weekly$Direction)
barplot(counts, main="Direction", ylab = "Count")

# b
logit4 <- glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = "binomial", data = Weekly)
summary(logit4)

# c
logit4.probs=predict(logit4,type = "response")
logit4.probs[1:10] # probability of the market going up, weekly
contrasts(Weekly$Direction)
logit4.pred=rep("Down",1089)
logit4.pred[logit4.probs>.5]="Up"
table(logit4.pred,Weekly$Direction)

# d
train.d.less = (Weekly$Year <= 2008)
train.d.greater <- Weekly[!train.d.less,]
dim(train.d.greater) # to find number of weeks of the 2009 & 2010 data
Direction.d.greater = Weekly$Direction[!train.d.less]
logit4d.fits <- glm(Direction ~ Lag2, data = Weekly, family = "binomial", subset = train.d.less)
logit4d.probs = predict(logit4d.fits,train.d.greater,type="response")
logit4d.pred = rep("Down", 104)
logit4d.pred[logit4d.probs>.5] = "Up"
table(logit4d.pred,Direction.d.greater)
mean(logit4d.pred==Direction.d.greater)
summary(logit4d.fits)

# e
library(MASS)
lda4e.fits = lda(Direction ~ Lag2, data = Weekly, subset=train.d.less)
lda4e.fits
lda4e.pred = predict(lda4e.fits, train.d.greater)
names(lda4e.pred)
lda.class = lda4e.pred$class
table(lda.class,Direction.d.greater)
mean(lda.class==Direction.d.greater)

# f
qda.fit = qda(Direction ~ Lag2, data = Weekly, subset=train.d.less)
qda.fit
qda.class = predict(qda.fit,train.d.greater)$class
table(qda.class,Direction.d.greater)
mean(qda.class==Direction.d.greater)

# g
library(class)
train.g.X = as.matrix((Weekly$Lag2)[train.d.less])
test.g.X = as.matrix((Weekly$Lag2)[!train.d.less])
train.g.Direction = Weekly$Direction[train.d.less]
set.seed(1)
knn.pred = knn(train.g.X, test.g.X, train.g.Direction, k = 1)
table(knn.pred, Direction.d.greater)

# i
logit4i.fits <- glm(Direction ~ Lag5 + Lag3*Lag4, data = Weekly, family = "binomial", subset = train.d.less)
logit4i.probs = predict(logit4i.fits, train.d.greater, type="response")
logit4i.pred = rep("Down", 104)
logit4i.pred[logit4i.probs>.5] = "Up"
table(logit4i.pred, Direction.d.greater)
mean(logit4i.pred == Direction.d.greater)
summary(logit4i.fits)

lda4i.fits = lda(Direction ~ Lag5 + Lag3*Lag4, data = Weekly, subset=train.d.less)
lda4i.fits
lda4i.pred = predict(lda4i.fits, train.d.greater)
lda4i.class = lda4i.pred$class
table(lda4i.class, Direction.d.greater)
mean(lda4i.class == Direction.d.greater)

qda4i.fit = qda(Direction ~ Lag5 + Lag3*Lag4, data = Weekly, subset=train.d.less)
qda4i.fit
qda4i.class = predict(qda4i.fit,train.d.greater)$class
table(qda4i.class, Direction.d.greater)
mean(qda4i.class == Direction.d.greater)

train.i.X = cbind(Weekly$Lag5, Weekly$Lag3*Weekly$Lag4)[train.d.less,]
test.i.X = cbind(Weekly$Lag5, Weekly$Lag3*Weekly$Lag4)[!train.d.less,]
train.i.Direction = Weekly$Direction[train.d.less]
set.seed(1)
knn.i.pred = knn(train.i.X, test.i.X, train.i.Direction, k = 100)
table(knn.i.pred, Direction.d.greater)
mean(knn.i.pred == Direction.d.greater)
