# MA 543/DS 502 HW4 - Problem 5/question 6
library('ISLR')
library('glm')
library(boot)
attach(Wage)
summary(Wage)

# a
# optimal degree using CV (10-fold)
set.seed(1)
k = 10
folds = sample(1:k, nrow(Wage), replace = TRUE)
cv.errors = matrix(NA, k, 10, dimnames=list(NULL,paste(1:10)))
# for loop
for (j in 1:k) {
  for (i in 1:10) {
    lm.fit = lm(wage~poly(age,i,raw=T), data = Wage[folds!=j,])
    
    pred = predict(lm.fit,Wage[folds==j,])
    cv.errors[j,i] = mean( (Wage$wage[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
which.min(mean.cv.errors) 

# optimal degree using ANOVA
fit.1 = lm(wage~poly(age, 1), data=Wage)
fit.2 = lm(wage~poly(age, 2), data=Wage)
fit.3 = lm(wage~poly(age, 3), data=Wage)
fit.4 = lm(wage~poly(age, 4), data=Wage)
fit.5 = lm(wage~poly(age, 5), data=Wage)
fit.6 = lm(wage~poly(age, 6), data=Wage)
fit.7 = lm(wage~poly(age, 7), data=Wage)
fit.8 = lm(wage~poly(age, 8), data=Wage)
fit.9 = lm(wage~poly(age, 9), data=Wage)
fit.10 = lm(wage~poly(age, 10), data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5,fit.6,fit.7,fit.8,fit.9,fit.10) 

# plotting using the 9 degree poly fit
ageRange = data.frame(age = seq(min(Wage$age),max(Wage$age), by = 0.1))
plot(Wage$age, Wage$wage, ylab = "Wage", xlab = "Age", main = "Wage vs Age", col = "darkgrey")
lines(ageRange$age,predict(fit.9, ageRange), col="red", lwd=3)

# b
# use CV to get optimal cut
cv.array = rep(NA,10)
for (i in 2:10) {
  Wage$age.cut = cut(Wage$age, i)
  lm.fit = glm(wage~age.cut, data=Wage)
  cv.array[i] = cv.glm(Wage, lm.fit, K = 10)$delta[2]
}
plot(2:10, cv.array[-1], xlab="Number of cuts", ylab="CV error", type="l", pch=20, lwd=2)

#therefore, optimal cuts is 8
lm.fit = glm(wage~cut(age, 8), data = Wage)
ageRange = range(Wage$age)
age.interval = seq(from = ageRange[1], to = ageRange[2])
lm.pred = predict(lm.fit, data.frame(age = age.interval))
plot(wage~age, data=Wage, main = "Wage vs Age", col="darkgrey")
lines(age.interval, lm.pred, col="red", lwd=2)
