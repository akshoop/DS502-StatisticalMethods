# MA 543/DS 502 HW2 - Problem 9
# ISLR library
library('ISLR')

# a
summary(Default)
set.seed(1)

logit.default.fit <- glm(default ~ income + balance, data = Default, family = "binomial")
summary(logit.default.fit)

# b
boot.fn = function(data,index) {
  coefficients(glm(default ~ income + balance, data = Default, family = "binomial", subset = index))
}

# c
library(boot)
boot(Default, boot.fn, 100)
