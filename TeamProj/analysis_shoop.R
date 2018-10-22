# MA 543/DS 502
# CVS CASESTUDY PROJECT
# Team 4: Alex Shoop, Ming Min, Mohita Goplani, Yijiang (Chuck) Xu
library('caret')

# Importing the pre-setup dataset (which has SPENDSNEXT col, and no NA entries)
df.CVS <- read.csv(file="analysis_data.csv", header=TRUE, sep=",")
attach(df.CVS)
set.seed(1)

# looking at basic plots, and observing any visual trend
plot(df.CVS$TTL_QNTY, df.CVS$SPENDSNEXT, xlab = "TTL_QNTY", ylab = "SPENDSNEXT", main = "SPENDSNEXT vs TTL_QNTY")
boxplot(df.CVS$SPENDSNEXT ~ df.CVS$PRIMARY_VISIT)
# setting up training and test datasets
# random sampling
training = sample(nrow(df.CVS), nrow(df.CVS)/2)
train.CVS = df.CVS[training,]
test.CVS = df.CVS[-training,]

# performing basic regression analysis

# linear regression, GOOD CHOICE
lin.regr <- lm(SPENDSNEXT ~ PRIMARY_VISIT + DAYS_FROM_LAST_VISIT + TTL_QNTY + SPENDS + CPN_FLAG, data = train.CVS)
# the version below is for the variable importance function
#lin.regr <- train(SPENDSNEXT ~ PRIMARY_VISIT + DAYS_FROM_LAST_VISIT + TTL_QNTY + SPENDS + CPN_FLAG, data = train.CVS, method = "lm")
summary(lin.regr)
# diagnosis plots
#par(mfrow=c(2,2))
#plot(lin.regr) #this takes a while
# prediction test
lin.pred = predict(lin.regr, test.CVS)
mean((lin.pred - test.CVS$SPENDSNEXT)^2)
# residual plot
plot(lin.pred[-length(lin.pred)], lin.regr$residuals)

# polynomial fit, NOT A GOOD CHOICE
poly.fit = lm(SPENDSNEXT ~ PRIMARY_VISIT + SPENDS + CPN_FLAG, data = train.CVS)
summary(poly.fit)
poly.pred = predict(poly.fit, test.CVS)
mean((poly.pred - test.CVS$SPENDSNEXT)^2)

# 1/x fit, NOT A GOOD CHOICE
inverse.fit = lm(SPENDSNEXT ~ I(1/PRIMARY_VISIT) + SPENDS + CPN_FLAG, data = train.CVS)
summary(inverse.fit)
inverse.pred = predict(inverse.fit, test.CVS)
mean((inverse.pred - test.CVS$SPENDSNEXT)^2)
plot(inverse.pred[-length(inverse.pred)], inverse.fit$residuals)

# variable importance, for linear regression model
important.regr = varImp(lin.regr, scale = FALSE)
plot(important.regr)
