# MA 543/DS 502
# CVS CASESTUDY PROJECT
# Team 4: Alex Shoop, Ming Min, Mohita Goplani, Yijiang (Chuck) Xu
library('caret')

# Importing the pre-setup dataset (which has SPENDSNEXT col, and no NA entries)
df.CVS <- read.csv(file="analysis_data.csv", header=TRUE, sep=",")
# removing unnecessary 1st column
df.CVS <- df.CVS[,-1]
attach(df.CVS)
set.seed(1)

# looking at basic plots, and observing any visual trend
# needs more plots and visuals
plot(df.CVS$TTL_QNTY, df.CVS$SPENDS, xlab = "TTL_QNTY", ylab = "SPENDS", main = "TTL_QNTY vs SPENDS")
plot(df.CVS$PRIMARY_VISIT, df.CVS$SPENDS, xlab = "PRIMARY_VISIT", ylab = "SPENDS", main = "PRIMARY_VISIT vs SPENDS")
boxplot(df.CVS$SPENDS ~ df.CVS$CPN_FLAG)
pairs(data.frame(df.CVS$SPENDS, df.CVS[3:5], df.CVS$CPN_FLAG), labels = c("SPENDS","PRIMARY_VISIT","DAYS_FROM_LAST_VISIT","TTL_QNTY","CPN_FLAG"), lower.panel = NULL)

# setting up training and test datasets
# random sampling
training = sample(nrow(df.CVS), nrow(df.CVS)/2)
train.CVS = df.CVS[training,]
test.CVS = df.CVS[-training,]

# performing basic regression analysis

# linear regression, SEMI-GOOD CHOICE
lin.regr <- lm(SPENDS ~ PRIMARY_VISIT + DAYS_FROM_LAST_VISIT + TTL_QNTY + CPN_FLAG, data = train.CVS)
# the version below is for the variable importance function
#lin.regr <- train(SPENDS ~ PRIMARY_VISIT + DAYS_FROM_LAST_VISIT + TTL_QNTY + CPN_FLAG, data = train.CVS, method = "lm")
summary(lin.regr)
# diagnosis plots
# prediction test
lin.pred = predict(lin.regr, test.CVS)
mean((lin.pred - test.CVS$SPENDS)^2)
# residual plot
plot(lin.pred[-length(lin.pred)], lin.regr$residuals)

# polynomial fit, GOOD CHOICE?
poly.fit = lm(SPENDS ~ PRIMARY_VISIT + DAYS_FROM_LAST_VISIT + I(TTL_QNTY^2) + CPN_FLAG, data = train.CVS)
summary(poly.fit)
poly.pred = predict(poly.fit, test.CVS)
mean((poly.pred - test.CVS$SPENDS)^2)

# 1/x fit, BAD CHOICE?
inverse.fit = lm(SPENDS ~ PRIMARY_VISIT + DAYS_FROM_LAST_VISIT + I(1/TTL_QNTY) + CPN_FLAG, data = train.CVS)
summary(inverse.fit)
inverse.pred = predict(inverse.fit, test.CVS)
mean((inverse.pred - test.CVS$SPENDS)^2)
plot(inverse.pred[-length(inverse.pred)], inverse.fit$residuals)

# variable importance, for linear regression model
important.regr = varImp(lin.regr, scale = FALSE)
plot(important.regr)
