# MA 543/DS 502
# CVS CASESTUDY PROJECT
# Team 4: Alex Shoop, Ming Min, Mohita Goplani, Yijiang (Chuck) Xu
#library('caret')
library('scatterplot3d')
library('corrplot')
library('lubridate')
set.seed(1)

# Importing the pre-setup dataset
df.CVS <- read.csv(file="analysis_data_ming.csv", header=TRUE, sep=",")
# removing unnecessary columns
newDFwithoutQNTY <- df.CVS[-c(1,2,3,6)]
newDFwithQNTY <- df.CVS[-c(1,2,3)]

# looking at basic plots, and observing any visual trend
col1 <- colorRampPalette(c("red", "grey", "blue"))
dfCVScor = cor(df.CVS)
corrplot(dfCVScor, method = "number", col = col1(100))
plot(df.CVS$DATE_VISIT, df.CVS$SPENDS)

# centered
#primvisit.c = scale(df.CVS$PRIMARY_VISIT, center=TRUE, scale=FALSE)
#daysfromlast.c = scale(df.CVS$DAYS_FROM_LAST_VISIT, center=TRUE, scale=FALSE)
#spendprev.c = scale(df.CVS$SPENDSPRV, center=TRUE, scale=FALSE)
#newvars.c = cbind(primvisit.c, daysfromlast.c, spendprev.c)
#newDF.CVS = cbind(df.CVS, newvars.c)
#names(newDF.CVS)[10:12] = c("primVisit.c", "daysFromLast.c", "spendPrev.c" )
#summary(newDF.CVS)


plot(df.CVS$TTL_QNTY, df.CVS$SPENDS, xlab = "TTL_QNTY", ylab = "SPENDS", main = "TTL_QNTY vs SPENDS")
plot(df.CVS$PRIMARY_VISIT, df.CVS$SPENDS, xlab = "PRIMARY_VISIT", ylab = "SPENDS", main = "PRIMARY_VISIT vs SPENDS")
boxplot(df.CVS$SPENDS ~ df.CVS$CPN_FLAG)
pairs(data.frame(df.CVS$SPENDS, df.CVS[3:5], df.CVS$CPN_FLAG), labels = c("SPENDS","PRIMARY_VISIT","DAYS_FROM_LAST_VISIT","TTL_QNTY","CPN_FLAG"), lower.panel = NULL)


# setting up training and test datasets
# random sampling
training = sample(nrow(df.CVS), nrow(df.CVS)/2)
#train.CVS = df.CVS[training,]
#test.CVS = df.CVS[-training,]
train.withoutQNTY = newDFwithoutQNTY[training,]
test.withoutQNTY = newDFwithoutQNTY[-training,]
train.withQNTY = newDFwithQNTY[training,]
test.withQNTY = newDFwithQNTY[-training,]

# performing basic regression analysis

# linear regression, SEMI-GOOD CHOICE
lin.regr.noQNTY <- lm(SPENDS ~ ., data = train.withoutQNTY)
lin.regr.QNTY <- lm(SPENDS ~., data = train.withQNTY)
# the version below is for the variable importance function
#lin.regr <- train(SPENDS ~ PRIMARY_VISIT + DAYS_FROM_LAST_VISIT + CPN_FLAG + SPENDSPRV + QNTYPRV, data = train.CVS, method = "lm")
summary(lin.regr.noQNTY)
summary(lin.regr.QNTY)
# diagnosis plots
# prediction test
lin.pred.noQNTY = predict(lin.regr.noQNTY, test.withoutQNTY)
lin.pred.QNTY = predict(lin.regr.QNTY, test.withQNTY)
mean((lin.pred.noQNTY - test.withoutQNTY$SPENDS)^2)
mean((lin.pred.QNTY - test.withQNTY$SPENDS)^2)

# residual plot
plot(lin.pred[-length(lin.pred)], lin.regr$residuals)


# polynomial fit, BAD CHOICE
poly.fit = lm(SPENDS ~ PRIMARY_VISIT + DAYS_FROM_LAST_VISIT + CPN_FLAG + SPENDSPRV*QNTYPRV, data = train.CVS)
summary(poly.fit)
poly.pred = predict(poly.fit, test.CVS)
mean((poly.pred - test.CVS$SPENDS)^2)


# log response fit, GOOD CHOICE
logresp.fit.noQNTY <- lm(log(SPENDS) ~ ., data = train.withoutQNTY)
logresp.fit.QNTY <- lm(log(SPENDS) ~ ., data = train.withQNTY)
logresp.fit.invQNTY <- lm(log(SPENDS) ~ PRIMARY_VISIT + DAYS_FROM_LAST_VISIT + I(1/TTL_QNTY) + CPN_FLAG + SPENDSPRV, data = train.withQNTY)
summary(logresp.fit.noQNTY)
summary(logresp.fit.QNTY)
summary(logresp.fit.invQNTY)

logresp.pred.noQNTY <- predict(logresp.fit.noQNTY, test.withoutQNTY)
logresp.pred.QNTY <- predict(logresp.fit.QNTY, test.withQNTY)
logresp.pred.invQNTY <- predict(logresp.fit.invQNTY, test.withQNTY)

mean((logresp.pred.noQNTY - log(test.withoutQNTY$SPENDS))^2)
mean((logresp.pred.QNTY - log(test.withQNTY$SPENDS))^2)
mean((logresp.pred.invQNTY - log(test.withQNTY$SPENDS))^2)

# residual plots
plot(lin.regr.noQNTY, which = 1, main = "MSLR, no QNTY")
plot(lin.regr.QNTY, which = 1, main = "MSLR, w/ QNTY")
plot(logresp.fit.noQNTY, which = 1, main = "MSLR, where response = log(SPENDS), no QNTY")
plot(logresp.fit.QNTY, which = 1, main = "MSLR, where response = log(SPENDS), w/ QNTY")
plot(logresp.fit.invQNTY, which = 1, main = "MSLR, where response = log(SPENDS), w/ (1/QNTY)")

randomSampling = sample(nrow(test.withoutQNTY),10)
testSPENDS = test.withoutQNTY$SPENDS[randomSampling]
#testPRIMVISIT = test.withoutQNTY$PRIMARY_VISIT[randomSampling]
#testDAYSLASTVISIT = test.withoutQNTY$DAYS_FROM_LAST_VISIT[randomSampling]
#testCPN = test.withoutQNTY$CPN_FLAG[randomSampling]
#testPREVSPENDS = test.withoutQNTY$SPENDSPRV[randomSampling]
logresp.noQNTY.coefs = coef(logresp.fit.noQNTY)

logresp.pred.testing = predict(logresp.fit.noQNTY, test.withoutQNTY[randomSampling,])

inverse.pred = predict(inverse.fit, test.CVS)
mean((inverse.pred - test.CVS$SPENDS)^2)
plot(inverse.pred[-length(inverse.pred)], inverse.fit$residuals)
plot(inverse.fit, pch = 16, which = 1)
# variable importance, for linear regression model
important.regr = varImp(lin.regr, scale = FALSE)
plot(important.regr)
