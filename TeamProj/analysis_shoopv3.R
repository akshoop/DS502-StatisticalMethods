# MA 543/DS 502
# CVS CASESTUDY PROJECT
# Team 4: Alex Shoop, Ming Min, Mohita Goplani, Yijiang (Chuck) Xu
library('caret')
library('scatterplot3d')
set.seed(1)

# Importing the pre-setup dataset (which has SPENDSNEXT col, and no NA entries)
df.CVS <- read.csv(file="analysis_data_shoop.csv", header=TRUE, sep=",")
# removing unnecessary 1st column
df.CVS <- df.CVS[,-1]
# centered
primvisit.c = scale(df.CVS$PRIMARY_VISIT, center=TRUE, scale=FALSE)
daysfromlast.c = scale(df.CVS$DAYS_FROM_LAST_VISIT, center=TRUE, scale=FALSE)
spendprev.c = scale(df.CVS$SPENDSPRV, center=TRUE, scale=FALSE)
newvars.c = cbind(primvisit.c, daysfromlast.c, spendprev.c)
newDF.CVS = cbind(df.CVS, newvars.c)
names(newDF.CVS)[10:12] = c("primVisit.c", "daysFromLast.c", "spendPrev.c" )
summary(newDF.CVS)
attach(newDF.CVS)

# looking at basic plots, and observing any visual trend
# needs more plots and visuals
plot(df.CVS$TTL_QNTY, df.CVS$SPENDS, xlab = "TTL_QNTY", ylab = "SPENDS", main = "TTL_QNTY vs SPENDS")
plot(df.CVS$PRIMARY_VISIT, df.CVS$SPENDS, xlab = "PRIMARY_VISIT", ylab = "SPENDS", main = "PRIMARY_VISIT vs SPENDS")
boxplot(df.CVS$SPENDS ~ df.CVS$CPN_FLAG)
pairs(data.frame(df.CVS$SPENDS, df.CVS[3:5], df.CVS$CPN_FLAG), labels = c("SPENDS","PRIMARY_VISIT","DAYS_FROM_LAST_VISIT","TTL_QNTY","CPN_FLAG"), lower.panel = NULL)

# setting up training and test datasets
# random sampling
training = sample(nrow(newDF.CVS), nrow(newDF.CVS)/2)
train.CVS = newDF.CVS[training,]
test.CVS = newDF.CVS[-training,]

# performing basic regression analysis

# linear regression, SEMI-GOOD CHOICE
lin.regr <- lm(SPENDS ~ primVisit.c + daysFromLast.c + CPN_FLAG + spendPrev.c, data = train.CVS)
# the version below is for the variable importance function
#lin.regr <- train(SPENDS ~ PRIMARY_VISIT + DAYS_FROM_LAST_VISIT + CPN_FLAG + SPENDSPRV + QNTYPRV, data = train.CVS, method = "lm")
summary(lin.regr)
# diagnosis plots
# prediction test
lin.pred = predict(lin.regr, test.CVS)
mean((lin.pred - test.CVS$SPENDS)^2)
# residual plot
plot(lin.pred[-length(lin.pred)], lin.regr$residuals)

# polynomial fit, BAD CHOICE
poly.fit = lm(SPENDS ~ PRIMARY_VISIT + DAYS_FROM_LAST_VISIT + CPN_FLAG + SPENDSPRV*QNTYPRV, data = train.CVS)
summary(poly.fit)
poly.pred = predict(poly.fit, test.CVS)
mean((poly.pred - test.CVS$SPENDS)^2)

# 1/x predictor and log response fit, GOOD CHOICE
inverse.fit = lm(log(SPENDS) ~ primVisit.c + daysFromLast.c + CPN_FLAG + spendPrev.c, data = train.CVS)
summary(inverse.fit)
inverse.pred = predict(inverse.fit, test.CVS)
mean((inverse.pred - test.CVS$SPENDS)^2)
plot(inverse.pred[-length(inverse.pred)], inverse.fit$residuals)
plot(inverse.fit, pch = 16, which = 1)
# variable importance, for linear regression model
important.regr = varImp(lin.regr, scale = FALSE)
plot(important.regr)
