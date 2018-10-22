# MA 543/DS 502
# CVS CASESTUDY PROJECT
# Team 4: Alex Shoop, Ming Min, Mohita Goplani, Yijiang (Chuck) Xu

# Importing the CVS raw dataset
rawDF <- read.csv(file="mainCVSdata.csv", header=TRUE, sep=",")

# Setting up the additional column of SPENDSNEXT
newDF <- rawDF
# removing first row since we will have no use for it
newDF <- newDF[-1,]
# getting the previous visit's SPENDS for the customer
newDF$SPENDSPRV <- rawDF$SPENDS[-nrow(newDF)]

# loop to make NA the SPENDSNEXT where it's the customer's last visit
# this takes about 5 minutes to process
#for (i in 1:nrow(newDF)){
#  if (newDF$XTRA_CARD_NBR[i] != newDF$XTRA_CARD_NBR[i+1] || newDF$PRIMARY_VISIT[i] + 1 == newDF$PRIMARY_VISIT[i+1]){
#    newDF$SPENDSNEXT[i] <- NA
#  }
#}
for (i in 2:nrow(newDF)){
  # check if card number A != card number B. If yes, then it means we are at card A's last entry.
  if (newDF$XTRA_CARD_NBR[i-1] != newDF$XTRA_CARD_NBR[i]){
    newDF$SPENDSPRV[i] <- NA
    
  }
  # check if card B != card A. If yes, check card B's first visit.
  if (newDF$XTRA_CARD_NBR[i] != newDF$XTRA_CARD_NBR[i-1]){
    # check if card B's first visit is not 1. If yes, then bad card B entry. Thus, NA.
    if (newDF$PRIMARY_VISIT[i] != 1){
      newDF$PRIMARY_VISIT[i] <- NA
    }
    
  }
  # if there's an NA entry previously for SPENDSNEXT (ie, bad card entry), then make the rest NA
  else
    if (is.na(newDF$PRIMARY_VISIT[i-1])){
    newDF$PRIMARY_VISIT[i] <- NA
    }
  # else then card B's entry is good (eg, they have a good first visit entry, and subsequent entries)
    
}

# removing the rows that have an NA entry
analysisDF <- na.omit(newDF)
# output analysis data
write.csv(analysisDF, file = 'analysis_data.csv')

