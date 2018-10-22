# MA 543/DS 502
# CVS CASESTUDY PROJECT
# Team 4: Alex Shoop, Ming Min, Mohita Goplani, Yijiang (Chuck) Xu

# Importing the CVS raw dataset
rawDF <- read.csv(file="mainCVSdata.csv", header=TRUE, sep=",")

# Setting up the additional column of SPENDSNEXT
newDF <- rawDF
# removing last row since we will have no use for it
newDF <- newDF[-nrow(newDF),]
# getting the next visit's SPENDS for the customer
newDF$SPENDSNEXT <- with(newDF, rawDF$SPENDS[-1])
# loop to make NA the SPENDSNEXT where it's the customer's last visit
# this takes about 5 minutes to process
#for (i in 1:nrow(newDF)){
#  if (newDF$XTRA_CARD_NBR[i] != newDF$XTRA_CARD_NBR[i+1] || newDF$PRIMARY_VISIT[i] + 1 == newDF$PRIMARY_VISIT[i+1]){
#    newDF$SPENDSNEXT[i] <- NA
#  }
#}
for (i in 2:nrow(newDF)){
  # check if card number A != card number B. If yes, then it means we are at card A's last entry.
  if (newDF$XTRA_CARD_NBR[i] != newDF$XTRA_CARD_NBR[i+1]){
    newDF$SPENDSNEXT[i] <- NA
    next
  }
  # check if card B != card A. If yes, check card B's first visit.
  if (newDF$XTRA_CARD_NBR[i] != newDF$XTRA_CARD_NBR[i-1]){
    # check if card B's first visit is not 1. If yes, then bad card B entry. Thus, NA.
    if (newDF$PRIMARY_VISIT[i] != 1){
      newDF$SPENDSNEXT[i] <- NA
    }
    next
  }
  # if there's an NA entry previously for SPENDSNEXT (ie, bad card entry), then make the rest NA
  if (is.na(newDF$SPENDSNEXT[i-1])){
    newDF$SPENDSNEXT[i] <- NA
    next
  } 
  # else then card B's entry is good (eg, they have a good first visit entry, and subsequent entries)
  else {
    newDF$SPENDSNEXT[i] <- rawDF$SPENDS[i+1]
    next
  }
}

# removing the rows that have an NA entry
analysisDF <- na.omit(newDF)
# output analysis data
write.csv(analysisDF, file = 'analysis_data.csv')
