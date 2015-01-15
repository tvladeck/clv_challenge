# this worksheet has some functions to help calculate churn

# here, I code each purchase with the date between it and the 
# user's next purchase
# WARNING - runs very slowly
purchases.only <- data[which(data$actionType == "Purchase"), ]
index <- length(purchases.only[, 1])
for (j in 1:index) {
  remaining.rows <- purchases.only[j+1:index, ]
  purchase.date <- purchases.only$date[j]
  userID <- purchases.only$userID[j]
  next.tx.date <- remaining.rows[which(remaining.rows$userID == userID), ]$date[1]
  days.between.txs <- next.tx.date - purchase.date
  purchases.only$days.between.txs[j] <- days.between.txs
}

# a useful column and data munging
purchases.only$inv.days.between.txs <- 1/purchases.only$days.between.txs
purchases.only$inv.days.between.txs[is.na(purchases.only$inv.days.between.txs)] <- 0
purchases.only$inv.days.between.txs[is.infinite(purchases.only$inv.days.between.txs)] <- 1

# here, we estimate the decay function for users next purchases
# we use exponential decay in the number of next txs as a fn of time
decay.cohort.10d <- purchases.only[purchases.only$date <10, ]
decay <- ddply(decay.cohort.10d, "days.between.txs", summarise, count=length(days.between.txs))
decay.model <- lm(log(count) ~ days.between.txs, data=decay)
half.life <- -log(2)/coefficients(decay.model)[2]

