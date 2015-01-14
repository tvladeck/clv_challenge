# churn visualization


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

purchases.only$inv.days.between.txs <- 1/purchases.only$days.between.txs
purchases.only$inv.days.between.txs[is.na(purchases.only$inv.days.between.txs)] <- 0
purchases.only$inv.days.between.txs[is.infinite(purchases.only$inv.days.between.txs)] <- 0


churn.plot <- ggplot(purchases.only[purchases.only$inv.days.between.txs > 0, ], 
                     aes(date, inv.days.between.txs)) + 
  geom_point(alpha=3/10) + scale_y_log10(breaks=c(0.01,0.1,1),
                                         labels=c("100 days", "10 days", "1 day"),
                                         name="time between sales")