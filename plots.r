### PLOTS

purchases.x.first.purchase <- ggplot(lifetimeData, aes(first.purchase, purchases)) +
  geom_point(alpha=1/4) + scale_y_log10() + scale_x_log10()

repeat.purchaser.plot <- ggplot(lifetimeData, aes(is.repeat.purchaser, purchases)) + 
  geom_boxplot() + scale_y_log10(breaks=c(1000, 10000, 100000), labels=c("$10","$100","$1000")) 

churn.plot <- ggplot(purchases.only[purchases.only$inv.days.between.txs > 0, ], 
                     aes(date, inv.days.between.txs)) + 
  geom_point(alpha=3/10) + scale_y_log10(breaks=c(0.01,0.1,1),
                                         labels=c("100 days", "10 days", "1 day"),
                                         name="time between sales") +
  theme(text = element_text(size=20))

repeat.purchasing.plot <- ggplot(purchases.only, aes(days.between.txs)) +
  geom_histogram() + geom_density()