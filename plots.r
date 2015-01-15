### PLOTS

# these are ggplots and the below is just constructing various plots I found interesting

num.purchases.x.avg.purchase <- ggplot(lifetimeData, aes(num.purchases, avg.purchase, 
                                                         fill=both.actions)) + 
  geom_point(alpha=1/2) + scale_y_continuous(breaks=c(0,25000,50000,75000), 
                                             labels=c("$0", "$25", "$50", "$75"),
                                             name="Average purchase size") +
  scale_x_continuous(name="# of purchases") + theme(text = element_text(size=20))

purchases.x.first.purchase <- ggplot(lifetimeData, aes(first.purchase, purchases)) +
  geom_point(alpha=1/4) + scale_y_log10(breaks=c(1000,10000,100000,1000000),
                                        labels=c("$10", "$100", "$1000", "$10000"),
                                        name="total purchases") + 
  scale_x_log10(breaks=c(1000,10000,100000), labels=c("$10","$100","$1000"), 
                name="first purchase amount") + theme(text=element_text(size=20))

repeat.purchaser.plot <- ggplot(lifetimeData, aes(is.repeat.purchaser, purchases)) + 
  geom_boxplot() + scale_y_log10(breaks=c(1000, 10000, 100000), 
                                 labels=c("$10","$100","$1000"),
                                 name=c("Total observed purchases")) +
  scale_x_discrete(breaks=c(FALSE, TRUE), 
                   labels=c("One-time buyers", "Repeat buyers"), 
                   name="") +
  theme(text=element_text(size=20))

churn.plot <- ggplot(purchases.only[purchases.only$inv.days.between.txs > 0, ], 
                     aes(date, inv.days.between.txs)) + 
  geom_point(alpha=1/4) + scale_y_log10(breaks=c(0.01,0.1,1),
                                         labels=c("100 days", "10 days", "1 day"),
                                         name="time between sales") +
  theme(text = element_text(size=20))

repeat.purchasing.plot <- ggplot(purchases.only, aes(days.between.txs)) +
  geom_histogram() + geom_density()