### PLOTS

purchases.x.first.purchase <- ggplot(lifetimeData, aes(first.purchase, purchases)) +
  geom_point(alpha=1/4) + scale_y_log10() + scale_x_log10()

repeat.purchaser.plot <- ggplot(lifetimeData, aes(is.repeat.purchaser, purchases)) + 
  geom_boxplot() + scale_y_log10(breaks=c(1000, 10000, 100000), labels=c("$10","$100","$1000")) 

