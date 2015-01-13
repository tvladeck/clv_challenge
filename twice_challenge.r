# setwd("~/Git/Twice")
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("reshape")
# install.packages("date")
library("plyr")
library("ggplot2")
library("reshape")
library("date")

data <- data.frame(read.table("twice_transactions.txt"))
colnames(data) <- c("userID", "actionType", "date", "centAmount")

purchases <- ddply(data[which(data$actionType == "Purchase"), ], "userID", summarise,
  purchases = sum(centAmount),
  avg.purchase = mean(centAmount),
  num.purchases = sum(centAmount) /  mean(centAmount)
)

sales <- ddply(data[which(data$actionType == "Sale"), ], "userID", summarise, 
  sales = sum(centAmount),
  avg.sale = mean(centAmount),
  num.sales = sum(centAmount) / mean(centAmount)
)

first.purchase <- ddply(data[!duplicated(data$userID) & data$actionType == "Purchase", ], "userID", summarise, 
  first.purchase=centAmount,
  date.first.purchase = as.date(as.character(date), order="ymd")
)

first.sale <- ddply(data[!duplicated(data$userID) & data$actionType == "Sale", ], "userID", summarise, 
  first.sale=centAmount,
  date.first.sale = as.date(as.character(date), order="ymd")
)

lifetimeData <- merge(purchases, sales, by = "userID", all = TRUE)
lifetimeData <- merge(lifetimeData, first.purchase, by = "userID", all = TRUE)
lifetimeData <- merge(lifetimeData, first.sale, by = "userID", all = TRUE)
lifetimeData$both.actions <- !is.na(lifetimeData$purchases) & !is.na(lifetimeData$sales)
lifetimeData$is.repeat.purchaser <- lifetimeData$num.purchases > 1 & !is.na(lifetimeData$purchases)


### MODELS

fpmodel <- lm(purchases ~ first.purchase, data = lifetimeData)

ltDataExOutliers <- lifetimeData[c(-110, -890, -1051, -67, -1352), ]
fpmodel.logged <- lm(log1p(purchases) ~ log1p(first.purchase), data = ltDataExOutliers)

salesmodel <- lm(log1p(purchases) ~ log1p(sales), data = lifetimeData)

num.purchases.model <- lm(num.purchases ~ date.first.purchase, data=lifetimeData)

kitchen.sink <- lm(log1p(purchases) ~ date.first.purchase + both.actions + log1p(first.purchase) + is.repeat.purchaser, data = lifetimeData)



### PLOTS
avg.x.number <- ggplot(lifetimeData, aes(num.purchases, avg.purchase)) + geom_point()

purchases.x.first.purchase <- ggplot(lifetimeData, aes(first.purchase, purchases)) + geom_point()

purchases.x.first.purchase.log <- purchases.x.first.purchase + scale_y_log10() + scale_x_log10()

both.actions.plot <- ggplot(lifetimeData, aes(both.actions, purchases)) + geom_point() + scale_y_log10()
num.purchases.plot <- ggplot(lifetimeData, aes(date.first.purchase, num.purchases)) + geom_point()
repeat.purchaser.plot <- ggplot(lifetimeData, aes(purchases, is.repeat.purchaser)) + geom_point() + scale_x_log10()

purchases.x.sales <- ggplot(lifetimeData, aes(sales, purchases)) + geom_point()
purchases.x.sales.log <- purchases.x.sales + scale_y_log10() + scale_x_log10()

