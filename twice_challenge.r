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

sales <- ddply(data[which(data$actionType == "Sale"), ], "userID", summarise, sales=sum(centAmount))

first.purchase <- ddply(data[!duplicated(data$userID) & data$actionType == "Purchase", ], "userID", summarise, 
  first.purchase=centAmount,
  date.first.purchase = as.date(as.character(date), order="ymd")
)

lifetimeData <- merge(purchases, sales, by = "userID")
lifetimeData <- merge(lifetimeData, first.purchase, by = "userID")

avg.x.number <- ggplot(lifetimeData, aes(num.purchases, avg.purchase)) + geom_point()

purchases.x.sales <- ggplot(lifetimeData, aes(sales, purchases)) + geom_point()
purchases.x.sales.log <- purchases.x.sales + scale_y_log10() + scale_x_log10()
salesmodel <- lm(log1p(purchases) ~ log1p(sales), data = lifetimeData)
summary(salesmodel)

purchases.x.first.purchase <- ggplot(lifetimeData, aes(first.purchase, purchases)) + geom_point()
purchases.x.first.purchase.log <- purchases.x.first.purchase + scale_y_log10() + scale_x_log10()

fpmodel <- lm(purchases ~ first.purchase, data = lifetimeData)
ltDataExOutliers <- lifetimeData[c(-110, -890, -1051, -67, -1352), ]
fpmodel.logged <- lm(log1p(purchases) ~ log1p(first.purchase), data = ltDataExOutliers)
summary(fpmodel.logged)

# separate customers into repeat purchasers and sellers
# 

# remember the date range!