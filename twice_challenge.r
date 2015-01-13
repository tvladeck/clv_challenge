# setwd("~/Git/Twice")
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("reshape")
library("plyr")
library("ggplot2")
library("reshape")
data <- data.frame(read.table("twice_transactions.txt"))
colnames(data) <- c("userID", "actionType", "date", "centAmount")

purchases <- ddply(data[which(data$actionType == "Purchase"), ], "userID", summarise, purchases=sum(centAmount))
sales <- ddply(data[which(data$actionType == "Sale"), ], "userID", summarise, sales=sum(centAmount))
first.purchase <- ddply(data[!duplicated(data$userID) & data$actionType == "Purchase",], "userID", summarise, first.purchase=centAmount)

lifetimeData <- merge(purchases, sales, by = "userID")
lifetimeData <- merge(lifetimeData, first.purchase, by = "userID")

purchases.x.sales <- ggplot(lifetimeData, aes(purchases, sales)) + geom_point()
purchases.x.sales.log <- purchases.x.sales + scale_y_log10() + scale_x_log10()

purchases.x.first.purchase <- ggplot(lifetimeData, aes(first.purchase, purchases)) + geom_point()
purchases.x.first.purchase.log <- purchases.x.first.purchase + scale_y_log10() + scale_x_log10()

fpmodel <- lm(purchases ~ first.purchase, data = lifetimeData)
ltDataExOutliers <- lifetimeData[c(-110, -890, -1051, -67, -1352), ]
fpmodel.logged <- lm(log1p(purchases) ~ log1p(first.purchase), data = ltDataExOutliers)
summary(fpmodel.logged)



# remember the date range!