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

# normalizing dates to integers for help later on
data$date <- as.integer(as.date(as.character(data$date), order = "ymd"))
data$date <- data$date - min(data$date)

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

purchases.data <- data[data$actionType == "Purchase", ]
first.purchase  <- ddply(purchases.data[!duplicated(purchases.data$userID), ], "userID", summarise, 
  first.purchase=centAmount,
  date.first.purchase = date
)

sales.data <- data[data$actionType == "Sale", ]
first.sale <- ddply(sales.data[!duplicated(sales.data$userID), ], "userID", summarise, 
  first.sale=centAmount,
  date.first.sale = date
)

lifetimeData <- merge(purchases, sales, by = "userID", all = TRUE)
lifetimeData <- merge(lifetimeData, first.purchase, by = "userID", all = TRUE)
lifetimeData <- merge(lifetimeData, first.sale, by = "userID", all = TRUE)
lifetimeData$both.actions <- !is.na(lifetimeData$purchases) & !is.na(lifetimeData$sales)
lifetimeData$is.repeat.purchaser <- lifetimeData$num.purchases > 1 & !is.na(lifetimeData$purchases)
lifetimeData$is.repeat.seller <- lifetimeData$num.sales > 1 & !is.na(lifetimeData$sales)


purchases.data <- lifetimeData[!is.na(lifetimeData$purchases) & !is.na(lifetimeData$first.purchase) & 
                                 lifetimeData$first.purchase > 0, ]

sales.data <- lifetimeData[!is.na(lifetimeData$sales) & !is.na(lifetimeData$first.sale) & 
                             lifetimeData$first.sale > 0, ]


### MODELS

## purchases

full.purchases.model <- lm(log1p(purchases) ~ log1p(date.first.purchase) + log1p(first.purchase) + 
  both.actions + is.repeat.purchaser + log1p(date.first.purchase)*is.repeat.purchaser, 
  data = purchases.data
)

## sales

# because so few sellers are not also buyers, the "both.actions" dummy is not linearly independent
full.sales.model <- lm(log1p(sales) ~ log1p(date.first.sale) + log1p(first.sale) + 
  is.repeat.seller + is.repeat.purchaser*log1p(date.first.sale), 
  data = sales.data
)

### PLOTS
avg.x.number <- ggplot(lifetimeData, aes(num.purchases, avg.purchase)) + geom_point()

purchases.x.first.purchase <- ggplot(lifetimeData, aes(first.purchase, purchases)) + geom_point()

purchases.x.first.purchase.log <- purchases.x.first.purchase + scale_y_log10() + scale_x_log10()

both.actions.plot <- ggplot(lifetimeData, aes(both.actions, purchases)) + geom_point() + scale_y_log10()
num.purchases.plot <- ggplot(lifetimeData, aes(date.first.purchase, num.purchases)) + geom_point()
repeat.purchaser.plot <- ggplot(lifetimeData, aes(purchases, is.repeat.purchaser)) + geom_point() + scale_x_log10()

purchases.x.sales <- ggplot(lifetimeData, aes(sales, purchases)) + geom_point()
purchases.x.sales.log <- purchases.x.sales + scale_y_log10() + scale_x_log10()

