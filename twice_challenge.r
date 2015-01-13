# setwd("~/Git/Twice")
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("reshape")
data <- data.frame(read.table("twice_transactions.txt"))
colnames(data) <- c("userID", "actionType", "date", "centAmount")

purchases <- ddply(data[which(data$actionType == "Purchase"), ], "userID", summarise, purchases=sum(centAmount))
sales <- ddply(data[which(data$actionType == "Sale"), ], "userID", summarise, sales=sum(centAmount))

lifetimeData <- merge(purchases, sales, by = "userID")





# remember the date range!