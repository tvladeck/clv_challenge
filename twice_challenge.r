setwd("~/Git/Twice")
install.packages("plyr")
install.packages("ggplot2")
install.packages("reshape")
data <- data.frame(read.table("twice_transactions.txt"))
colnames(data) <- c("userID", "actionType", "date", "centAmount")

uniqueIDs <- unique(data["userID"])

lifetimeSales <- data.frame(userId=uniqueIDs, purchases="", sales="")

for (i in uniqueIDs[,1]) {
  
  df <- data[which(data$userID == i), ]
  purchases <- sum(df[which(df$actionType=="Purchase"),]$centAmount)
  sales <- sum(df[which(df$actionType=="Sale"),]$centAmount)
  
  lifetimeSales <- rbind(lifetimeSales, c(i, purchases, sales))
  
}

# remember the date range!