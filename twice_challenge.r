# setwd("~/Git/Twice")

# install these packages if they are not already
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("date")

# loads the required packages 
library("plyr")
library("ggplot2")
library("date")

# imports the raw data
# the raw file is not tracked in the git repo
data <- data.frame(read.table("twice_transactions.txt"))
colnames(data) <- c("userID", "actionType", "date", "centAmount")

# normalizing dates to integers for help later on
# and set the start date to zero for help with regression below
data$date <- as.integer(as.date(as.character(data$date), order = "ymd"))
data$date <- data$date - min(data$date)

# converting the data to a table with each row as a summary of
# a particular user's total purchasing behavior
purchases <- ddply(data[which(data$actionType == "Purchase"), ], "userID", summarise,
  purchases = sum(centAmount),
  avg.purchase = mean(centAmount),
  num.purchases = sum(centAmount) /  mean(centAmount)
)

# similar for sales
sales <- ddply(data[which(data$actionType == "Sale"), ], "userID", summarise, 
  sales = sum(centAmount),
  avg.sale = mean(centAmount),
  num.sales = sum(centAmount) / mean(centAmount)
)

# finding the date and amount of the first purchase for each user
purchases.data <- data[data$actionType == "Purchase", ]
first.purchase  <- ddply(purchases.data[!duplicated(purchases.data$userID), ], "userID", summarise, 
  first.purchase=centAmount,
  date.first.purchase = date
)

# finding the date and amount of the first sale for each user
sales.data <- data[data$actionType == "Sale", ]
first.sale <- ddply(sales.data[!duplicated(sales.data$userID), ], "userID", summarise, 
  first.sale=centAmount,
  date.first.sale = date
)

# these lines are simply merging the tables produced above in an
# outer join on userID
lifetimeData <- merge(purchases, sales, by = "userID", all = TRUE)
lifetimeData <- merge(lifetimeData, first.purchase, by = "userID", all = TRUE)
lifetimeData <- merge(lifetimeData, first.sale, by = "userID", all = TRUE)

# adding columns for helpful booleans for each user
lifetimeData$both.actions <- !is.na(lifetimeData$purchases) & !is.na(lifetimeData$sales)
lifetimeData$is.repeat.purchaser <- lifetimeData$num.purchases > 1 & !is.na(lifetimeData$purchases)
lifetimeData$is.repeat.seller <- lifetimeData$num.sales > 1 & !is.na(lifetimeData$sales)


# splitting the dataset in two for use in regressions below
purchases.data <- lifetimeData[!is.na(lifetimeData$purchases) & !is.na(lifetimeData$first.purchase) & 
                                 lifetimeData$first.purchase > 0, ]

sales.data <- lifetimeData[!is.na(lifetimeData$sales) & !is.na(lifetimeData$first.sale) & 
                             lifetimeData$first.sale > 0, ]


### MODELS

## purchases
# this is a multiple regression to estimate the payout over a 175 day period to a given purchaser
full.purchases.model <- lm(log1p(purchases) ~ date.first.purchase + log1p(first.purchase) + 
  both.actions + is.repeat.purchaser + date.first.purchase*is.repeat.purchaser, 
  data = purchases.data
)

# helpful statistics to estimate the proceeds to a representative user
percent.repeat <- sum(purchases.data$is.repeat.purchaser)/length(purchases.data$is.repeat.purchaser)
percent.both <- sum(purchases.data$both.actions)/length(purchases.data$both.actions)
avg.first.purchase <- mean(purchases.data$first.purchase)

# extracting the coefficients from the linear model above
full.intercept <- coefficients(full.purchases.model)[1]
full.first.purchase <- coefficients(full.purchases.model)[3]
full.both.actions <- coefficients(full.purchases.model)[4]
full.is.repeat <- coefficients(full.purchases.model)[5]

# putting it all together to get an estimate for 175-day proceeds
# note we omit the date term to represent a user purchasing today
log.estimate <- full.intercept + full.first.purchase * log1p(avg.first.purchase) +
  percent.repeat * full.is.repeat + percent.both * full.both.actions
estimate <- exp(log.estimate)

# extrapolating behavior forward to get an estimate of lifetime value
# we use the cohort of the first 10 days to estimate the 175-day churn rate
cohort.10d <- purchases.data[purchases.data$date.first.purchase<10,  ]

# we then have to adjust up the value by the unobserved transactions
churn.rate.175 <- (sum(purchases.data$num.purchases>1)*(1 + 0.5^(175/half.life))) / sum(purchases.data$num.purchases>0)
multiplier <- sum(churn.rate.175^c(0,1,2,3))
two.year.ltv <- multiplier * estimate

## sales

# because so few sellers are not also buyers, the "both.actions" dummy is not linearly independent
# so it is not included in the model below

# the code below follows exactly the same pattern for as the purchases model above

# regression model
full.sales.model <- lm(log1p(sales) ~ date.first.sale + log1p(first.sale) + 
  is.repeat.seller + is.repeat.seller*date.first.sale, 
  data = sales.data
)

# statistics for an average seller
percent.repeat.seller <- sum(purchases.data$is.repeat.purchaser)/length(purchases.data$is.repeat.purchaser)
avg.first.sale <- mean(purchases.data$first.sale[!is.na(purchases.data$first.sale)])

# extracting model coefficients
sm.intercept <- coefficients(full.sales.model)[1]
sm.first.sale <- coefficients(full.sales.model)[3]
sm.repeat <-  coefficients(full.sales.model)[4]

# estimate for a 175 day period
log.sm.estimate <- sm.intercept + sm.first.sale * log1p(avg.first.sale) + 
  sm.repeat * percent.repeat.seller
sm.estimate <- exp(log.sm.estimate)

# extrapolating forward to a two year period
sm.churn.rate.175 <- percent.repeat.seller
sm.multiplier <- sum(sm.churn.rate.175 ^ c(0,1,2,3))
two.year.ltp <- sm.estimate * sm.multiplier



