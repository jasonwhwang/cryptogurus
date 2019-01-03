library(dplyr)
library(reshape)

# # closingPricesMain <- read.csv("closingPrices.csv", stringsAsFactors = F)
# closingPrices <- closingPricesMain
# 
# coinsToKeep <- closingPrices %>% group_by(currentSymbol) %>% summarise(rowCount = n())
# coinsToKeep <- subset(coinsToRemove, rowCount < 283)$currentSymbol
# 
# closingPrices <- subset(closingPrices, currentSymbol %in% coinsToKeep)
# 
# write.csv(closingPrices, "closingPricesSub.csv", row.names = F)

closingPrices <- read.csv("closingPricesSub.csv", stringsAsFactors = F)

closingPrices$id <- paste(closingPrices$currentSymbol, closingPrices$date, sep = ":")
closingPrices <- closingPrices[!duplicated(closingPrices$id),]

signals <- read.csv("signalsBySymbol.csv", stringsAsFactors = F)

#########################################################################################################
signals$timeStamp <- as.POSIXct(signals$timeStamp, "%Y-%m-%d %H:%M:%S", tz = "GMT")

allTimes <- sort(signals$timeStamp)

minDate <- as.Date(head(allTimes,1) - 24*60*60)
maxDate <- as.Date(tail(allTimes,1) + 24*60*60)
allDates <- seq(minDate, maxDate, by="days")
#########################################################################################################
signals$quanityChange <- ifelse(signals$signal == 1, 1000/signals$priceAtTime, -1*(1000/signals$priceAtTime))
signals$date <- as.Date(signals$timeStamp)
signalsByDay <- signals %>% group_by(user, date, symbol) %>% summarise(dailyHoldingsChange = sum(quanityChange))

coinsWithAvailablePrices <- unique(closingPrices$currentSymbol)
signalsByDay <- subset(signalsByDay, symbol %in% coinsWithAvailablePrices)

################################# for loop
allDailyNetWorthByCoins <- data.frame()

allUsers <- unique(signalsByDay$user)
for (userIndex in 1:length(unique(signalsByDay$user))){

# selectedUserDailyNetWorthByCoins <- data.frame()
# userIndex = 1
  
  print(paste("index", userIndex))
  
selectedUser <- allUsers[userIndex]
account <- as.data.frame(subset(signalsByDay, user == selectedUser))
account$user <- NULL

accountWide <- reshape(account, idvar = "date", timevar = "symbol", direction = "wide")
accountWide <- accountWide[!is.na(accountWide$date),]

colnames(accountWide) <- gsub("dailyHoldingsChange.", "", colnames(accountWide))

simulation <- data_frame(date = allDates)
simulation <- merge(simulation, accountWide, by="date", all = T)

simulation[is.na(simulation)] <- 0

#get number of non nulls per row (minus 1 for date)
# simulation$cashSpent <- apply(simulation, 1, function(x) sum(!(is.na(x))) -1 ) * 1000

rownames(simulation) <- simulation$date
simulation$date <- NULL
###########################################################################################################
#get cash value

cashSpent <- apply(simulation, 1, function(x) {sum(x > 0, na.rm = T)})
cashRec <- apply(simulation, 1, function(x) {sum(x < 0, na.rm = T)})
cashHoldingsChange <- cashRec - cashSpent
simulation$cashHoldingsChange <- cashHoldingsChange * 1000

simulation$cashHoldings <- NA
cashHoldingsCol <- which(colnames(simulation) == "cashHoldings")
cashHoldingsChangeCol <- which(colnames(simulation) == "cashHoldingsChange")

simulation[1,cashHoldingsCol] <- 200000

for (i in 2:nrow(simulation)){
  previousCashHoldings <- simulation[i-1,cashHoldingsCol]
  currentCashHoldingsChange <- simulation[i,cashHoldingsChangeCol]
  currentCashHoldings <- (previousCashHoldings + currentCashHoldingsChange)
  simulation[i,cashHoldingsCol] <- currentCashHoldings
}

cashHoldings <- simulation$cashHoldings
simulation$cashHoldings <- simulation$cashHoldingsChange <- NULL


##########################################################################################################
#get coins value
#translate the buy/sell vol change of eash asset to holdings
for (j in 1:ncol(simulation)){
  for (i in 2:nrow(simulation)){
    previousHolding <- simulation[i-1,j]
    currentChange <- simulation[i,j]
    
    currentHolding <- previousHolding + currentChange
    simulation[i,j] <-currentHolding
  }
}

#translate the vol of holdings to daily net worth
simulation$dailyCoinNetWorth <- NA
dailyCoinNetWorthCol <- which(colnames(simulation) == "dailyCoinNetWorth")

for (i in 1:nrow(simulation)){
  # for (i in 58:64){
  print(i)
  currentDate <- rownames(simulation)[i]
  dailyHoldings <- 0
  for (j in 1:(ncol(simulation) -1)){
    symbol <- colnames(simulation)[j]
    # print(paste("col", j))
    
    coinHoldingQty <- simulation[i,j]
    coinHoldingValue <- 0
    
    # printTest <- paste(currentDate, symbol, selectedUser)
    # print(printTest)
    
    if ((coinHoldingQty != 0) == T){
      
      # printTest <- paste(currentDate, symbol, selectedUser)
      # print(printTest)
      
      
      currentSymbolPrice <- subset(closingPrices, date == currentDate & currentSymbol == symbol)$close
      
      if(length(currentSymbolPrice) == 0){ currentSymbolPrice = 0}
      
      currentSymbolHoldingWorth <- currentSymbolPrice * coinHoldingQty
      
      if ((coinHoldingQty > 0) == T){ #for long position
        dailyHoldings <- dailyHoldings + currentSymbolHoldingWorth
        coinHoldingValue <- currentSymbolHoldingWorth
        # print(dailyHoldings)
        
      } else if ((coinHoldingQty < 0) == T){ #for short position
        # print("evaluate short")
        for (k in 1:nrow(simulation)){ #find the date the short position started
          currentRowUpwards <- i - k
          currentHoldingQtyShort <- simulation[currentRowUpwards,j]
          if (currentHoldingQtyShort >= 0){ #the day before the short position acquired
            indexOfShortAcquiredDay <- currentRowUpwards +1
            break
          }
        }
        dateShortPositionInitiated <- rownames(simulation)[indexOfShortAcquiredDay]
        priceShortPositionAcquired <- subset(closingPrices, date == dateShortPositionInitiated & currentSymbol == symbol)$close
        valueOfShortPosition <- (priceShortPositionAcquired - currentSymbolPrice) * abs(coinHoldingQty)
        
        print(paste("a",dateShortPositionInitiated))
        print(paste("b",currentDate))
        print(paste("c",coinHoldingQty))
        print(paste("d",valueOfShortPosition))

        dailyHoldings <- dailyHoldings + valueOfShortPosition
        coinHoldingValue <- valueOfShortPosition
        # print(dailyHoldings)
      }
    } else {
      coinHoldingValue <- 0
      currentSymbolPrice <- 0
    }
    currentRow <- cbind.data.frame(selectedUser, currentDate, symbol, coinHoldingQty, currentSymbolPrice, coinHoldingValue)
    allDailyNetWorthByCoins <- rbind.data.frame(allDailyNetWorthByCoins,currentRow )
  }
  # print(dailyHoldings)
  simulation[i, dailyCoinNetWorthCol] <- dailyHoldings
  

  
  # print("insert success")
}

dailyCoinNetWorth <- simulation$dailyCoinNetWorth
simulation$dailyCoinNetWorth <- NULL
simulation$cash <- cashHoldings

x <- subset(allDailyNetWorthByCoins, coinHoldingQty != 0)

allDailyNetWorthByCoins$currentSymbolPrice <- NULL
# colnames(allDailyNetWorth) <- c("account", "date", "dailyNetWorth")
# write.csv(allDailyNetWorthByCoins, "accountDailyNetWorthByCoins.csv", row.names = F)
##########################################################################################################
simulation$date <- rownames(simulation)
simulationLong <- melt(simulation, id.vars = c("date"))
simulationLong$user <- selectedUser

# dailyNetWorth <- dailyCoinNetWorth + cashHoldings
# dailyNetWorthUserForUser <- cbind.data.frame(selectedUser, rownames(simulation), dailyNetWorth)
# allDailyNetWorthByCoins <- rbind.data.frame(allDailyNetWorthByCoins, simulationLong)
}

# colnames(allDailyNetWorth) <- c("account", "date", "dailyNetWorth")
# write.csv(allDailyNetWorthByCoins, "accountDailyNetWorthByCoins.csv", row.names = F)

###########
# simulationLong$closingPriceId <- paste(simulationLong$variable, simulationLong$date, sep=":")
# simulationLong <- merge(simulationLong, closingPrices[,c("id", "close")], by.x = "closingPriceId", by.y = "id")
# 
# 
# simulationLong <- simulationLong[order(simulationLong$user, simul)]