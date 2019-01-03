library(stringr)
library(PoloniexR)
library(crypto)

poloniex.public <- PoloniexPublicAPI()

data <- read.csv("signalsBySymbol.csv", stringsAsFactors = F)
data$timeStamp <- as.POSIXct(data$timeStamp, "%Y-%m-%d %H:%M:%S", tz = "GMT")
data <- na.omit(data)

###########
data <- data[order(data$timeStamp),]
timeStampCol <- which(colnames(data) == "timeStamp")
minDate <- data[1,timeStampCol] - 24*60*60
maxDate <- data[nrow(data),timeStampCol] + 24*60*60
###########
allDates <- seq(as.Date(minDate), as.Date(maxDate), by="days")
allCoins <- sort(unique(data$symbol))


###########
priceSheet <- data.frame(date = allDates)
priceSheet <- data.frame(symbol = character(), date = character(), closingPrice = numeric())

# rownames(priceSheet) <- allDates
# colnames(priceSheet) <- allCoins

startDate <- gsub("-", "", as.Date(minDate - 24*60*60))
endDate <- gsub("-", "", as.Date(maxDate))


allCoins <- head(allCoins,6)

for (coinIndex in 1:length(allCoins)){
  print(coinIndex)
  currentSymbol <- allCoins[coinIndex]
  print(currentSymbol)
    
    retrievedPrices <- tryCatch({getCoins(coin = toupper(currentSymbol), cpu_cores = NULL, start_date = startDate,
                                 end_date = endDate)[,c(4,9)]
      },  
                                 error = function(e){})
    
    if (is.null(retrievedPrices) == F){
      if(nrow(retrievedPrices) > 0){
        
        currentPriceSheet <- cbind.data.frame(currentSymbol, retrievedPrices)
        priceSheet <- rbind(priceSheet, currentPriceSheet)
        # priceSheet <- merge(priceSheet, retrievedPrices, by="date")
        # colnames(priceSheet)[ncol(priceSheet)] <- currentSymbol
        write.csv(priceSheet, "semiPriceSheet.csv", row.names = F)
      }
    }
}


length(allCoins) * length(allDates)