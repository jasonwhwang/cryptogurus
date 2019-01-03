library(stringr)


data <- read.csv("relevantTweetsTagged.csv", stringsAsFactors = F)
allCoins <- read.csv("listOfCoins.csv")

###########################
allCoins$Coin <- tolower(allCoins$Coin)
allCoins$Symbol <- tolower(allCoins$Symbol)
allCoins <- subset(allCoins, Symbol != "amp")
########################################################################################

data <- subset(data, Buy.Neutral.Sell != 0)
#adjust time zome
data$timeStamp <- as.POSIXct(data$timeStamp, "%Y-%m-%d %H:%M:%S", tz = "GMT") - (3600*8)

data$replies <- data$retweets <- data$favs <- data$new_dts <- data$assigned_member <- NULL
data$termsMentioned <- NULL
########################################################################################
#get coins mentioned

#get explicit symbols first
data$explicitSymbols <- tolower(lapply(data$tweets, function(x) paste(str_match_all(x, ".\\$</s><b>(.*?)</b>")[[1]][,2], collapse = ",")))

#get implicit symbols
data$implicitSymbols <- NA
implicitSymbolsCol <- which(colnames(data) == "implicitSymbols")

tweetCol <- which(colnames(data) == "tweets")
newTweetCol <- which(colnames(data) == "new_tweets")




for (currentTweetRow in 1:nrow(data)){
  
 print(currentTweetRow)
  
  coinSymbols <- NULL
  currentTweet <- data[currentTweetRow, tweetCol]
  
  for (currentCoinRow in 1:nrow(allCoins)){
    currentCoinSymbol <- allCoins[currentCoinRow,2]
    currentCoinName <- allCoins[currentCoinRow,1]
    
    #if a symbol exists
    if (grepl(currentCoinSymbol, currentTweet, ignore.case = TRUE) == TRUE){
      coinSymbols <- c(coinSymbols, currentCoinSymbol)
    }
    
    #if a name exists
    if (grepl(currentCoinName, currentTweet, ignore.case = TRUE) == TRUE){
      
      #then get the symbol
      symbolOfCoinName <- subset(allCoins, Coin == currentCoinName)$Symbol
      coinSymbols <- c(coinSymbols, symbolOfCoinName)
    }
    
    
    
    print(coinSymbols)
    
    if(is.null(coinSymbols) != TRUE){
      coinSymbols <- unique(coinSymbols)
      data[currentTweetRow, implicitSymbolsCol] <- paste(coinSymbols, collapse = ",")
                                                                
    }
  }
}


data$symbolToUse <- ifelse(data$explicitSymbols != "", data$explicitSymbols, data$implicitSymbols)
data$symbolToUse2 <- lapply(data$symbolToUse, function(x) paste(unique(str_split(x, pattern = ",")[[1]]), collapse = ","))

########################################################################################
#generate new df to observe signals (buy/sell)

#keep only the cols we want
signals <- cbind.data.frame(data$X, data$Buy.Neutral.Sell, data$timeStamp, data$user, data$symbolToUse)
colnames(signals) <- c("tweetId", "signal", "timeStamp", "user", "symbolToUse")

#remove any rows without a valid symbol to use
signals <- subset(signals, is.na(symbolToUse) == F)


s <- strsplit(as.character(signals$symbolToUse), split = ",")
signalsBySymbol <- data.frame(V1 = rep(signals$tweetId, sapply(s, length)), V2 = unlist(s))
signalsBySymbol <- merge(signalsBySymbol, signals, by.y = "tweetId", by.x = "V1")

signalsBySymbol$symbolToUse <- NULL
signalsBySymbol$signalId <- 1:nrow(signalsBySymbol)

colnames(signalsBySymbol) <- c("tweetId", "symbol", "signal", "timeStamp", "user", "signalId")

###################################################
#priceAtTime 

signalsBySymbol$priceAtTime <- NA
priceAtTimeCol <- which(colnames(signalsBySymbol) == "priceAtTime")
symbolCol <- which(colnames(signalsBySymbol) == "symbol")
timeCol <- which(colnames(signalsBySymbol) == "timeStamp")

poloniex.public <- PoloniexPublicAPI()

for (i in 1:nrow(signalsBySymbol)){
  
  currentSymbol <- as.character(signalsBySymbol[i, symbolCol])
  currentTime <- signalsBySymbol[i, timeCol]
  
  print(i)
  print(currentSymbol)
  
  pairString <- paste("USDT_", toupper(currentSymbol), sep="")
  
  retrievedPrices <-  tryCatch({as.data.frame(ReturnChartData(poloniex.public, pair = pairString,
                                                              from = as.POSIXct(currentTime - 20*60),
                                                              to = as.POSIXct(currentTime), period = "5M"))},
                               error = function(e){})
  
  
  retrievedPrice <- tail(retrievedPrices,1)[4]
  
  if (is.null(retrievedPrice) == T){
    startDate <- gsub("-", "", as.Date(currentTime - 24*60*60))
    endDate <- gsub("-", "", as.Date(currentTime))
    
    retrievedPrice <- tryCatch({tail(getCoins(coin = toupper(currentSymbol), limit = 1, cpu_cores = NULL , start_date = startDate, end_date = endDate),1)[9]},
                               error = function(e){})
    
    print(getCoins(coin = toupper(currentSymbol), limit = 1, cpu_cores = NULL , start_date = startDate, end_date = endDate))
  }
  
  signalsBySymbol[i, priceAtTimeCol] <- retrievedPrice
  
}