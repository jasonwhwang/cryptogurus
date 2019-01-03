

dataMain <- read.csv("process_all_tweets.csv", stringsAsFactors = F)
allCoins <- read.csv("listOfCoins.csv")
tradingTerms <- read.csv("tradingTermsAll.csv")

##################################################################
#process data frames
###########################
data <- subset(dataMain, new_tweets != "")
###########################
allCoins$Coin <- tolower(allCoins$Coin)
allCoins$Symbol <- tolower(allCoins$Symbol)
###########################
tradingTermsString <- paste(tradingTerms$term, collapse='|' )

##################################################################


coinsNamesAndSymbols <- c(allCoins$Coin, allCoins$Symbol)

data$explictCoinMentioned <- grepl(".\\$</s><b>", data$tweets, ignore.case = TRUE)
data$coinMentioned <- grepl(paste(coinsNamesAndSymbols,collapse="|"), data$new_tweets, ignore.case = TRUE)
data$termsMentioned <- grepl(tradingTermsString, data$new_tweets)

releveantTweetsOld <- subset(data, termsMentioned == TRUE | coinMentioned == TRUE)
releveantTweetsNew <- subset(data, explictCoinMentioned == TRUE | termsMentioned == TRUE | coinMentioned == TRUE)

nrow(releveantTweetsOld)
nrow(releveantTweetsNew)

z <- subset(data, explictCoinMentioned == TRUE & termsMentioned == FALSE & coinMentioned == FALSE)

# write.csv(releveantTweets, "relevantTweets.csv", row.names = F)