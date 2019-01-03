# docker pull selenium/standalone-chrome
# docker run -d -p 4445:4444 selenium/standalone-chrome

library(stringr)
library(RSelenium)
library(XML)
library(rvest)

# remDr <- remoteDriver(port = 4445L, browserName = "chrome")
# remDr$open() 
rD <- rsDriver(port = 4445L, browser = 'chrome')
remDr <- rD$client
# remDr$navigate("https://twitter.com/officialmcafee?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor")
# remDr$navigate("https://twitter.com/CryptoYoda1338")
# remDr$navigate("https://twitter.com/anbessa100")
# remDr$navigate("https://twitter.com/crypto_trader?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor")
remDr$navigate("https://twitter.com/ZeusZissou")
# remDr$navigate("https://twitter.com/cryptousemaki")


webElem <- remDr$findElement("css", "body")

for (i in 1:1500){
  # for (i in 1:15){
  print(i)
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(1L)
  
}




elemtxt <- webElem$getElementAttribute("outerHTML")[[1]]
text <- as.character(elemtxt)

################################ extract tweets ###################################################
tweets <- read_html(elemtxt) %>% html_nodes(xpath = '//*[@class="js-tweet-text-container"]') 

tweets1 <- matrix()

for (i in 1:length(tweets)){
  tweet <- as.character(tweets[i][1])
  tweets1 <- rbind(tweets1, tweet)
}
tweets1 <- as.data.frame(tweets1)
tweets1 <- na.omit(tweets1)

tweets2 <- as.matrix(substring(tweets1[,1], 148))
tweets2[,1] <- substr(tweets2[,1],1,nchar(tweets2[,1])-11)
rm(tweets1)
##################################################################################################

################################ extract time ###################################################
time <- read_html(elemtxt) %>% html_nodes(xpath = '//*[@class="stream-item-header"]') 

time1 <- matrix()

for (i in 1:length(time)){
  currentTime <- as.character(time[i][1])
  time1 <- rbind(time1, currentTime)
}


time1 <- as.data.frame(time1)
time1 <- na.omit(time1)

time2 <- as.matrix(str_extract(time1[,1],'tooltip" title(.*)data-conversation-id'))



time3 <- as.matrix(substring(time2[,1], 17))
time3[,1] <- substr(time3[,1],1,nchar(time3[,1])-22)
rm(time1, time2)
##################################################################################################

################################ extract fav ###################################################
fav <- read_html(elemtxt) %>% html_nodes(xpath = '//*[@class="ProfileTweet-action ProfileTweet-action--favorite js-toggleState"]') 

fav1 <- matrix()

for (i in 1:length(fav)){
  currentFav <- as.character(fav[i][1])
  fav1 <- rbind(fav1, currentFav)
}


fav1 <- as.data.frame(fav1)
fav1 <- na.omit(fav1)

fav2 <- as.matrix(str_extract(fav1[,1],'<span class="ProfileTweet-actionCountForPresentation" aria-hidden="true">(.*)</span>'))

#trim front and back
fav3 <- as.matrix(substring(fav2[,1], 74))
fav3[,1] <- substr(fav3[,1],1,nchar(fav3[,1])-7)


for (i in 1:nrow(fav3)){
  stat <- as.character(fav3[i,1])
  if (grepl("K", stat) == T){
    fav3[i,1] <- as.numeric(gsub("K", "", stat)) * 1000
  }
}

rm(fav, fav1, fav2)
##################################################################################################

################################ extract reply ###################################################
reply <- read_html(elemtxt) %>% html_nodes(xpath = '//*[@class="ProfileTweet-action ProfileTweet-action--reply"]') 

reply1 <- matrix()

for (i in 1:length(reply)){
  currentReply <- as.character(reply[i][1])
  reply1 <- rbind(reply1, currentReply)
}


reply1 <- as.data.frame(reply1)
reply1 <- na.omit(reply1)

reply2 <- as.matrix(str_extract(reply1[,1],'<span class="ProfileTweet-actionCountForPresentation" aria-hidden="true">(.*)</span>'))

#trim front and back
reply3 <- as.matrix(substring(reply2[,1], 74))
reply3[,1] <- substr(reply3[,1],1,nchar(reply3[,1])-7)


for (i in 1:nrow(reply3)){
  stat <- as.character(reply3[i,1])
  if (grepl("K", stat) == T){
    reply3[i,1] <- as.numeric(gsub("K", "", stat)) * 1000
  }
}

rm(reply, reply1, reply2)
##################################################################################################

################################ extract retweet ###################################################
retweet <- read_html(elemtxt) %>% html_nodes(xpath = '//*[@class="ProfileTweet-action ProfileTweet-action--retweet js-toggleState js-toggleRt"]') 

retweet1 <- matrix()

for (i in 1:length(retweet)){
  currentRetweet <- as.character(retweet[i][1])
  retweet1 <- rbind(retweet1, currentRetweet)
}


retweet1 <- as.data.frame(retweet1)
retweet1 <- na.omit(retweet1)

retweet2 <- as.matrix(str_extract(retweet1[,1],'<span class="ProfileTweet-actionCountForPresentation" aria-hidden="true">(.*)</span>'))

#trim front and back
retweet3 <- as.matrix(substring(retweet2[,1], 74))
retweet3[,1] <- substr(retweet3[,1],1,nchar(retweet3[,1])-7)


for (i in 1:nrow(retweet3)){
  stat <- as.character(retweet3[i,1])
  if (grepl("K", stat) == T){
    retweet3[i,1] <- as.numeric(gsub("K", "", stat)) * 1000
  }
}

rm(retweet, retweet1, retweet2)
##################################################################################################

main <- cbind.data.frame(tweets2, time3, reply3, retweet3, fav3)