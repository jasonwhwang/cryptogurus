library(stringr)
library(tm)
library(wordcloud)


anbessabuy1<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/anbessa100 (1)/LinSVC_full_buy_words (1).csv")
anbessabuy2<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/anbessa100 (1)/MultiNB_full_buy_words (1).csv")
anbessabuy3<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/anbessa100 (1)/LogReg_full_buy_words (1).csv")
#makibuy1<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/cryptousemaki (1)/LinSVC_full_buy_words (1).csv")
#makibuy2<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/cryptousemaki (1)/MultiNB_full_buy_words (1).csv")
#makibuy3<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/cryptousemaki (1)/LogReg_full_buy_words (1).csv")
#yodabuy1<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/CryptoYoda1338 (1)/MultiNB_full_buy_words (1).csv")
#yodabuy2<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/CryptoYoda1338 (1)/LinSVC_full_buy_words (1).csv")
#yodabuy3<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/CryptoYoda1338 (1)/LogReg_full_buy_words (1).csv")
#zeusbuy1<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/ZeusZissou (1)/LinSVC_full_buy_words (1).csv")
#zeusbuy2<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/ZeusZissou (1)/MultiNB_full_buy_words (1).csv")
#zeusbuy3<- readLines("C:/Users/Haowen/Desktop/twt user results (3 class + stemmed + upsamp) app6 (1)-20180417T030844Z-001/twt user results (3 class + stemmed + upsamp) app6 (1)/ZeusZissou (1)/LogReg_full_buy_words (1).csv")

anbessabuy = paste(anbessabuy1, anbessabuy2, anbessabuy3, collapse = "")
#allbuy = paste(anbessabuy1, anbessabuy2, anbessabuy3, makibuy1, makibuy2, makibuy3, yodabuy1, yodabuy2, yodabuy3, zeusbuy1, zeusbuy2, zeusbuy3, collapse = "")
corpus <- Corpus(VectorSource(anbessabuy))
#corpus <- Corpus(VectorSource(allbuy))

tdm <- TermDocumentMatrix(corpus,control = list(removePunctuation= TRUE, stopwords = FALSE , removeNumbers = TRUE))



m = as.matrix(tdm)
mt <- t(m)

word_freqs = sort(rowSums(m), decreasing = TRUE)
dm = data.frame(word=names(word_freqs), freq=word_freqs)

wordcloud(dm$word, dm$freq, random.order=FALSE)
