tweets = read.csv("tweets.csv",stringsAsFactors = FALSE)
str(tweets)

library(tm)
corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus,content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

str(allTweets)



install.packages("wordcloud")
library(wordcloud)

?wordcloud

wordcloud(colnames(allTweets), colSums(allTweets))

corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus,content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))
ncol(allTweets)

wordcloud(colnames(allTweets), colSums(allTweets))

NegTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(NegTweets),colSums(NegTweets))

display.brewer.all() 

wordcloud(colnames(NegTweets),colSums(NegTweets),colors = brewer.pal(9, "Blues")[-1:-4])
