tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
summary(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]$content

corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]$content

corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content

stopwords("english")

corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))
corpus[[1]]$content

corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content

frequencies = DocumentTermMatrix(corpus)
frequencies

inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq = 20)
findFreqTerms(frequencies, lowfreq = 100)

sparse = removeSparseTerms(frequencies, 0.995)
sparse

tweetsSparse = as.data.frame(as.matrix(sparse))

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
str(tweetsSparse)

tweetsSparse$Negative = tweets$Negative

library(caTools)
set.seed(123)
spl = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, spl == TRUE)
testSparse = subset(tweetsSparse, spl == FALSE)

tweetLog = glm (Negative ~ ., data = trainSparse, family = binomial)
predLog = predict(tweetLog, newdata = testSparse, type = "response")
table(predLog>0.5, testSparse$Negative)

(253+33)/nrow(testSparse)
