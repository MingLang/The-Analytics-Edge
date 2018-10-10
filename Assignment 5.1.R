wiki = read.csv("wiki.csv", stringsAsFactors = TRUE)
wiki$Vandal = as.factor(wiki$Vandal)
str(wiki)
summary(wiki)
table(wiki$Vandal)

library(tm)
library(SnowballC)

corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded
corpusAdded[[1]]$content

corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
corpusAdded[[1]]$content

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved
corpusRemoved[[1]]$content

corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
corpusRemoved[[1]]$content

dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

set.seed(123)
library(caTools)

spl = sample.split(wikiWords$Vandal, 0.7)

trainWiki = subset(wikiWords, spl == TRUE)
testWiki = subset(wikiWords, spl == FALSE)

table(testWiki$Vandal)[1]/nrow(testWiki)

library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~ ., data = trainWiki, method = "class")
predwikiCART = predict(wikiCART, newdata = testWiki, type = "class")

table(predwikiCART, testWiki$Vandal)
(618+12)/nrow(testWiki)
prp(wikiCART)

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http", wiki$Added, fixed = TRUE), 1, 0)
table(wikiWords2$HTTP)

trainWiki2 = subset(wikiWords2, spl==TRUE)
testWiki2 = subset(wikiWords2, spl==FALSE)

wikiCART2 = rpart(Vandal ~ ., data = trainWiki2, method = "class")
predwikiCART2 = predict(wikiCART2, newdata = testWiki2, type = "class")

table(predwikiCART2, testWiki2$Vandal)
(609+57)/nrow(testWiki2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)
mean(wikiWords2$NumWordsRemoved)

trainWiki2 = subset(wikiWords2, spl==TRUE)
testWiki2 = subset(wikiWords2, spl==FALSE)

wikiCART2 = rpart(Vandal ~ ., data = trainWiki2, method = "class")
predwikiCART2 = predict(wikiCART2, newdata = testWiki2, type = "class")

table(predwikiCART2, testWiki2$Vandal)
(514+248)/nrow(testWiki2)

wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

trainWiki3 = subset(wikiWords3, spl==TRUE)
testWiki3 = subset(wikiWords3, spl==FALSE)

wikiCART3 = rpart(Vandal ~ ., data = trainWiki3, method = "class")
predwikiCART3 = predict(wikiCART3, newdata = testWiki3, type = "class")

table(predwikiCART3, testWiki3$Vandal)
(595+241)/nrow(testWiki3)
prp(wikiCART3)
