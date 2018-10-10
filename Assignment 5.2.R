trials = read.csv("clinical_trial.csv", stringsAsFactors = TRUE)
str(trials)
summary(trials)

max(nchar(as.character(trials$abstract)))
table(nchar(as.character(trials$abstract))==0)
trials$title[which.min(nchar(as.character(trials$title)))]

library(tm)
library(SnowballC)

corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

sparseTitle = removeSparseTerms(dtmTitle, sparse = 0.95)
sparseAbstract = removeSparseTerms(dtmAbstract, sparse = 0.95)

dtmTitle = as.data.frame(as.matrix(sparseTitle))
dtmAbstract = as.data.frame(as.matrix(sparseAbstract))

names(dtmAbstract)[which.max(colSums(dtmAbstract))]
which.max(colSums(dtmAbstract))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
str(dtm)
ncol(dtm)

set.seed(144)
library(caTools)

spl = sample.split(dtm$trial, 0.7)

traintrial = subset(dtm, spl == TRUE)
testtrial = subset(dtm, spl == FALSE)

table(traintrial$trial)[1]/nrow(traintrial)

library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data = traintrial, method = "class")
prp(trialCART)

max(predict(trialCART)[,2])

table((predict(trialCART)[,2]>0.5), traintrial$trial)
(631+441)/nrow(traintrial)
441/(441+131)
631/(631+99)

predTrial = predict(trialCART, newdata = testtrial, type = "class")
table(predTrial, testtrial$trial)
(261+162)/nrow(testtrial)

predTrial = predict(trialCART, newdata = testtrial)
library(ROCR)
trialRORC = prediction(predTrial[,2],testtrial$trial)
as.numeric(performance(trialRORC,"auc")@y.values)
