emails = read.csv("emails.csv", stringsAsFactors = TRUE)
str(emails)
summary(emails)

sum(emails$spam)
emails$text[1]

max(nchar(as.character(emails$text)))
which.min(nchar(as.character(emails$text)))

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm, 0.95)

emailsSparse  = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

colSums(emailsSparse)[which.max(colSums(emailsSparse))]

emailsSparse$spam = emails$spam

sort(colSums(subset(emailsSparse, emailsSparse$spam == 0)))
sort(colSums(subset(emailsSparse, emailsSparse$spam == 1)))

emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)

train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

spamLog = glm(spam ~ ., data = train, family = binomial)
summary(spamLog)

library(rpart)
library(rpart.plot)
spamCART = rpart(spam~., data = train, method = "class")
prp(spamCART)

library(randomForest)
set.seed(123)
spamRF = randomForest(spam~., data = train)

#pred train
predspamLog = predict(spamLog, type="response")
sum(predspamLog < 0.00001)
sum(predspamLog > 0.99999)
sum(predspamLog >= 0.00001 & predspamLog <= 0.99999)
table(predspamLog>=0.5, train$spam)
(3052+954)/nrow(train)

library(ROCR)
spamLogROCR = prediction(predspamLog, train$spam)
as.numeric(performance(spamLogROCR,"auc")@y.values)

predspamCART = predict(spamCART)
predspamRF = predict(spamRF,type = "prob")

table(predspamCART[,2]>=0.5, train$spam)
(2885+894)/nrow(train)

spamCARTROCR = prediction(predspamCART[,2], train$spam)
as.numeric(performance(spamCARTROCR,"auc")@y.values)

table(predspamRF[,2]>=0.5, train$spam)
(3013+914)/nrow(train)

spamRFROCR = prediction(predspamRF[,2], train$spam)
as.numeric(performance(spamRFROCR,"auc")@y.values)

#pred test
predspamLog = predict(spamLog, type="response", newdata = test)
table(predspamLog>=0.5, test$spam)
(1257+376)/nrow(test)

spamLogROCR = prediction(predspamLog, test$spam)
as.numeric(performance(spamLogROCR,"auc")@y.values)

predspamCART = predict(spamCART, newdata = test)
predspamRF = predict(spamRF,type = "prob", newdata = test)

table(predspamCART[,2]>=0.5, test$spam)
(1228+386)/nrow(test)

spamCARTROCR = prediction(predspamCART[,2], test$spam)
as.numeric(performance(spamCARTROCR,"auc")@y.values)

table(predspamRF[,2]>=0.5, test$spam)
(1290+386)/nrow(test)

spamRFROCR = prediction(predspamRF[,2], test$spam)
as.numeric(performance(spamRFROCR,"auc")@y.values)
