stevens = read.csv("stevens.csv")
str(stevens)
summary(stevens)

library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, 0.7)

train = subset(stevens, spl==TRUE)
test = subset(stevens, spl==FALSE)

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent 
                    + LowerCourt + Unconst, data = train, method="class", minbucket=25)
prp(StevensTree)

predStevens = predict(StevensTree, newdata = test, type = "class")
table(predStevens, test$Reverse)

library(ROCR)
predictROC = predict(StevensTree, newdata = test)
predictROC

pred = prediction(predictROC[,2],test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)


install.packages("randomForest")
library(randomForest)

train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, ntree=200, nodesize=25)

predForest = predict(StevensForest, newdata = test)
table(test$Reverse, predForest)

set.seed(100)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, ntree=200, nodesize=25)

predForest = predict(StevensForest, newdata = test)
table(test$Reverse, predForest)
(43+74)/nrow(test)

set.seed(200)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, ntree=200, nodesize=25)

predForest = predict(StevensForest, newdata = test)
table(test$Reverse, predForest)
(44+76)/nrow(test)

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

numFolds = trainControl(method = "CV", number = 10)
cpGrid = expand.grid( .cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method="rpart",
      trControl = numFolds, tuneGrid = cpGrid)

StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train,
                      method="class", cp=0.19)
PredictCV = predict(StevensTreeCV, newdata = test, type = "class")
table(PredictCV, test$Reverse)

library(rpart)
library(rpart.plot)
prp(StevensTreeCV)
