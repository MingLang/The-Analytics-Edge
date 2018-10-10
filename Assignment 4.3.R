census = read.csv("census.csv")
str(census)
summary(census)

library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, 0.6)

train = subset(census, spl==TRUE)
test = subset(census, spl == FALSE)

censusLog = glm(over50k~., data = train, family = binomial)
summary(censusLog)

censuspred = predict(censusLog, newdata = test, type = "response")
table(censuspred>=0.5,test$over50k)
(9051+1888)/nrow(test)

max(table(test$over50k))/nrow(test)

library(ROCR)
ROCRpred = prediction(censuspred, test$over50k)
as.numeric(performance(ROCRpred,"auc")@y.values)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE)

library(rpart)
CARTcensus = rpart(over50k~., data = train, method = "class")
library(rpart.plot)
prp(CARTcensus)

CARTcensuspred = predict(CARTcensus, newdata = test, type = "class")
table(CARTcensuspred, test$over50k)
(9243+1596)/nrow(test)
PredictROC = predict(CARTcensus, newdata = test)

library(ROCR)
ROCRpred = prediction(PredictROC[,2], test$over50k)
as.numeric(performance(ROCRpred,"auc")@y.values)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE)

set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

library(randomForest)
set.seed(1)
censusforest = randomForest(over50k~., data = trainSmall)

predcensusforest = predict(censusforest, newdata = test)
table(predcensusforest, test$over50k)
(9586+1093)/nrow(test)

vu = varUsed(censusforest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusforest$forest$xlevels[vusorted$ix]))

varImpPlot(censusforest)


library(caret)
library(e1071)

numFolds = trainControl( method = "cv", number = 10 )
set.seed(2)
cpGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 

train(over50k~., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

library(rpart)
CARTcensusCV = rpart(over50k~., data = train, method = "class", cp=0.002)

CARTcensusCVpred = predict(CARTcensusCV, newdata = test, type = "class")
table(CARTcensusCVpred, test$over50k)
(9178+1838)/nrow(test)
library(rpart.plot)
prp(CARTcensusCV)
