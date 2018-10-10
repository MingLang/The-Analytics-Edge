fedFunds = read.csv("federalFundsRate.csv",stringsAsFactors = FALSE)
summary(fedFunds)
str(fedFunds)

table(fedFunds$Streak>0)
295/585

table(fedFunds$Chairman)

fedFunds$Chairman = as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres = as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds = as.factor(fedFunds$RaisedFedFunds)

set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)

train = subset(fedFunds, spl == TRUE)
test = subset(fedFunds, spl == FALSE)

logModel = glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection,
               data = train, family = "binomial")
summary(logModel)

temp = test[1,]
temp[3] = 1.7
temp[4] = -3
temp[6] = 5.1
temp[8] = 65.3
temp[10] = as.factor(0)
temp[11] = 18

pred = predict(logModel, newdata = temp, type = "response")

sum(logModel$coefficients*c(1,1.7,-3,5.1,65.3,0,18))
1/(1+exp(-sum(logModel$coefficients*c(1,1.7,-3,5.1,65.3,0,18))))

pred = predict(logModel, newdata = test, type = "response")

table(test$RaisedFedFunds, pred>0.5)
table(test$RaisedFedFunds)

library(ROCR)
ROCpred = prediction(pred, test$RaisedFedFunds)
as.numeric(performance(ROCpred, "auc")@y.values)

ROCperf = performance(ROCpred, "tpr", "fpr")
plot(ROCperf, colorize =TRUE, print.cutoffs.at=seq(0,1,0.1), test.adj=c(-0.2,1.7))

library(caret)     
library(e1071)
set.seed(201)
numFolds = trainControl(method="cv", number = 10)
cpGrid = expand.grid(.cp=seq(0.001,0.05,0.001))
train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection,
      data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
library(rpart.plot)
fedFundsCV = rpart(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection,
                   data = train, method = "class", cp = 0.016)
prp(fedFundsCV)

pred = predict(fedFundsCV, newdata = temp, type = "class")

pred = predict(fedFundsCV, newdata = test, type = "class")
table(test$RaisedFedFunds,pred)
(64+48)/nrow(test)
