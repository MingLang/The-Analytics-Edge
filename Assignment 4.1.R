gerber = read.csv("gerber.csv")
str(gerber)
summary(gerber)
mean(gerber$voting)

table(gerber$hawthorne,gerber$voting)[2,2]/sum(gerber$hawthorne)
table(gerber$civicduty,gerber$voting)[2,2]/sum(gerber$civicduty)
table(gerber$neighbors,gerber$voting)[2,2]/sum(gerber$neighbors)
table(gerber$self,gerber$voting)[2,2]/sum(gerber$self)

tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$neighbors, mean)
tapply(gerber$voting, gerber$self, mean)

voteLog = glm(voting~.-sex - yob - control, data = gerber, family=binomial)
summary(voteLog)

predVote = predict(voteLog, type = "response")
confusionMatrix = table(predVote >= 0.3, gerber$voting)
(confusionMatrix[1,1]+confusionMatrix[2,2])/nrow(gerber)

confusionMatrix = table(predVote >= 0.5, gerber$voting)
(confusionMatrix[1,1])/nrow(gerber)

1-mean(gerber$voting)

library(ROCR)
ROCRpred = prediction(predVote,gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

library(rpart)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
library(rpart.plot)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

table(gerber$sex,gerber$voting,gerber$control)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)

summary(CARTmodel4)
prp(CARTmodel4, digits = 6)
abs(CARTmodel4$frame[2,5] - CARTmodel4$frame[3,5])

prp(CARTmodel5, digits = 6)

Logmodel = glm(voting ~ control + sex, data=gerber, family = binomial )
summary(Logmodel)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(Logmodel, newdata=Possibilities, type="response")
abs(0.290456-0.2908065)

Logmodel2 = glm(voting ~ control + sex + sex:control, data=gerber, family = binomial )
summary(Logmodel2)
predict(Logmodel2, newdata=Possibilities, type="response")
abs(0.290456-0.2904558)
