parole = read.csv("parole.csv")
summary(parole)
str(parole)

table(parole$violator)

summary(parole$state)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole$state)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

nrow(train)/nrow(parole)

sum(train1 != train2)

glm1 = glm(violator~., data=train, family=binomial)
summary(glm1)

parolee = train[1,]
summary(parolee)

parolee$age=50
parolee$time.served=3
parolee$max.sentence=12
parolee$crime=2
parolee$crime=as.factor(parolee$crime)
summary(parolee)

paroleePred = predict(glm1, type="response",newdata = parolee)
paroleePred/(1-paroleePred)

parolePred = predict(glm1, type="response", newdata = test)
max(parolePred)

table(parolePred>=0.5,test$violator)

sensitivityglm1 = 12/(12+11)
specificityglm1 = 167/(167+12)
accuracyglm1 = (167+12)/nrow(test)

accuracybase = (167+12)/nrow(test)

library(ROCR)
ROCRpred = prediction(parolePred,test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
