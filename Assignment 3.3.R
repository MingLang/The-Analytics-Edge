loans = read.csv("loans.csv")
summary(loans)
str(loans)

mean(loans$not.fully.paid)

loansWOna= na.omit(loans)
summary(loansWOna)
str(loansWOna)

install.packages("mice")
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

summary(loans)
sum(loans!=read.csv("loans_imputed.csv"))
loans = read.csv("loans_imputed.csv")

set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split==TRUE)
test = subset(loans, split==FALSE)

glm1 = glm(not.fully.paid ~ ., data=train, family=binomial)
summary(glm1)

(700-710)*-9.317e-03
exp((700-710)*-9.317e-03)

testPred = predict(glm1, type = "response", newdata = test)
test$predicted.risk = testPred
table(testPred>=0.5,test$not.fully.paid)
accuracyglm1 = (2400+3)/nrow(test)
accuracybase = (2400+13)/nrow(test)

library(ROCR)
RORCpred = prediction(testPred,test$not.fully.paid)
as.numeric(performance(RORCpred,"auc")@y.values)

bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)

bivPred = predict(bivariate, type = "response", newdata = test)
max(bivPred)
bivRORC = prediction(bivPred,test$not.fully.paid)
as.numeric(performance(bivRORC,"auc")@y.values)

10*exp(3*0.06)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
10*max(test$profit)

highInterest = subset(test, int.rate>=0.15)
mean(highInterest$profit)
mean(highInterest$not.fully.paid)

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
lowRisk = subset(highInterest,predicted.risk<=cutoff)
nrow(lowRisk)

sum(lowRisk$profit)
table(lowRisk$not.fully.paid)
