abpr = read.csv("letters_ABPR.csv")
str(abpr)
summary(abpr)

abpr$isB = as.factor(abpr$letter=="B")

set.seed(1000)
library(caTools)
spl = sample.split(abpr$isB, 0.5)

train = subset(abpr, spl==TRUE)
test = subset(abpr, spl == FALSE)

table(test$isB)[1]/nrow(train)

library(rpart)
CARTb = rpart(isB~.-letter, data = train, method = "class")
preb = predict(CARTb, newdata = test, type = "class")

table(preb, test$isB)
(1118+340)/nrow(test)

library(randomForest)
abprforest = randomForest(isB~.-letter,data = train)
prebforest = predict(abprforest, newdata = test)

table(prebforest, test$isB)
(1165+375)/nrow(test)

set.seed(2000)
spl = sample.split(abpr$letter, 0.5)

train = subset(abpr, spl==TRUE)
test = subset(abpr, spl == FALSE)

max(table(test$letter))/nrow(test)

CARTabpr = rpart(letter~.-isB, data = train, method = "class")
preabpr = predict(CARTabpr, newdata = test, type = "class")

table(preabpr, test$letter)
sum(diag(table(preabpr, test$letter)))/nrow(test)

set.seed(1000)
abprforest = randomForest(letter~.-isB,data = train)
predabprforest = predict(abprforest, newdata = test)

table(predabprforest, test$letter)
sum(diag(table(predabprforest, test$letter)))/nrow(test)
