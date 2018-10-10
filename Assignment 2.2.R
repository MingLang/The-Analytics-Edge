pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

summary(pisaTrain)
str(pisaTrain)

tapply(pisaTrain$readingScore,pisaTrain$male,summary)

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
nrow(pisaTrain)
nrow(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth,"White")
pisaTest$raceeth = relevel(pisaTest$raceeth,"White")
str(pisaTrain)

lmScore = lm(readingScore~.,data=pisaTrain)
summary(lmScore)

names(lmScore)
sqrt(mean(lmScore$residuals^2))

lmScore$coefficients[2]*(11-9)

predTest = predict(lmScore,newdata=pisaTest)
max(predTest)-min(predTest)
SSE = sum((predTest-pisaTest$readingScore)^2)
RMSE = sqrt(mean((predTest-pisaTest$readingScore)^2))

baseline = mean(pisaTrain$readingScore)
SST = sum((baseline-pisaTest$readingScore)^2)

1-SSE/SST
