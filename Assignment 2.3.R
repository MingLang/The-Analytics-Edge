FluTrain = read.csv("FluTrain.csv")
summary(FluTrain)
str(FluTrain)

FluTrain$Week[which.max(FluTrain$ILI)]
FluTrain$Week[which.max(FluTrain$Queries)]

plot(FluTrain$Week,FluTrain$ILI)
plot(FluTrain$Week,FluTrain$Queries)

hist(FluTrain$ILI,breaks=40)
hist(FluTrain$Queries,breaks=40)

plot(FluTrain$Queries,log(FluTrain$ILI))

FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)
(cor(FluTrain$Queries,log(FluTrain$ILI)))^2

FluTest = read.csv("FluTest.csv")

predFlu = exp(predict(FluTrend1,newdata = FluTest))
pred20120311=predFlu[which(FluTest$Week=="2012-03-11 - 2012-03-17")]
obs20120311=FluTest$ILI[which(FluTest$Week=="2012-03-11 - 2012-03-17")]
relativeError = (obs20120311 - pred20120311) / obs20120311

RMSE1 = sqrt(mean((FluTest$ILI-predFlu)^2))

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)

plot(log(FluTrain$ILI),log(FluTrain$ILILag2))

FluTrend2 = lm(log(ILI)~Queries+log(ILILag2),data = FluTrain)
summary(FluTrend2)

ILILag2Test = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2Test)
summary(FluTest$ILILag2)


FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]

predFlu2 = exp(predict(FluTrend2,newdata = FluTest))
RMSE2 = sqrt(mean((FluTest$ILI-predFlu2)^2))
