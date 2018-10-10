IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
PG = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

str(IBM)
summary(IBM)

IBM$Date = as.Date(IBM$Date,"%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
PG$Date = as.Date(PG$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)

sd(PG$StockPrice)

plot(CocaCola$Date,CocaCola$StockPrice,type="l")
plot(CocaCola,type="l")
lines(PG,col="red")
abline(v=as.Date(c("2000-03-01")),lwd=2)
abline(v=as.Date(c("1983-01-01")),col=2)

plot(CocaCola[301:432,],type="l",ylim=c(0,210))
lines(PG[301:432,],col=2)
lines(IBM[301:432,],col=3)
lines(GE[301:432,],col=4)
lines(Boeing[301:432,],col=5)

abline(v=as.Date(c("2000-03-01")),lwd=1)
abline(v=as.Date(c("1997-09-01")))
abline(v=as.Date(c("1997-11-01")))

tapply(IBM$StockPrice,match(months(IBM$Date,abbreviate=TRUE),month.abb),mean)-mean(IBM$StockPrice)
which.max(tapply(GE$StockPrice,match(months(GE$Date,abbreviate=TRUE),month.abb),mean))
which.max(tapply(CocaCola$StockPrice,match(months(CocaCola$Date,abbreviate=TRUE),month.abb),mean))

tapply(CocaCola$StockPrice,match(months(CocaCola$Date,abbreviate=TRUE),month.abb),mean)
tapply(IBM$StockPrice,match(months(IBM$Date,abbreviate=TRUE),month.abb),mean)
