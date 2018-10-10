feaeur = read.csv("FEAEUR.csv")
dat = ts(feaeur$TEU[1:84],start = c(2008,1),frequency = 12)

plot(dat)
plot(diff(dat))
plot(log10(dat))
plot(diff(log10(dat)))

install.packages("forecast")
library(forecast)

tsdisplay(diff(dat,12))

par(mfrow = c(1,2))
acf(dat)
pacf(dat)
dev.off()

require(forecast)
arimafit = auto.arima(dat,approximation=FALSE,trace=FALSE)
summary(arimafit)
pred = forecast(arimafit, h =12)
plot(pred)

dat = ts(feaeur$TEU[1:96],start = c(2008,1),frequency = 12)
lines(dat,col="red")
