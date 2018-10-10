climate = read.csv("climate_change.csv")
summary(climate)
str(climate)

train = subset(climate, Year<=2006)
test = subset(climate, Year>2006)

lm1 = lm(Temp ~ . - Year - Month, data=train)
summary(lm1)

cor(train)
pairs(train)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
#pairs(~. - Year - Month, data=train,lower.panel=panel.smooth, upper.panel=panel.cor, pch=20, main="train")

lm2 = lm(Temp ~ MEI + TSI +Aerosols + N2O, data=train)
summary(lm2)

lm3=step(lm1)
summary(lm3)

pre = predict(lm3,newdata=test)
1-sum((pre-test$Temp)^2)/sum((test$Temp-mean(train$Temp))^2)
                             
plot(test$Temp)
lines(pre)
