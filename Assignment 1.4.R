poll = read.csv("AnonymityPoll.csv")
summary(poll)
str(poll)
nrow(poll)

table(poll$Smartphone)
summary(poll$Smartphone)

table(poll$State,poll$Region)
midWest = subset(poll, Region=="Midwest")
table(midWest$State)

sort(table(poll$State,poll$Region)[,3])
sort(table(subset(poll, Region=="South")$State))

table(poll$Internet.Use,poll$Smartphone)

summary(poll$Internet.Use)
summary(poll$Smartphone)

limited = subset(poll, Internet.Use == 1 | Smartphone == 1)
nrow(limited)

summary(limited)

mean(limited$Info.On.Internet)
table(limited$Info.On.Internet)

mean(limited$Worry.About.Info,na.rm=TRUE)
summary(limited$Worry.About.Info)

summary(limited$Anonymity.Possible)

summary(limited$Tried.Masking.Identity)

summary(limited$Privacy.Laws.Effective)

hist(limited$Age)

sort(table(limited$Age,limited$Info.On.Internet))
max(table(limited$Age,limited$Info.On.Internet))

jitter(c(1,2,3))

plot(jitter(limited$Age),jitter(limited$Info.On.Internet))

tapply(limited$Info.On.Internet,limited$Smartphone,mean)
tapply(limited$Info.On.Internet,limited$Smartphone,summary)

table(limited$Smartphone,limited$Tried.Masking.Identity)
tapply(limited$Tried.Masking.Identity,limited$Smartphone,summary)

