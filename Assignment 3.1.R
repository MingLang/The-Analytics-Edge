songs = read.csv("songs.csv")
summary(songs)
str(songs)

table(songs$year)
table(songs$artistname)
MJTop10 = subset(songs, artistname == "Michael Jackson" & Top10 == 1)
MJTop10$songtitle

levels(as.factor(songs$timesignature))
mode(songs$timesignature)

sort(table(songs$timesignature))

songs$songtitle[which.max(songs$tempo)]

songsTrain = subset(songs, year <= 2009)
songsTest = subset(songs, year == 2010)
nrow(songsTrain)
nrow(songsTest)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
songsTrain = songsTrain[ , !(names(songsTrain) %in% nonvars) ]
songsTest = songsTest[ , !(names(songsTest) %in% nonvars) ]
songsLog1 = glm(Top10 ~ ., data=songsTrain, family=binomial)
summary(songsLog1)

cor(songsTrain$loudness,songsTrain$energy)

songsLog2 = glm(Top10 ~ .-loudness, data=songsTrain, family = binomial)
songsLog3 = glm(Top10 ~ .-energy, data=songsTrain, family = binomial)
summary(songsLog2)
summary(songsLog3)

install.packages("ROCR")
library(ROCR)

predictTest = predict(songsLog3, type="response",newdata = songsTest)
summary(predictTest)
table(predictTest>=0.45,songsTest$Top10)

accuracyLog3=(19+309)/373
accuracyBase=(309+5)/373

sensitivityLog3=19/(40+19)
specificityLog3=309/(309+5)
