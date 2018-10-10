airlines = read.csv("AirlinesCluster.csv")
str(airlines)
summary(airlines)

library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

airlinesdist = dist(airlinesNorm, method = "euclidean") 
airlinesHC = hclust(airlinesdist, method = "ward.D")

plot(airlinesHC)

airlinesGroup = cutree(airlinesHC, k=5)
table(airlinesGroup)

tapply(airlines$Balance, airlinesGroup, mean)
spl = split(airlines,airlinesGroup)
lapply(spl, colMeans)

k = 5
library(caTools)
set.seed(88)
kmeansClust = kmeans(airlinesNorm, centers = k, iter.max = 1000)
table(kmeansClust$cluster)
kmeansClust$centers
