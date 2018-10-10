dailykos = read.csv("dailykos.csv")
str(dailykos)
summary(dailykos)

kosDist = dist(dailykos,method = "euclidean")
kosHierClust = hclust(kosDist, method = "ward.D")

plot(kosHierClust)

kosGroups = cutree(kosHierClust, k =7)
table(kosGroups)

HierCluster1 = subset(dailykos, kosGroups == 1)
HierCluster2 = subset(dailykos, kosGroups == 2)
HierCluster3 = subset(dailykos, kosGroups == 3)
HierCluster4 = subset(dailykos, kosGroups == 4)
HierCluster5 = subset(dailykos, kosGroups == 5)
HierCluster6 = subset(dailykos, kosGroups == 6)
HierCluster7 = subset(dailykos, kosGroups == 7)

tail(sort(colMeans(HierCluster1)))
tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
tail(sort(colMeans(HierCluster5)))
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))

k = 7
library(caTools)
set.seed(1000)
KMC = kmeans(dailykos, centers = k)

table(KMC$cluster)
KmeansCluster = split(dailykos, KMC$cluster)
KmeansCluster[[1]]

tail(sort(colMeans(KmeansCluster[[1]])))
tail(sort(colMeans(KmeansCluster[[2]])))
tail(sort(colMeans(KmeansCluster[[3]])))
tail(sort(colMeans(KmeansCluster[[4]])))
tail(sort(colMeans(KmeansCluster[[5]])))
tail(sort(colMeans(KmeansCluster[[6]])))
tail(sort(colMeans(KmeansCluster[[7]])))

table(kosGroups, KMC$cluster)
