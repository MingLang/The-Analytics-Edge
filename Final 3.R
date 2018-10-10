energy = read.csv("energy.csv")
summary(energy)
str(energy)

sort(tapply(energy$GenTotalRenewable, energy$STATE, sum))
energy$YEAR[which.max(energy$GenTotalRenewable[energy$STATE == "ID"])]

tapply(energy$AllSourcesCO2, energy$STATE, mean, na.rm = TRUE)
tapply(energy$presidential.results, energy$STATE, sum) == 0

sum(tapply(energy$AllSourcesCO2, energy$STATE, mean, na.rm = TRUE)*(tapply(energy$presidential.results, energy$STATE, sum) == 0))/sum(tapply(energy$presidential.results, energy$STATE, sum) == 0)
sum(tapply(energy$AllSourcesCO2, energy$STATE, mean, na.rm = TRUE)*(tapply(energy$presidential.results, energy$STATE, sum) == 14))/sum(tapply(energy$presidential.results, energy$STATE, sum) == 14)

sum(tapply(energy$AllSourcesNOx, energy$STATE, mean, na.rm = TRUE)*(tapply(energy$presidential.results, energy$STATE, sum) == 0))/sum(tapply(energy$presidential.results, energy$STATE, sum) == 0)
sum(tapply(energy$AllSourcesNOx, energy$STATE, mean, na.rm = TRUE)*(tapply(energy$presidential.results, energy$STATE, sum) == 14))/sum(tapply(energy$presidential.results, energy$STATE, sum) == 14)
