start_time <- Sys.time()
library(tidyverse)
data <- read.csv("C:\\Users\\dopha\\Documents\\DataMiningProject\\full_grouped.csv")
class(data)
nrow(data)
data
PreviousInfo <- dplyr::select(data, Date, Country.Region, Confirmed, Deaths, Recovered, WHO.Region)
CurrentInfo <- dplyr::select(data, Date, Country.Region, New.cases, New.deaths, New.recovered, WHO.Region)
ConfirmedCases <- dplyr::select(data, Date, Country.Region, Confirmed)
ConfirmedCases
ConfirmedCases$Confirmed[ConfirmedCases$Country.Region == "Vietnam"]
with(data, plot(Confirmed, Deaths))
pairs(dplyr::select(data, Confirmed, Deaths, Recovered))
psych::cor.plot(dplyr::select(data, Confirmed, Deaths, Recovered, Active))
EUcon <- sum(PreviousInfo$Confirmed[PreviousInfo$WHO.Region=="Europe"])
AFcon <- sum(PreviousInfo$Confirmed[PreviousInfo$WHO.Region=="Africa"])
UScon <- sum(PreviousInfo$Confirmed[PreviousInfo$WHO.Region=="Americas"])
WPcon <- sum(PreviousInfo$Confirmed[PreviousInfo$WHO.Region=="Western Pacific"])
EMcon <- sum(PreviousInfo$Confirmed[PreviousInfo$WHO.Region=="Eastern Mediterranean"])
SAcon <- sum(PreviousInfo$Confirmed[PreviousInfo$WHO.Region=="South-East Asia"])
RegionCases <- c(EUcon, AFcon, UScon, WPcon, EMcon, SAcon)
names(RegionCases) <- c("Europe","Africa","Americas","Western Pacific","Eastern Mediterranean","South-East Asia")
EUde <- sum(PreviousInfo$Deaths[PreviousInfo$WHO.Region=="Europe"])
AFde <- sum(PreviousInfo$Deaths[PreviousInfo$WHO.Region=="Africa"])
USde <- sum(PreviousInfo$Deaths[PreviousInfo$WHO.Region=="Americas"])
WPde <- sum(PreviousInfo$Deaths[PreviousInfo$WHO.Region=="Western Pacific"])
EMde <- sum(PreviousInfo$Deaths[PreviousInfo$WHO.Region=="Eastern Mediterranean"])
SAde <- sum(PreviousInfo$Deaths[PreviousInfo$WHO.Region=="South-East Asia"])
RegionDeaths <- c(EUde, AFde, USde, WPde, EMde, SAde)
names(RegionDeaths) <- c("Europe","Africa","Americas","Western Pacific","Eastern Mediterranean","South-East Asia")
par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
png(file="RegionCasesBarplot.png",
    width=1000, height=800)
barplot(RegionCases, main="Barplot")
dev.off()
png(file="RegionCasesPiechart.png",
    width=1000, height=800)
pie(RegionCases, main="Piechart", radius=1)
dev.off()
png(file="RegionCasesPiechart.png",
    width=1000, height=800)
barplot(RegionDeaths, main="barplot")
dev.off()
png(file="RegionCasesPiechart.png",
    width=1000, height=800)
pie(RegionDeaths, main="Piechart", radius=1)
dev.off()
Jan <- unique(data$Date)[1:10]
Feb <- unique(data$Date)[11:39]
Mar <- unique(data$Date)[40:70]
Apr <- unique(data$Date)[71:100]
May <- unique(data$Date)[101:131]
Jun <- unique(data$Date)[132:161]
Jul <- unique(data$Date)[162:188]
JanCases <- sum(PreviousInfo$Confirmed[data$Date %in% Jan])
FebCases <- sum(PreviousInfo$Confirmed[data$Date %in% Feb])
MarCases <- sum(PreviousInfo$Confirmed[data$Date %in% Mar])
AprCases <- sum(PreviousInfo$Confirmed[data$Date %in% Apr])
MayCases <- sum(PreviousInfo$Confirmed[data$Date %in% May])
JunCases <- sum(PreviousInfo$Confirmed[data$Date %in% Jun])
JulCases <- sum(PreviousInfo$Confirmed[data$Date %in% Jul])
WorldCases <- c(JanCases, FebCases, MarCases, AvrCases, MaiCases, JunCases, JulCases)
names(WorldCases) <- c("January", "February", "March", "April", "May", "June", "July")
barplot(WorldCases, main="barplot")
pie(WorldCases, main="Piechart", radius=1)
library(data.table)
spread <- data.table(CurrentInfo)
spread[,list(cases=sum(New.cases)), by='WHO.Region']
library("ggplot2")
contagious <- as.data.frame(spread)
contagious
MostSpread <- spread[,list(cases=sum(New.cases)), by='Country.Region']
MostSpread[order(cases,decreasing=TRUE),]
Overall <- data.table(PreviousInfo)
DeathsCount <- Overall[,list(bodycount=sum(Deaths)), by='Country.Region']
deceased <- as.data.frame(DeathsCount)
DeathsCount[order(bodycount,decreasing=TRUE),]
RecoveryCount <- Overall[,list(bodycount=sum(Recovered)), by='Country.Region']
healed <- as.data.frame(RecoveryCount)
RecoveryCount[order(bodycount,decreasing=TRUE),]
Percentage <- Overall[,list(Infected=sum(Confirmed),
                            Death=sum(Deaths),
                            Recover=sum(Recovered),
                            PercenDeaths=100*sum(Deaths)/sum(Confirmed),
                            PercenRecover=100*sum(Recovered)/sum(Confirmed),
                            Others=100*((sum(Confirmed)-sum(Deaths)-sum(Recovered))/sum(Confirmed))),
                      by='Country.Region']
Percentage[Percentage$Country.Region=="US"]
Percentage[order(PercenDeaths,decreasing=TRUE),]
Percentage[order(PercenRecover,decreasing=TRUE),]
# Linear Regression of Deaths in US
library(readxl)
dataLR <- dplyr::select(data, Country.Region, Confirmed, Deaths, Recovered)
dataLR
USDeaths <- dataLR$Deaths[dataLR$Country.Region=="US"]
USConfirmed <- dataLR$Confirmed[dataLR$Country.Region=="US"]
USRecovered <- dataLR$Recovered[dataLR$Country.Region=="US"]
USInfoOfDeaths <- data.frame(USDeaths, USConfirmed)
lrDeaths = lm(USDeaths~USConfirmed, data = USInfoOfDeaths) #Create the linear regression
summary(lrDeaths) #Review the results
#Plot the results
plot(USInfoOfDeaths, pch = 1, col = "blue")
plot(lrDeaths, pch = 1, col = "blue")
# same for recovered
USInfoOfRecovery <- data.frame(USDeaths, USConfirmed)
lrRecover = lm(USRecovered~USConfirmed, data = USInfoOfRecovery)
summary(lrRecover) #Review the results
plot(USInfoOfRecovery, pch = 1, col = "blue")
plot(lrRecover, pch = 1, col = "blue")
abline(lrDeaths)
end_time <- Sys.time()
start_time
end_time