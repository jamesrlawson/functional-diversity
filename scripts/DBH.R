source("scripts/functions.R")
options(stringsAsFactors = FALSE)
library(plyr)
library(ggplot2)
library(reshape)
library(FD)

DBH <- na.omit(read.csv("data/DBH.csv", header=TRUE))
hydro <- read.csv("data/hydronorm.csv", header=TRUE)

DBH.small <- subset(DBH, dbh < 50)

stemsPerPlot <- tapply(DBH$plotID, DBH$plotID, length)
basalAreaPerPlot <- tapply(DBH$dbh, DBH$plotID, sum)

stemsPerPlot.small <- tapply(DBH.small$plotID, DBH.small$plotID, length)

# replace missing value for site 6 with zero

#stemsPerPlot.small1 <- stemsPerPlot.small[1:5]
#stemsPerPlot.small1[6] = c(0)
#stemsPerPlot.small1[7:15] <- stemsPerPlot.small[6:14]
#stemsPerPlot.small <- stemsPerPlot.small1

basalAreaPerPlot.small <- tapply(DBH.small$dbh, DBH.small$plotID, sum)

#basalAreaPerPlot.small1 <- basalAreaPerPlot.small[1:5]
#basalAreaPerPlot.small1[6] = c(0)
#basalAreaPerPlot.small1[7:15] <- basalAreaPerPlot.small[6:14]
#basalAreaPerPlot.small <- basalAreaPerPlot.small1

## multiply 500m2 plot by 20 to get 10000m2 Ha)

hydro$stemsPerHa <- stemsPerPlot * 20
hydro$basalAreaPerHa <- basalAreaPerPlot * 20

hydro$stemsPerHa.small <- stemsPerPlot.small * 20
hydro$basalAreaPerHa.small <- basalAreaPerPlot.small * 20

## transform  ?? ##

hist((hydro$stemsPerHa), breaks=7)
hist((hydro$basalAreaPerHa), breaks=7)

hist(log2(hydro$stemsPerHa), breaks=7)
hist(log2(hydro$basalAreaPerHa), breaks=7)

hydro$stemsPerHa <- log2(hydro$stemsPerHa)
hydro$basalAreaPerHa <- log2(hydro$basalAreaPerHa)

hist((hydro$stemsPerHa.small), breaks=7)
hist((hydro$basalAreaPerHa.small), breaks=7)

hist(log(hydro$stemsPerHa.small), breaks=7)
hist(log2(hydro$basalAreaPerHa.small), breaks=7)

hydro$stemsPerHa.small <- log(hydro$stemsPerHa.small)
hydro$basalAreaPerHa.small <- log2(hydro$basalAreaPerHa.small)

#hydro[6,41] <- 0
#hydro[6,40] <- 0 

## get stats ##

getStats(hydro, hydro$stemsPerHa, DBH)
getStats(hydro, hydro$basalAreaPerHa, DBH)
getStats(hydro, hydro$stemsPerHa.small, DBH)
getStats(hydro, hydro$basalAreaPerHa.small, DBH)

plot.linear(hydro, hydro$stemsPerHa, DBH)
plot.linear(hydro, hydro$basalAreaPerHa, DBH)
plot.linear(hydro, hydro$stemsPerHa.small, DBH)
plot.linear(hydro, hydro$basalAreaPerHa.small, DBH)

plot.quad(hydro, hydro$stemsPerHa, DBH)
plot.quad(hydro, hydro$basalAreaPerHa, DBH)
plot.quad(hydro, hydro$stemsPerHa.small, DBH)
plot.quad(hydro, hydro$basalAreaPerHa.small, DBH)


## aov ##

stemsPerHa.aov <- aov(hydro$stemsPerHa ~ as.factor(hydro$category))
TukeyHSD(stemsPerHa.aov)

basalAreaPerHa.aov <- aov(hydro$basalAreaPerHa ~ as.factor(hydro$category))
TukeyHSD(basalAreaPerHa.aov)


### correlations with CWMs and richness ###

plot(hydroplots$maxheight.CWM, hydro$stemsPerHa.small)
cor(hydroplots$maxheight.CWM, hydro$stemsPerHa.small)
summary(lm(hydroplots$maxheight.CWM ~ hydro$stemsPerHa.small))

plot(hydroplots$maxheight.CWM, hydro$basalAreaPerHa.small)
cor(hydroplots$maxheight.CWM, hydro$basalAreaPerHa.small)
summary(lm(hydroplots$maxheight.CWM ~ hydro$basalAreaPerHa.small))


plot(hydroplots$seedmass.CWM, hydro$stemsPerHa.small)
cor(hydroplots$seedmass.CWM, hydro$stemsPerHa.small)
summary(lm(hydroplots$seedmass.CWM ~ hydro$stemsPerHa.small))

plot(hydroplots$FDis, hydro$stemsPerHa.small)
cor(hydroplots$FDis, hydro$stemsPerHa.small)
summary(lm(hydroplots$FDiv ~ hydro$stemsPerHa.small))




#########

DBH.1 <- subset(DBH.hydro, category ==1)
DBH.2 <- subset(DBH.hydro, category ==2)
DBH.3 <- subset(DBH.hydro, category ==3)

hist(DBH.1$dbh, breaks=20)
hist(DBH.2$dbh, breaks=20)
hist(DBH.3$dbh, breaks=20)


DBH.hydro <- merge(DBH, hydro)
DBH.hydro.large <- subset(DBH.hydro, dbh > 50)
DBH.large.aov <- aov(DBH.hydro.large$dbh ~ as.factor(DBH.hydro.large$category))
TukeyHSD(DBH.large.aov)

DBH.hydro <- merge(DBH, hydro)
DBH.hydro.small <- subset(DBH.hydro, dbh < 50)
DBH.small.aov <- aov(DBH.hydro.small$dbh ~ as.factor(DBH.hydro.small$category))
TukeyHSD(DBH.small.aov)


