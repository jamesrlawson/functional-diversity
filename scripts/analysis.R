#############################################################################################
#################### ANALYSIS FOR ONLY SPP. WITH COMPLETE DATA ####################
#############################################################################################
#################### LHS + woody/herbaceous ##################################################

source("scripts/functions.R")
options(stringsAsFactors = FALSE)
library(plyr)
library(ggplot2)
library(reshape)
library(FD)

#############################################################################################
############################# GET RELATIVE ABUNDANCES FOR ALL TRAITS ########################
#############################################################################################

# load in data

alltraits <- read.csv("data/alltraits.csv", header=TRUE)
percentcover <- read.csv("data/percentcover.csv", header=TRUE)
hydro <- read.csv("data/hydro_all.csv", header=TRUE)
WD <- read.csv("data/WD.csv", header=TRUE)
WDferns <- read.csv("data/WD.csv", header=TRUE)



# remove observations that average less than 1% cover
percentcover <- subset(percentcover, avgcover > 1)

# generate df with total % cover (across all strata) of species at each plot
plotsums <- ddply(percentcover, .(plotID, species), summarise, speciescover = sum(avgcover))

# test how much % cover per plot is reduced by applying na.omit to trait data

allspp <- alltraits
allspp$growthform <- NULL

allspp.cover <- findtraitvals(plotsums, allspp)

richness.all <- tapply(allspp.cover$species, allspp.cover$plotID, length)
plotcover.all <- tapply(allspp.cover$speciescover, allspp.cover$plotID, sum)

allspp <- alltraits
allspp <- na.omit(allspp)
# add in the ferns that got omitted due to no seedmass data
#allspp <- rbind(allspp, alltraits[53,]) # Calochlaena dubia
#allspp <- rbind(allspp, alltraits[43,]) # Doodia aspera
#allspp <- rbind(allspp, alltraits[113,]) # Blechnum nudum

allspp <- allspp[order(allspp$species), ]
allspp$WD <- WD$WD
allspp$growthform <- NULL

allspp.cover <- findtraitvals(plotsums, allspp)

richness.naomit <- tapply(allspp.cover$species, allspp.cover$plotID, length)
plotcover.naomit <- tapply(allspp.cover$speciescover, allspp.cover$plotID, sum)

dataDensity <- data.frame(cbind("richness" = richness.naomit / richness.all,
                                "plotcover" = plotcover.naomit / plotcover.all))

#View(dataDensity)

###

allspp.totalcover <- ddply(allspp.cover, .(plotID), summarise, totalcover = sum(speciescover))
allspp.cover <- merge(allspp.cover, allspp.totalcover, by.x = "plotID", by.y = "plotID")
allspp.cover <-relabund(allspp.cover)

test <- ddply(allspp.cover, .(plotID), summarise, summedRelCover = (sum(relcover)))
print(test)
rm(test)

#######

traits <- allspp.cover[2]
traits <- cbind(traits, allspp.cover[4:10])
traits$lifehistory <- NULL
traits <- ddply(traits, .(species), unique)

# transform traits

hist(log10(traits$seedmass))
hist(log10(traits$SLA))
hist(log10(traits$maxheight), breaks = 5)
hist((traits$flowering.period)) # non-normal distribution with any common transform

traits$seedmass <- log10(traits$seedmass)
traits$SLA <- log10(traits$SLA)
traits$maxheight <- log10(traits$maxheight)

traits$woody <- as.numeric(traits$woody)
#traits$lifehistory <- as.numeric(traits$lifehistory)


# create abun, in correct input format for FD analysis

abun <- data.frame(allspp.cover[1])
abun <- cbind(abun, allspp.cover[2])
abun <- cbind(abun, allspp.cover[12])
abun <- cast(abun, plotID ~ species, value="relcover", fill=0)
rownames(abun) <- abun$plotID
abun$plotID <- NULL

spp <- traits$species 
traits$species <- NULL
rownames(traits) <- spp
rm(spp)

### run FD analysis ### important that traits are scaled (stand.x = TRUE)
### 12 groups ###

FD.dbfd <- dbFD(traits, 
                abun, 
                w.abun = TRUE, 
                stand.x = TRUE,
                ord = c("metric"),
                corr = c("cailliez"),
                calc.FGR = TRUE, 
                calc.FDiv = TRUE, 
                calc.CWM=TRUE, 
                clust.type="ward", 
                print.pco=TRUE, 
#                scale.RaoQ=TRUE, 
                stand.FRic=TRUE)

hydroplots <- hydro

hydroplots$FDis <- FD.dbfd$FDis
hydroplots$FDiv <- FD.dbfd$FDiv
hydroplots$FRic <- FD.dbfd$FRic
hydroplots$FEve <- FD.dbfd$FEve
hydroplots$RaoQ <- FD.dbfd$RaoQ
hydroplots$FGR <- FD.dbfd$FGR
hydroplots$nbsp <- FD.dbfd$nbsp

CWM <- FD.dbfd$CWM

hydroplots$SLA.CWM <- CWM$SLA
hydroplots$seedmass.CWM <- CWM$seedmass
hydroplots$maxheight.CWM <- CWM$maxheight
hydroplots$flowering.period.CWM <- CWM$flowering.period
hydroplots$WD.CWM <- CWM$WD


CWM$woody <- NULL
CWM$lifehistory <- NULL

#View(cor(CWM))
#pairs(CWM)

## try a Tukey's test to compare hydro categories ##

FDis.aov <- aov(hydroplots$FDis ~ as.factor(hydroplots$category))
TukeyHSD(FDis.aov)

## plot everything against hydro ##

plot.linear(hydroplots, hydroplots$FDis, FD)
plot.linear(hydroplots, hydroplots$FDiv, FD)
plot.linear(hydroplots, hydroplots$FRic, FD)
plot.linear(hydroplots, hydroplots$FEve, FD)
plot.linear(hydroplots, hydroplots$nbsp, FD)

plot.linear(hydroplots, hydroplots$SLA.CWM, CWM)
plot.linear(hydroplots,hydroplots$seedmass.CWM, CWM)
plot.linear(hydroplots, hydroplots$maxheight.CWM, CWM)
plot.linear(hydroplots, hydroplots$flowering.period.CWM, CWM)
plot.linear(hydroplots, hydroplots$WD.CWM, CWM)


plot.quad(hydroplots, hydroplots$FDis, FD)
plot.quad(hydroplots, hydroplots$FDiv, FD)
plot.quad(hydroplots,hydroplots$FRic, FD)
plot.quad(hydroplots, hydroplots$FEve, FD)
plot.quad(hydroplots, hydroplots$nbsp, FD)

plot.quad(hydroplots, hydroplots$SLA.CWM, CWM)
plot.quad(hydroplots,hydroplots$seedmass.CWM, CWM)
plot.quad(hydroplots, hydroplots$maxheight.CWM, CWM)
plot.quad(hydroplots, hydroplots$flowering.period.CWM, CWM)
plot.quad(hydroplots, hydroplots$WD.CWM, CWM)



getStats(hydroplots, hydroplots$FDis, FD)
getStats(hydroplots, hydroplots$FDiv, FD)
getStats(hydroplots, hydroplots$FRic, FD)
getStats(hydroplots, hydroplots$FEve, FD)
getStats(hydroplots, hydroplots$nbsp, FD)

getStats(hydroplots, hydroplots$SLA.CWM, CWM)
getStats(hydroplots,hydroplots$seedmass.CWM, CWM)
getStats(hydroplots, hydroplots$maxheight.CWM, CWM)
getStats(hydroplots, hydroplots$flowering.period.CWM, CWM)
getStats(hydroplots, hydroplots$WD.CWM, CWM)



