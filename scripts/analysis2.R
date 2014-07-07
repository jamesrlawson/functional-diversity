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
hydro <- read.csv("data/hydronorm_all.csv", header=TRUE)
leafDimensions <- read.csv("data/leafDimensions.csv", header=TRUE, stringsAsFactors = FALSE)
WD <- read.csv("data/WD.csv")

# remove observations that average less than 1% cover
percentcover <- subset(percentcover, avgcover > 1)

# generate df with total % cover (across all strata) of species at each plot
plotsums <- ddply(percentcover, .(plotID, species), summarise, speciescover = sum(avgcover))
plotcover <- ddply(percentcover, .(plotID), summarise, plotCover = sum(avgcover))

# remove data poor species

allspp <- subset(alltraits, growthform != "F") # remove ferns
ferns <- subset(alltraits, growthform == "F")
allspp <- na.omit(allspp)

allspp <- rbind(allspp, alltraits[2,]) # Acacia boormanii
allspp <- rbind(allspp, alltraits[130,]) # Eucalyptus resinifera
#allspp <- rbind(allspp, alltraits[135,]) # Eustrephus latifolius
allspp <- rbind(allspp, alltraits[176,]) # Hovea asperifolia subsp. asperifolia
allspp <- rbind(allspp, alltraits[204,]) # Lomandra hystrix
#allspp <- rbind(allspp, alltraits[223,]) # Notelaea microcarpa subsp. microcarpa
#allspp <- rbind(allspp, alltraits[318,]) # Stephania japonica
allspp <- rbind(allspp, alltraits[348,]) # Waterhousea floribunda
allspp <- allspp[order(allspp$species), ]

# add wood density and leaf dimension data

allspp <- merge(allspp, leafDimensions, all.x=TRUE)
allspp <- merge(allspp, WD, all.x=TRUE)

allspp.cover <- findtraitvals(plotsums, allspp)
allspp.cover <- merge(allspp.cover, plotcover)

### get relative abundance ###

allspp.traitcover <- ddply(allspp.cover, .(plotID), summarise, traitcover = sum(speciescover))
allspp.cover <- merge(allspp.cover, allspp.traitcover, by.x = "plotID", by.y = "plotID")
allspp.cover <- relabund(allspp.cover)

ferns.cover <- merge(ferns, percentcover, by.x = "species", by.y = "species")
ferns.totalcover <- ddply(ferns.cover, .(plotID), summarise, totalferncover = sum(avgcover))

dataDensity <- data.frame(cbind("plotID" = unique(allspp.cover$plotID), 
                                "traitcover" = unique(allspp.cover$traitcover),
                                "totalferncover" = c(10.8,
                                                     38.8,
                                                     41.2,
                                                     16.2,
                                                     0,
                                                     0,
                                                     0,
                                                     36.0,
                                                     15.5,
                                                     7.0,
                                                     0,
                                                     2.9,
                                                     33.4,
                                                     0,
                                                     6.5), # values from ferns.totalcover
                                "plotCover" = unique(allspp.cover$plotCover)))
dataDensity$dataDensity <- dataDensity$traitcover / dataDensity$plotCover
dataDensity$dataDensity.withferns <- (dataDensity$traitcover + dataDensity$totalferncover) / (dataDensity$plotCover)
print(dataDensity)

test <- ddply(allspp.cover, .(plotID), summarise, summedRelCover = (sum(relcover)))
print(test)
rm(test)

#######

traits <- allspp.cover[2]
traits <- cbind(traits, allspp.cover[4:14])
traits$leafLength.mean <- NULL
traits$leafWidth.mean <- NULL
#traits$length.width.ratio <- NULL

traits$growthform <- NULL
traits$lifehistory <- NULL
traits <- ddply(traits, .(species), unique)

# transform traits

hist(log10(traits$seedmass))
hist(log10(traits$SLA))
hist(log10(traits$maxheight), breaks = 5)
hist((traits$flowering.period)) # non-normal distribution with any common transform
hist(log10(traits$leafWidth.mean))
hist(log10(traits$leafLength.mean))
hist(log10(traits$length.width.ratio))

traits$seedmass <- log10(traits$seedmass)
traits$SLA <- log10(traits$SLA)
traits$maxheight <- log10(traits$maxheight)
traits$leafWidth.mean <- log10(traits$leafWidth.mean)
traits$length.width.ratio <- log10(traits$length.width.ratio)
traits$leafLength.mean <- log10(traits$leafLength.mean)

traits$woody <- as.numeric(traits$woody)

# create abun, in correct input format for FD analysis

abun <- data.frame(allspp.cover[1])
abun <- cbind(abun, allspp.cover[2])
abun <- cbind(abun, allspp.cover[17])
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
#                calc.FGR = TRUE, 
                calc.FDiv = TRUE, 
                calc.FRic = TRUE,
                calc.CWM=TRUE, 
                clust.type="ward", 
                print.pco=TRUE, 
               #scale.RaoQ=TRUE, 
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
hydroplots$leafratio.CWM <- CWM$length.width.ratio
hydroplots$leafWidth.CWM <- CWM$leafWidth
hydroplots$leafLength.CWM <- CWM$leafLength

CWM$woody <- NULL
CWM$lifehistory <- NULL

View(cor(CWM))
pairs(CWM)

## try a Tukey's test to compare hydro categories ##

FDis.aov <- aov(hydroplots$FDis ~ as.factor(hydroplots$category))
TukeyHSD(FDis.aov)

## plot everything against hydro ##

#plot.linear(hydroplots, hydroplots$FDis, FD)
#plot.linear(hydroplots, hydroplots$FDiv, FD)
#plot.linear(hydroplots, hydroplots$FRic, FD)
#plot.linear(hydroplots, hydroplots$FEve, FD)
#plot.linear(hydroplots, hydroplots$nbsp, FD)

#plot.linear(hydroplots, hydroplots$SLA.CWM, CWM)
#plot.linear(hydroplots,hydroplots$seedmass.CWM, CWM)
#plot.linear(hydroplots, hydroplots$maxheight.CWM, CWM)
#plot.linear(hydroplots, hydroplots$flowering.period.CWM, CWM)
#plot.linear(hydroplots, hydroplots$WD.CWM, CWM)
#plot.linear(hydroplots,hydroplots$leafratio.CWM)
#plot.linear(hydroplots,hydroplots$leafWidth.CWM)




#plot.quad(hydroplots, hydroplots$FDis, FD)
#plot.quad(hydroplots, hydroplots$FDiv, FD)
#plot.quad(hydroplots,hydroplots$FRic, FD)
#plot.quad(hydroplots, hydroplots$FEve, FD)
#plot.quad(hydroplots, hydroplots$nbsp, FD)

#plot.quad(hydroplots, hydroplots$SLA.CWM, CWM)
#plot.quad(hydroplots,hydroplots$seedmass.CWM, CWM)
#plot.quad(hydroplots, hydroplots$maxheight.CWM, CWM)
#plot.quad(hydroplots, hydroplots$flowering.period.CWM, CWM)
#plot.quad(hydroplots, hydroplots$WD.CWM, CWM)
#plot.quad(hydroplots,hydroplots$leafratio.CWM)
#plot.quad(hydroplots,hydroplots$leafWidth.CWM)
#plot.quad(hydroplots,hydroplots$leafLength.CWM)




getStats(hydroplots, hydroplots$FDis, FD)
getStats(hydroplots, hydroplots$FDiv, FD)
getStats(hydroplots, hydroplots$FRic, FD)
getStats(hydroplots, hydroplots$FEve, FD)
getStats(hydroplots, hydroplots$nbsp, FD)
#getStats(hydroplots, hydroplots$FGR, FD)


getStats(hydroplots, hydroplots$SLA.CWM, CWM)
getStats(hydroplots, hydroplots$seedmass.CWM, CWM)
getStats(hydroplots, hydroplots$maxheight.CWM, CWM)
getStats(hydroplots, hydroplots$flowering.period.CWM, CWM)
getStats(hydroplots, hydroplots$WD.CWM, CWM)
getStats(hydroplots,hydroplots$leafratio.CWM)
#getStats(hydroplots,hydroplots$leafWidth.CWM)
#getStats(hydroplots,hydroplots$leafLength.CWM)


