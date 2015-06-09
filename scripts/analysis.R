

source("scripts/functions.R")
options(stringsAsFactors = FALSE)
library(plyr)
library(ggplot2)
library(reshape)
library(FD)
library(SYNCSA)

#############################################################################################
############################# GET RELATIVE ABUNDANCES FOR ALL TRAITS ########################
#############################################################################################

# load in data

alltraits <- read.csv("data/alltraits.csv", header=TRUE)
percentcover <- read.csv("data/percentcover.csv", header=TRUE)
hydro <- read.csv("data/hydro.csv", header=TRUE)
leafDimensions <- read.csv("data/leafDimensions.csv", header=TRUE, stringsAsFactors = FALSE)
WD <- read.csv("data/WD.csv")
categories <- read.csv("data/categories.csv", header=TRUE)

# remove observations that average less than 1% cover
percentcover <- subset(percentcover, avgcover > 1)

# generate df with total % cover (across all strata) of species at each plot
plotsums <- ddply(percentcover, .(plotID, species), summarise, speciescover = sum(avgcover))
plotcover <- ddply(percentcover, .(plotID), summarise, plotCover = sum(avgcover))

# remove data poor species

allspp <- subset(alltraits, growthform != "F") # remove ferns

# separate out ferns so we can use this in the data density analysis below
ferns <- subset(alltraits, growthform == "F")
# remove fern spp. we're going to add back in to the dataset

ferns <- ferns[-17,] # Pteridium esculentum
#ferns <- ferns[-15,] # Pellaea falcata - adding causes NA's in FD distance matrix
ferns <- ferns[-10,] # Doodia aspera
ferns <- ferns[-8,] # Calochlaena dubia
ferns <- ferns[-5,] # Blechnum nudum
#ferns <- ferns[-1,] # Adiantum aethiopicum - adding causes NA's in FD distance matrix

allspp <- na.omit(allspp)

allspp <- rbind(allspp, alltraits[2,]) # Acacia boormanii
allspp <- rbind(allspp, alltraits[130,]) # Eucalyptus resinifera
allspp <- rbind(allspp, alltraits[135,]) # Eustrephus latifolius
allspp <- rbind(allspp, alltraits[176,]) # Hovea asperifolia subsp. asperifolia
allspp <- rbind(allspp, alltraits[204,]) # Lomandra hystrix
allspp <- rbind(allspp, alltraits[223,]) # Notelaea microcarpa subsp. microcarpa
allspp <- rbind(allspp, alltraits[318,]) # Stephania japonica
allspp <- rbind(allspp, alltraits[348,]) # Waterhousea floribunda

#allspp <- rbind(allspp, alltraits[18,]) # Adiantum aethiopicum
allspp <- rbind(allspp, alltraits[42,]) # Blechnum nudum
allspp <- rbind(allspp, alltraits[53,]) # Calochlaena dubia
allspp <- rbind(allspp, alltraits[112,]) # Doodia aspera
allspp <- rbind(allspp, alltraits[273,]) # Pteridium esculentum
#allspp <- rbind(allspp, alltraits[239,]) # Pellaea falcata

                                                
allspp <- allspp[order(allspp$species), ]

# add wood density and leaf dimension data

allspp <- merge(allspp, leafDimensions, all.x=TRUE)
allspp <- merge(allspp, WD, all.x=TRUE)


## add calculated leaf area ##

allspp$leafarea <- allspp$leafLength.mean * allspp$leafWidth.mean * 0.7

##

allspp.cover <- findtraitvals(plotsums, allspp)
allspp.cover <- merge(allspp.cover, plotcover)

### data density ###

allspp.traitcover <- ddply(allspp.cover, .(plotID), summarise, traitcover = sum(speciescover))
allspp.cover <- merge(allspp.cover, allspp.traitcover, by.x = "plotID", by.y = "plotID")
allspp.cover <- relabund(allspp.cover)

ferns.cover <- merge(ferns, percentcover, by.x = "species", by.y = "species")
ferns.totalcover <- ddply(ferns.cover, .(plotID), summarise, totalferncover = sum(avgcover))

dataDensity <- data.frame(cbind("plotID" = unique(allspp.cover$plotID), 
                                "traitcover" = unique(allspp.cover$traitcover),
                                "totalferncover" = c(1.2,
                                                     0,
                                                     0,
                                                     1.4,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     2.6,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0), # values from ferns.totalcover
                                "plotCover" = unique(allspp.cover$plotCover)))
dataDensity$coverage <- dataDensity$traitcover / dataDensity$plotCover
dataDensity$coverage.withferns <- (dataDensity$traitcover + dataDensity$totalferncover) / (dataDensity$plotCover)


dataDensity$WD = ddply(allspp.cover, .(plotID), summarise, WD = length(na.omit(WD)) / length(WD))$WD
dataDensity$maxheight = ddply(allspp.cover, .(plotID), summarise, maxheight = length(na.omit(maxheight)) / length(maxheight))$maxheight
dataDensity$seedmass = ddply(allspp.cover, .(plotID), summarise, seedmass = length(na.omit(seedmass)) / length(seedmass))$seedmass
dataDensity$SLA = ddply(allspp.cover, .(plotID), summarise, SLA = length(na.omit(SLA)) / length(SLA))$SLA
dataDensity$flowering.period = ddply(allspp.cover, .(plotID), summarise, flowering.period = length(na.omit(flowering.period)) / length(flowering.period))$flowering.period
dataDensity$length.width.ratio = ddply(allspp.cover, .(plotID), summarise, length.width.ratio = length(na.omit(length.width.ratio)) / length(length.width.ratio))$length.width.ratio

dataDensity$traitcover <- NULL
dataDensity$totalferncover <- NULL
dataDensity$plotCover <- NULL
dataDensity$coverage.withferns <- NULL

dataDensity <- format(dataDensity, digits = 3)

#

test <- ddply(allspp.cover, .(plotID), summarise, summedRelCover = (sum(relcover)))
print(test)
rm(test)



traits <- allspp.cover[2]
traits <- cbind(traits, allspp.cover[4:15])
traits$leafLength.mean <- NULL
traits$leafWidth.mean <- NULL
traits$leafarea <- NULL

traits$growthform <- NULL
traits$lifehistory <- NULL
traits <- ddply(traits, .(species), unique)

# transform traits

hist(log10(traits$seedmass))
hist(log10(traits$SLA))
hist(log10(traits$maxheight), breaks = 5)
hist((traits$flowering.period)) # non-normal distribution with any common transform
hist(log10(traits$WD))

traits$seedmass <- log10(traits$seedmass)
traits$SLA <- log10(traits$SLA)
traits$maxheight <- log10(traits$maxheight)
traits$length.width.ratio <- log10(traits$length.width.ratio)
traits$WD <- log10(traits$WD)

traits$woody <- as.numeric(traits$woody)
traits$woody <- NULL

# create abun, in correct input format for FD analysis

abun <- data.frame(allspp.cover[1])
abun <- cbind(abun, allspp.cover[2])
abun <- cbind(abun, allspp.cover[18])
abun <- cast(abun, plotID ~ species, value="relcover", fill=0)
rownames(abun) <- abun$plotID
abun$plotID <- NULL

spp <- traits$species 
traits$species <- NULL
rownames(traits) <- spp
rm(spp)


### run FD analysis ### important that traits are scaled (stand.x = TRUE)

FD.dbfd <- dbFD(traits, 
                abun,
                w.abun = TRUE,  # use presence - absence converted data?
                stand.x = FALSE,
                corr = c("cailliez"),
#                calc.FGR = TRUE, 
#                clust.type = c("kmeans"),
#                km.inf.gr = c(2),
#                km.sup.gr = c(10),
#                km.iter = (100),
#                calc.FDiv = TRUE, 
#                calc.FRic = TRUE,
                m = "max",
                calc.CWM=TRUE, 
                print.pco=TRUE, 
#                scale.RaoQ=TRUE, 
 #               stand.FRic=TRUE
                )

## run SYNCSA analysis for functional redundancy and Simpson diversity ##

# to use presabs only
abun.presabs <- abun
abun.presabs[abun.presabs>0] <- 1

FD.redun_presabs <- rao.diversity(abun.presabs, traits=traits)

FD.redun <- rao.diversity(abun, traits=traits)
##

hydroplots <- hydro
catname <- as.factor(categories$cats)

hydroplots$FDis <- FD.dbfd$FDis
hydroplots$FDiv <- FD.dbfd$FDiv
hydroplots$FRic <- FD.dbfd$FRic
hydroplots$FEve <- FD.dbfd$FEve
hydroplots$RaoQ <- FD.dbfd$RaoQ
hydroplots$FGR <- FD.dbfd$FGR
hydroplots$nbsp <- FD.dbfd$nbsp
hydroplots$simpson <- FD.redun$Simpson
hydroplots$FunRao <- FD.redun$FunRao
hydroplots$redun <- FD.redun$FunRedundancy

CWM <- FD.dbfd$CWM

hydroplots$SLA.CWM <- CWM$SLA
hydroplots$seedmass.CWM <- CWM$seedmass
hydroplots$maxheight.CWM <- CWM$maxheight
hydroplots$flowering.period.CWM <- CWM$flowering.period
hydroplots$WD.CWM <- CWM$WD
hydroplots$leafratio.CWM <- CWM$length.width.ratio

CWM$woody <- NULL
CWM$lifehistory <- NULL

View(cor(CWM))
pairs(CWM)

## try a Tukey's test to compare hydro categories ##

FDis.aov <- aov(hydroplots$FDis ~ as.factor(categories$cats))
TukeyHSD(FDis.aov)

## add total species richness information ##
## presence / absence data for all species at each site including rare spp. ##

diversity <- read.csv("data/diversity.csv", header=TRUE, stringsAsFactors = FALSE)
div_hydro <- merge(diversity, hydro)
hydroplots$richness <- tapply(div_hydro$species, div_hydro$plot, length)


# get stats tables for all FDis tests

FDis.tests <- getAllStats(hydroplots, hydroplots$FDis, FD)
FDis.tests <- FDis.tests[c(17:39),] # remove incidental variables or p.adjust breaks
FDis.tests$padj.linear <- p.adjust(FDis.tests$pval.linear, method="BH")
FDis.tests$padj.quad <- p.adjust(FDis.tests$pval.quad, method="BH")
write.csv(FDis.tests, "output/stats/FDis-all.csv")

