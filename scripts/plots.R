require(plyr)
require(reshape)
require(multtest)

## summary hydro stats ##

hydro.summary <- melt(hydro)
hydro.summary <- ddply(hydro.summary, .(variable), summarize, 
                       min = min(value), 
                       max=max(value),
                       mean=mean(value),
                       sd = sd(value))
hydro.summary[2:5] <- round(hydro.summary[2:5], 2)

## summary trait stats ##
# do this before transforming #

traits.summary <- melt(traits)
traits.summary <- ddply(traits.summary, .(variable), summarize, 
                       min = min(value, na.rm=TRUE), 
                       max=max(value, na.rm=TRUE),
                       mean=mean(value, na.rm=TRUE),
                       sd = sd(value, na.rm=TRUE))
traits.summary[2:5] <- round(traits.summary[2:5], 2)
traits.summary <- traits.summary[-4,]

## trait PCAs ##


traits.woody <- na.omit(traits)
traits.woody.PCA <- prcomp(traits.woody, scale=TRUE, centre=TRUE, retx=TRUE)
summary(traits.woody.PCA)
plot(traits.woody.PCA)
traits.woody.PCA$rotatio[,1:6]


traits.minimal <- traits[,-6]
traits.minimal <- traits.minimal[,-5]
traits.minimal <- na.omit(traits.minimal)
traits.minimal.PCA <- prcomp(traits.minimal, scale=TRUE, centre=TRUE, retx=TRUE)
summary(traits.minimal.PCA)
plot(traits.minimal.PCA)
traits.minimal.PCA$rotatio[,1:4]

## CWM PCA ##

CWM.PCA <- prcomp(CWM, scale=TRUE, centre=TRUE, retx=TRUE)
summary(CWM.PCA)
biplot(CWM.PCA)
CWM.PCA$rotatio[,1:4] # loadings

## univariate regression plots ##

plot.linear(hydroplots, hydroplots$FDis, FD)

## get stats tables for all FDis tests ##

FDis.tests <- getAllStats(hydroplots, hydroplots$FDis, FD)
FDis.tests <- FDis.tests[c(13:35),] # remove incidental variables or p.adjust breaks - run this straight after generating hydroplots in the analysis file 
  
write.csv(FDis.tests, "output/stats/FDis-all.csv")

# M_MinM and CVMDFSummer are best described by a quadratic fit, so we need to put 
# their p values into the vector of p values to be adjusted
# my function gives incorrect p values for quadratic fits, for some reason. have to run the regressions individually. 

M_MinM.quad <- lm(FDis ~ M_MinM + I(M_MinM^2), data = hydroplots)
CVMDFSummer.quad <- lm(FDis ~ CVMDFSummer + I(CVMDFSummer^2), data = hydroplots)

summary(M_MinM.quad)
summary(CVMDFSummer.quad)

p.vec <- c(0.009625127,
           0.034225202,
           0.02176, # this is the p val for the quadratic fit for CVMDFSummer
           0.025969223,
           0.088081735,
           0.109117651,
           0.003060679,
           0.013393695,
           0.025804779,
           0.088452064,
           0.009421, # this is the p val for the quadratic fit for M_MinM
           0.136126122,
           0.020857362,
           0.108604162,
           0.014833246,
           0.012875004,
           0.010989153,
           0.157241392,
           0.155629385,
           0.001014303,
           0.036001867,
           0.726998403,
           0.064823375)
           

padjs  <- mt.rawp2adjp(p.vec, c("TSBH"))
padjs  <- data.frame(padjs$adjp[order(padjs$index),])
FDis.tests$p.adj <- padjs$TSBH_0.05

## analyse global variables ##

hydroplots1$FDisp <- FD.dbfd$FDis

hydro1 <- read.csv("data/hydro1.csv", header=TRUE)
hydroplots$latinv <- hydro1$latinv
hydroplots$catchment <- hydro1$catchment
hydroplots$elevation <- hydro1$elevation

FDis_lat.lm <- lm(FDis ~ latinv, data = hydroplots)
FDis_catchment.lm <- lm(FDis ~ catchment, data = hydroplots)
FDis_elevation.lm <- lm(FDis ~ elevation, data = hydroplots)

## make regression plots ##

plot.linear(hydroplots, hydroplots$FDis, FD)
plot.quad(hydroplots, hydroplots$FDis, FD)


## significant hydro metrics PCA ##

hydro.FDis.signif <- data.frame(cbind(hydroplots["CVMDFWinter"],
                                      hydroplots["CVMDFAutumn"],
                                      hydroplots["CVMDFSpring"],
                                      hydroplots["MDFMDFSummer"],
                                      hydroplots["MDFMDFSpring"],
                                      hydroplots["M_MaxM"],
                                      hydroplots["M_MinM"],
                                      hydroplots["M_MDFM"],
                                      hydroplots["AS20YrARI"],
                                      hydroplots["CVAnnMRateFall"],
                                      hydroplots["CVAnnMRateRise"],
                                      hydroplots["CVAnnHSPeak"],
                                      hydroplots["CVAnnHSNum"]
                                      ))


FDis.signif.cor <- cor(hydro.FDis.signif)
View(FDis.signif.cor )
pairs(hydro.FDis.signif)

FDis.signif.PCA <- prcomp(hydro.FDis.signif, scale=TRUE, centre=TRUE, retx=TRUE)
summary(FDis.signif.PCA)
plot(FDis.signif.PCA)
biplot(FDis.signif.PCA)

FDis.signif.PCA$rotatio[,1:3] # loadings


FDis.signif.PCA$x[,1]


## CWM PCA ##


CWM.PCA <- prcomp(CWM, scale=TRUE, centre=TRUE, retx=TRUE)
summary(CWM.PCA)
CWM.PCA$rotatio[,1:4] # loadings
biplot(CWM.PCA)









# pco #

blah.gowdis <- gowdis(CWM)
blah <- capscale(blah.gowdis ~ 1)
plot(blah)
summary(blah)
blah

eig <- eigenvals(blah)
cumsum(eig / sum(eig))
eig / sum(eig)
