require(plyr)
require(reshape)

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
FDis.tests <- FDis.tests[c(17:39),] # remove incidental variables or p.adjust breaks
FDis.tests$padj.linear <- p.adjust(FDis.tests$pval.linear, method="BH")
FDis.tests$padj.quad <- p.adjust(FDis.tests$pval.quad, method="BH")
write.csv(FDis.tests, "output/stats/FDis-all.csv")

# M_MinM is the only relationship described by a quadratic fit, so we need to put 
# its p value into the vector of linear p values, otherwise we get an incorrect adjustment

p.vec <- c(0.001014303,
           0.003060679,
           0.009625127,
           0.013393695,
           0.014833246,
           0.012875004,
           0.010989153,
           0.025969223,
           0.025804779,
           0.012047481, # this is the p val for the quadratic fit for M_MinM
           0.020857362,
           0.034225202,
           0.036001867,
           0.064823375,
           0.088081735,
           0.088452064,
           0.109117651,
           0.108604162,
           0.136126122,
           0.157241392,
           0.155629385,
           0.286664786,
           0.726998403)

p.adjust(p.vec, "BH")[10]


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
