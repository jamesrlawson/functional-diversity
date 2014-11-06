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
                                      hydroplots["LSPeak"],
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




