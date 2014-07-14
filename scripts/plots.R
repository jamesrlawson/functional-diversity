hydro.FDis.signif <- data.frame(cbind(hydroplots["FDis"],
                                      hydroplots["MDFMDFSpring"],
                                      hydroplots["P_MaxM"],
                                      hydroplots["CVAnnMRateRise"],
                                      hydroplots["CVMDFWinter"],
                                      hydroplots["CVAnnHSPeak"],
                                      hydroplots["MDFMDFSummer"]))

plot.linear(hydro.FDis.signif, hydro.FDis.signif$FDis, FDis)
plot.quad(hydro.FDis.signif, hydro.FDis.signif$FDis, FDis)


FDis.signif <- data.frame(cbind(
  "CVMDFWinter" = hydro$CVMDFWinter,
  "MDFMDFSummer" = hydro$MDFMDFSummer,
  "MDFMDFSpring" = hydro$MDFMDFSpring,
  "P_MaxM" = hydro$P_MaxM,
  "CVAnnMRateRise"= hydro$CVAnnMRateRise,
  "CVAnnHSPeak"= hydro$CVAnnHSPeak))

FDis.signif.cor <- cor(FDis.signif)
View(FDis.signif.cor )
pairs(FDis.signif)

FDis.signif.PCA <- prcomp(FDis.signif, scale=TRUE, centre=TRUE, retx=TRUE)
summary(FDis.signif.PCA)
plot(FDis.signif.PCA)
biplot(FDis.signif.PCA)