## SPATIAL etc. AUTOCORRELATION TESTS ##

require(ade4)
require(ape)
require(FD)
require(vegan)
require(MuMIn)
options(na.action = "na.fail")

env <- read.csv("data/spatial_clim_soil.csv", header=TRUE)

everything <- cbind(hydroplots, env)

# hydro metrics vs geographic position #

spatial <- data.frame(everything["lat"],everything["long"])

spatial.gowdis <- gowdis(spatial)
hydro.gowdis <- gowdis(hydroplots[,2:37])

mantel.rtest(spatial.gowdis, hydro.gowdis, nrepet=9999)

# hydro metrics vs soil #

soil.gowdis <- gowdis(everything[,79:90])
mantel.rtest(soil.gowdis, hydro.gowdis, nrepet=9999)

# hydro metrics vs climate #

clim.gowdis <- gowdis(everything[,60:78])
mantel.rtest(clim.gowdis, hydro.gowdis, nrepet=9999)


# for FDis #

spatial.gowdis.inv <- as.matrix(1/spatial.gowdis)
diag(spatial.gowdis.inv) <- 0
Moran.I(hydroplots$FDis, spatial.gowdis.inv)

soil.gowdis.inv <- as.matrix(1/soil.gowdis)
diag(soil.gowdis.inv) <- 0
Moran.I(hydroplots$FDis, soil.gowdis.inv)

clim.gowdis.inv <- as.matrix(1/clim.gowdis)
diag(clim.gowdis.inv) <- 0
Moran.I(hydroplots$FDis, clim.gowdis.inv)

hydro.gowdis.inv <- as.matrix(1/hydro.gowdis)
diag(hydro.gowdis.inv) <- 0
Moran.I(hydroplots$FDis, hydro.gowdis.inv)


# variance partitioning

clim.lm <- lm(FDis ~ clim_pdmt + I(clim_pdmt^2) + clim_pcld + I(clim_pcld^2) + clim_isot + I(clim_isot^2) + clim_twet, data = everything)
clim.lm.dredge <- dredge(clim.lm, trace=TRUE, m.max = 5)
clim.lm.dredge

clim.lm.int <- lm(FDis ~ clim_pdmt * clim_pcld * clim_isot * clim_twet, data = everything)
clim.lm.int.dredge <- dredge(clim.lm.int, trace=TRUE, m.max=8)
clim.lm.int.dredge

FDis.varpart <- varpart(everything$FDis,
                        ~CVAnnHSNum + CVAnnHSPeak * MDFMDFSummer,
                        ~clim_isot + I(clim_isot^2),
                 #       ~soil_ece + I(soil_ece^2),
                        data = everything)
FDis.varpart
plot(FDis.varpart)



hydro.pca <- prcomp(hydro[,2:24], scale=TRUE, center=TRUE, retx=TRUE)
summary(hydro.pca)
hydro.pca$rotation[,1:5]
write.csv(hydro.pca$rotation[,1:5], "output/hydro_pca.csv")
 