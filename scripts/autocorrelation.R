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

clim.lm <- clim_pdmt + I(clim_pdmt^2) + clim_pcld + I(clim_pcld^2) + clim_isot + I(clim_isot^2) + clim_twet, everything)
clim.lm.dredge <- dredge(clim.lm, trace=TRUE)
clim.lm.dredge

FDis.varpart <- varpart(everything$FDis,
                        ~clim_isot + I(clim_isot^2),
                        ~CVAnnHSNum + CVAnnHSPeak * MDFMDFSummer,
         #               ~soil_ece + I(soil_ece^2),
                        data = everything)
FDis.varpart
plot(FDis.varpart)

