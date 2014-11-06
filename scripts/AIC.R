require(car)

## Model selection procedure ##

# screen for variables that are individually significant
# build models based on description of disturbance magnitude, disturbance frequency, seasonality of flows
# PCA shows three axes worth checking out. 
# For PC1 there was no clear differentiation in eigenvalues, so used metric with highest individual R2 values (CVAnnHSPeak):
# PC2 pointed to MDFMDFSummer and PC3 pointed to CVAnnHSNum as further sources of variability.
# 
# models were checked for multicollinearity according to variance inflation (VIF) and excluded where this was an issue
# models were then compared according to AIC scores

########### PCA ###############

# note LSPeak and CVAnnHSNum are included even though BH adjustment rendered them non-significant at alpha 0.05 #

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


########### models ############

## test every combination of CVAnnHSPeak, CVAnnHSNum and MDFMDFSummer 

# first centre variables 

hydroplots$CVAnnHSNum.centred <- hydroplots$CVAnnHSNum - mean(hydroplots$CVAnnHSNum)
hydroplots$CVAnnHSPeak.centred <- hydroplots$CVAnnHSPeak - mean(hydroplots$CVAnnHSPeak)
hydroplots$MDFMDFSummer.centred <- hydroplots$MDFMDFSummer - mean(hydroplots$MDFMDFSummer)

model1 <- lm(FDis ~ CVAnnHSNum.centred, data = hydroplots)
model2 <- lm(FDis ~ CVAnnHSPeak.centred, data = hydroplots)
model3 <- lm(FDis ~ MDFMDFSummer.centred, data = hydroplots)

model4 <- lm(FDis ~ CVAnnHSNum.centred + CVAnnHSPeak.centred, data = hydroplots)
model5 <- lm(FDis ~ CVAnnHSNum.centred + MDFMDFSummer.centred, data = hydroplots)
model6 <- lm(FDis ~ CVAnnHSPeak.centred + MDFMDFSummer.centred, data = hydroplots)
model7 <- lm(FDis ~ CVAnnHSNum.centred * CVAnnHSPeak.centred, data = hydroplots)
model8 <- lm(FDis ~ CVAnnHSNum.centred * MDFMDFSummer.centred, data = hydroplots)
model9 <- lm(FDis ~ CVAnnHSPeak.centred + MDFMDFSummer.centred, data = hydroplots)

model10 <- lm(FDis ~ CVAnnHSNum.centred + CVAnnHSPeak.centred + MDFMDFSummer.centred, data = hydroplots)
model11 <- lm(FDis ~ CVAnnHSNum.centred * CVAnnHSPeak.centred + MDFMDFSummer.centred, data = hydroplots)
model12 <- lm(FDis ~ CVAnnHSNum.centred + CVAnnHSPeak.centred * MDFMDFSummer.centred, data = hydroplots)
model13 <- lm(FDis ~ CVAnnHSNum.centred * CVAnnHSPeak.centred * MDFMDFSummer.centred, data = hydroplots)

AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12, model13)

# exclude according to VIFs #

vif(model4)
vif(model5)
vif(model6)
vif(model7) # getting dodgy (>4.5)
vif(model8)
vif(model9)
vif(model10)
vif(model11) # dodgy
vif(model12)
vif(model13) # way high

AIC(model1, model2, model3, model4, model5, model6, model8, model9, model10, model12)

# model 12 wins! lm(FDis ~ CVAnnHSNum.centred + CVAnnHSPeak.centred * MDFMDFSummer.centred, data = hydroplots)

