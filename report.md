---
title: "FD results"
author: "James Lawson"
date: "Monday, July 14, 2014"
output: html_document
---

Functional Diversity of riparian plant communities of south eastern Australia 
========================================================

This document presents the results of a functional diversity analysis using the following traits:

maxheight
SLA
seedmass
wood density
flowering period (proportion of year)
leaf length:width ratio
(leaf length)

Species by species trait correlations
--------------------------------------
Missing wood density cors due to missing values


```r
traits.naomit <- na.omit(traits)
cor(traits.naomit)
```

```
##                    maxheight seedmass     SLA flowering.period
## maxheight            1.00000  0.15627 -0.3701         -0.19589
## seedmass             0.15627  1.00000  0.1165          0.16079
## SLA                 -0.37009  0.11652  1.0000          0.15174
## flowering.period    -0.19589  0.16079  0.1517          1.00000
## length.width.ratio   0.19285 -0.22689 -0.5312         -0.11641
## WD                  -0.08565  0.04015 -0.4354          0.03592
##                    length.width.ratio       WD
## maxheight                     0.19285 -0.08565
## seedmass                     -0.22689  0.04015
## SLA                          -0.53118 -0.43544
## flowering.period             -0.11641  0.03592
## length.width.ratio            1.00000  0.07723
## WD                            0.07723  1.00000
```

```r
pairs(traits.naomit)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


PCA of trait data 
--------------------------------------

```r
traits.PCA <- prcomp(traits.all, centre=TRUE, retx=TRUE)
summary(traits.PCA)
```

```
## Importance of components:
##                          PC1   PC2    PC3    PC4    PC5    PC6
## Standard deviation     0.898 0.716 0.3863 0.2811 0.1996 0.1859
## Proportion of Variance 0.497 0.316 0.0921 0.0488 0.0246 0.0213
## Cumulative Proportion  0.497 0.813 0.9053 0.9541 0.9787 1.0000
```

```r
biplot(traits.PCA)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


Trait correlations by CWM
--------------------------------------


```r
print(cor(CWM))
```

```
##                    maxheight seedmass     SLA flowering.period
## maxheight            1.00000   0.5103 -0.4792         -0.62103
## seedmass             0.51033   1.0000  0.1357         -0.13660
## SLA                 -0.47916   0.1357  1.0000          0.56184
## flowering.period    -0.62103  -0.1366  0.5618          1.00000
## length.width.ratio  -0.17268  -0.3442 -0.4065          0.32308
## WD                  -0.09881   0.0384 -0.3615          0.05575
##                    length.width.ratio       WD
## maxheight                     -0.1727 -0.09881
## seedmass                      -0.3442  0.03840
## SLA                           -0.4065 -0.36149
## flowering.period               0.3231  0.05575
## length.width.ratio             1.0000  0.09990
## WD                             0.0999  1.00000
```

```r
pairs(CWM)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


FDis significant regressions
--------------------------------------

```r
getStats(hydroplots, hydroplots$FDis)
```

```
## Warning: ANOVA F-tests on an essentially perfect fit are unreliable
## Warning: ANOVA F-tests on an essentially perfect fit are unreliable
## Warning: cannot open file 'C:/Users/JLawson/Desktop/stuff/data/analysis/R/functional diversity/output/stats/hydroplots$FDis_stats.csv': Permission denied
```

```
## Error: cannot open the connection
```

PCA over significant hydrological variables
-------------------------------------------

```r
FDis.signif <- data.frame(cbind(
#                                hydroplots["CVMDFWinter"],
#                                hydroplots["CVMDFAutumn"],
 #                               hydroplots["CVMDFSpring"],
#                                hydroplots["MDFMDFSummer"],
#                                hydroplots["MDFMDFSpring"],
                                hydroplots["M_MaxM"],
                                hydroplots["P_MaxM"],
                                hydroplots["M_MinM"],
                                hydroplots["M_MDFM"],
                                hydroplots["P_MDFM"],
                                hydroplots["AS20YrARI"],
                                hydroplots["CVAnnMRateFall"],
                                hydroplots["CVAnnMRateRise"],
                                hydroplots["LSPeak"],
                                hydroplots["CVAnnHSPeak"]
))

                                                    
FDis.signif.cor <- cor(FDis.signif)
print(FDis.signif.cor)
```

```
##                 M_MaxM  P_MaxM  M_MinM  M_MDFM  P_MDFM AS20YrARI
## M_MaxM          1.0000  0.5744  0.8418  0.9497  0.9681   -0.9087
## P_MaxM          0.5744  1.0000  0.3735  0.5327  0.6033   -0.5818
## M_MinM          0.8418  0.3735  1.0000  0.9360  0.7883   -0.7486
## M_MDFM          0.9497  0.5327  0.9360  1.0000  0.9179   -0.8935
## P_MDFM          0.9681  0.6033  0.7883  0.9179  1.0000   -0.9102
## AS20YrARI      -0.9087 -0.5818 -0.7486 -0.8935 -0.9102    1.0000
## CVAnnMRateFall -0.9092 -0.6552 -0.7998 -0.9048 -0.9299    0.8312
## CVAnnMRateRise -0.8557 -0.8006 -0.6361 -0.7944 -0.8649    0.7632
## LSPeak          0.9148  0.5972  0.6792  0.8161  0.9572   -0.8958
## CVAnnHSPeak    -0.7194 -0.7642 -0.6587 -0.7124 -0.6564    0.7415
##                CVAnnMRateFall CVAnnMRateRise  LSPeak CVAnnHSPeak
## M_MaxM                -0.9092        -0.8557  0.9148     -0.7194
## P_MaxM                -0.6552        -0.8006  0.5972     -0.7642
## M_MinM                -0.7998        -0.6361  0.6792     -0.6587
## M_MDFM                -0.9048        -0.7944  0.8161     -0.7124
## P_MDFM                -0.9299        -0.8649  0.9572     -0.6564
## AS20YrARI              0.8312         0.7632 -0.8958      0.7415
## CVAnnMRateFall         1.0000         0.9062 -0.8792      0.5976
## CVAnnMRateRise         0.9062         1.0000 -0.8311      0.6665
## LSPeak                -0.8792        -0.8311  1.0000     -0.6179
## CVAnnHSPeak            0.5976         0.6665 -0.6179      1.0000
```

```r
FDis.signif.PCA <- prcomp(FDis.signif, scale=TRUE, centre=TRUE, retx=TRUE)
summary(FDis.signif.PCA)
```

```
## Importance of components:
##                          PC1    PC2    PC3    PC4     PC5     PC6     PC7
## Standard deviation     2.835 0.9344 0.7323 0.5520 0.30459 0.27716 0.19428
## Proportion of Variance 0.804 0.0873 0.0536 0.0305 0.00928 0.00768 0.00377
## Cumulative Proportion  0.804 0.8913 0.9449 0.9754 0.98464 0.99232 0.99609
##                            PC8     PC9    PC10
## Standard deviation     0.14905 0.11304 0.06389
## Proportion of Variance 0.00222 0.00128 0.00041
## Cumulative Proportion  0.99831 0.99959 1.00000
```

```r
plot(FDis.signif.PCA)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-51.png) 

```r
biplot(FDis.signif.PCA)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-52.png) 

