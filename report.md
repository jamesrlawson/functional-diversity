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
FDis.signif <- data.frame(cbind(hydroplots["CVMDFWinter"],
                                hydroplots["CVMDFAutumn"],
                                hydroplots["CVMDFSpring"],
                                hydroplots["MDFMDFSummer"],
                                hydroplots["MDFMDFSpring"],
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
##                CVMDFWinter CVMDFAutumn CVMDFSpring MDFMDFSummer
## CVMDFWinter         1.0000      0.6858      0.7883       0.3814
## CVMDFAutumn         0.6858      1.0000      0.7691       0.5859
## CVMDFSpring         0.7883      0.7691      1.0000       0.4019
## MDFMDFSummer        0.3814      0.5859      0.4019       1.0000
## MDFMDFSpring       -0.7523     -0.7708     -0.7219      -0.8011
## M_MaxM             -0.7842     -0.9369     -0.8074      -0.6376
## P_MaxM             -0.6988     -0.5826     -0.4450      -0.3189
## M_MinM             -0.6226     -0.8050     -0.6418      -0.7407
## M_MDFM             -0.7726     -0.8912     -0.7773      -0.6567
## P_MDFM             -0.7993     -0.9117     -0.8239      -0.5258
## AS20YrARI           0.7820      0.8625      0.8518       0.5993
## CVAnnMRateFall      0.8923      0.8788      0.8631       0.4792
## CVAnnMRateRise      0.8752      0.8169      0.7183       0.3922
## LSPeak             -0.8258     -0.8393     -0.8365      -0.5217
## CVAnnHSPeak         0.5652      0.7368      0.4265       0.7445
##                MDFMDFSpring  M_MaxM  P_MaxM  M_MinM  M_MDFM  P_MDFM
## CVMDFWinter         -0.7523 -0.7842 -0.6988 -0.6226 -0.7726 -0.7993
## CVMDFAutumn         -0.7708 -0.9369 -0.5826 -0.8050 -0.8912 -0.9117
## CVMDFSpring         -0.7219 -0.8074 -0.4450 -0.6418 -0.7773 -0.8239
## MDFMDFSummer        -0.8011 -0.6376 -0.3189 -0.7407 -0.6567 -0.5258
## MDFMDFSpring         1.0000  0.8894  0.4277  0.7691  0.8357  0.8373
## M_MaxM               0.8894  1.0000  0.5744  0.8418  0.9497  0.9681
## P_MaxM               0.4277  0.5744  1.0000  0.3735  0.5327  0.6033
## M_MinM               0.7691  0.8418  0.3735  1.0000  0.9360  0.7883
## M_MDFM               0.8357  0.9497  0.5327  0.9360  1.0000  0.9179
## P_MDFM               0.8373  0.9681  0.6033  0.7883  0.9179  1.0000
## AS20YrARI           -0.8336 -0.9087 -0.5818 -0.7486 -0.8935 -0.9102
## CVAnnMRateFall      -0.7838 -0.9092 -0.6552 -0.7998 -0.9048 -0.9299
## CVAnnMRateRise      -0.7046 -0.8557 -0.8006 -0.6361 -0.7944 -0.8649
## LSPeak               0.8648  0.9148  0.5972  0.6792  0.8161  0.9572
## CVAnnHSPeak         -0.6551 -0.7194 -0.7642 -0.6587 -0.7124 -0.6564
##                AS20YrARI CVAnnMRateFall CVAnnMRateRise  LSPeak CVAnnHSPeak
## CVMDFWinter       0.7820         0.8923         0.8752 -0.8258      0.5652
## CVMDFAutumn       0.8625         0.8788         0.8169 -0.8393      0.7368
## CVMDFSpring       0.8518         0.8631         0.7183 -0.8365      0.4265
## MDFMDFSummer      0.5993         0.4792         0.3922 -0.5217      0.7445
## MDFMDFSpring     -0.8336        -0.7838        -0.7046  0.8648     -0.6551
## M_MaxM           -0.9087        -0.9092        -0.8557  0.9148     -0.7194
## P_MaxM           -0.5818        -0.6552        -0.8006  0.5972     -0.7642
## M_MinM           -0.7486        -0.7998        -0.6361  0.6792     -0.6587
## M_MDFM           -0.8935        -0.9048        -0.7944  0.8161     -0.7124
## P_MDFM           -0.9102        -0.9299        -0.8649  0.9572     -0.6564
## AS20YrARI         1.0000         0.8312         0.7632 -0.8958      0.7415
## CVAnnMRateFall    0.8312         1.0000         0.9062 -0.8792      0.5976
## CVAnnMRateRise    0.7632         0.9062         1.0000 -0.8311      0.6665
## LSPeak           -0.8958        -0.8792        -0.8311  1.0000     -0.6179
## CVAnnHSPeak       0.7415         0.5976         0.6665 -0.6179      1.0000
```

```r
FDis.signif.PCA <- prcomp(FDis.signif, scale=TRUE, centre=TRUE, retx=TRUE)
summary(FDis.signif.PCA)
```

```
## Importance of components:
##                          PC1    PC2    PC3    PC4    PC5    PC6     PC7
## Standard deviation     3.392 1.0948 0.9814 0.6408 0.5725 0.4844 0.38212
## Proportion of Variance 0.767 0.0799 0.0642 0.0274 0.0219 0.0156 0.00973
## Cumulative Proportion  0.767 0.8470 0.9113 0.9386 0.9605 0.9761 0.98587
##                            PC8     PC9    PC10    PC11    PC12    PC13
## Standard deviation     0.29949 0.23265 0.18371 0.14583 0.09958 0.04769
## Proportion of Variance 0.00598 0.00361 0.00225 0.00142 0.00066 0.00015
## Cumulative Proportion  0.99185 0.99546 0.99771 0.99912 0.99979 0.99994
##                           PC14     PC15
## Standard deviation     0.03083 1.02e-16
## Proportion of Variance 0.00006 0.00e+00
## Cumulative Proportion  1.00000 1.00e+00
```

```r
plot(FDis.signif.PCA)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-51.png) 

```r
biplot(FDis.signif.PCA)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-52.png) 

