library(plyr)
library(ggplot2)

# find species in % cover list associated with trait values

findtraitvals <- function(plotsums, trait) {
  
  
  
  y <- data.frame()
  
  for(i in 1:nrow(plotsums)) {
    
    species <- as.data.frame(plotsums$species)
    colnames(species) <- c("species")
    x <- species[i,]
    
    # grep returns index of matches (similar to match but returns all indexes, not just the first)
    
    x <- grep(x, plotsums$species, fixed=TRUE) 
    x <- plotsums[x,]
    x <- merge(x, trait, by.x = "species", by.y = "species")
    
    # iteratively add things to dataframe y!
    y <- rbind(y,x)
    
  }
  # subset by matches 
  #x <- plotsums[na.omit(x), ] 
  
    y <- ddply(y, .(plotID), unique)
    return(y)
}


# relabund finds relative % cover for each species at each site

relabund <- function(df) {
                              
                relcover <- df$speciescover / df$traitcover 
                cbind(df, "relcover" = relcover)
}
  

spread <- function(x) (diff(range(x)))


plot.linear <- function(df, var, trait) { # var is alphaT/betaT/ts/Rs, etc.
  
  
  figureDir <- "C:/Users/James/Desktop/stuff/data/analysis/R/functional diversity/output/figures"
  traitDir <- deparse(substitute(trait))
  varDir <- deparse(substitute(var))
  
  outDir <- sprintf("%s/%s/%s/linear", figureDir, traitDir, varDir)
  
  dir.create(outDir, recursive=TRUE)
  
  labels <- list("ylab" <- c(deparse(substitute(trait))))
  
  for(i in 1:ncol(df)) {
    hydro <- df[[i]]  
    hydroname <- as.expression(colnames(df[i]))   
    fit.linear <- lm(var ~ hydro, data = df)
    
    #  padj <- labels$p.adj[i]
    r2 <- signif(summary(fit.linear)$r.squared, 5)
    pval <- anova(fit.linear)[1,"Pr(>F)"]
    
    tiff(sprintf("%s/%s_pval-%s_r2-%s.png", outDir, hydroname, pval, r2), width = 400, height = 300)
    
    p <- qplot(hydro, var, data = df) 
    p <- p + geom_point(size = 3)
  
    p <- p + stat_smooth(aes(group = 1), method = "lm", formula = y ~ x, fullrange=TRUE, se=TRUE, col="black", alpha = 0.2) 
    p <- p + xlab(hydroname)
    p <- p + ylab(c("FDis"))  
    p <- p + theme_bw() 
    p <- p + theme_set(theme_bw(base_size = 15))
    p <- p + theme(legend.position = "none",
                   axis.text = element_text(size = rel(0.8)),
#                   axis.title.y = element_text(hjust=0.35),
                   axis.title.x = element_text(vjust=0.35),
                   panel.border = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.line = element_line(size=.2, color = "black"))
    
    print(p)
   
    
    print(p) 
    dev.off()
  }
}

plot.quad <- function(df, var, trait, labels) { # var is alphaT/betaT/ts/Rs, etc.
  
  
  figureDir <- "C:/Users/James/Desktop/stuff/data/analysis/R/functional diversity/output/figures"
  traitDir <- deparse(substitute(trait))
  varDir <- deparse(substitute(var))
  
  outDir <- sprintf("%s/%s/%s/quad", figureDir, traitDir, varDir)
  
  dir.create(outDir, recursive=TRUE)
  
  labels <- list("ylab" <- c(deparse(substitute(trait))))
  
  for(i in 1:ncol(df)) {
    hydro <- df[[i]]  
    hydroname <- as.expression(colnames(df[i]))   
    fit.quad <- lm(var ~ hydro + I(hydro^2), data = df)
    
    #  padj <- labels$p.adj[i]
    r2 <- signif(summary(fit.quad)$r.squared, 5)
    pval <- anova(fit.quad)[1,"Pr(>F)"]
    
    tiff(sprintf("%s/%s_pval-%s_r2-%s.png", outDir, hydroname, pval, r2), width = 400, height = 300)
    
    p <- qplot(hydro, var, data = df) 
    p <- p + geom_point()

    p <- p + stat_smooth(aes(group = 1), method = "lm", formula = y ~ x + I(x^2), se=TRUE, col="black", alpha = 0.2) 
    p <- p + xlab(hydroname)
    p <- p + ylab(c("FDis"))  
    p <- p + theme_bw() 
    p <- p + theme_set(theme_bw(base_size = 15))
    p <- p + theme(legend.position = "none",
                   axis.text = element_text(size = rel(0.8)),
                   #                   axis.title.y = element_text(hjust=0.35),
                   axis.title.x = element_text(vjust=0.35),
                   panel.border = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   axis.line = element_line(size=.2, color = "black"))
    
    print(p)
    dev.off()
  }
}

# keeping this for future reference. returns the dataframe D_Sna_hydro_*trait*

gettrait<- function(trait) {
  
  traitname <- deparse(substitute(trait))
  get(sprintf("D_Sna_hydro_%s", traitname))
  
}


getStats <- function(df, var, trait) {
  
  # create / set output directory
  
  statsDir <- "C:/Users/James/Desktop/stuff/data/analysis/R/functional diversity/output/stats"
   
  dir.create(statsDir, recursive=TRUE, showWarnings=FALSE)

  # output stats for each metric to dataframe

  y <- data.frame()
  
  for(i in 1:ncol(df)) {
    
    hydro <- df[[i]]  
    hydroname <- as.expression(colnames(df[i]))  
    
    fit.linear <- lm(var ~ hydro, data = df)
    fit.quad <- lm(var ~ hydro + I(hydro^2), data = df)
    
    r2.linear <- signif(summary(fit.linear)$r.squared, 5)
    pval.linear <- anova(fit.linear)[1,"Pr(>F)"]
    
    r2.quad <- signif(summary(fit.quad)$r.squared, 5)
    pval.quad <- anova(fit.quad)[1,"Pr(>F)"]
    
    x <- cbind(pval.linear, r2.linear, pval.quad, r2.quad)
    
    x <- as.data.frame(x)
    
    x <- cbind(as.character(hydroname), x)
    
    colnames(x) <- c("metric", "pval.linear", "r2.linear", "pval.quad", "r2.quad")
    
    if (pval.linear < 0.05) { 
    y <- rbind(x,y)
    }
    
  }
  
  var <- deparse(substitute(var))
    
  y$padj.linear <- p.adjust(y$pval.linear, method="BH", n=31)
  y$padj.quad <- p.adjust(y$pval.quad, method="BH", n=31)
  
  write.csv(y, sprintf("%s/%s_stats.csv", statsDir, var))
   
  return(y)
  
}

getAllStats <- function(df, var, trait) {
  
  y <- data.frame()
  
  for(i in 1:ncol(df)) {
    
    hydro <- df[[i]]  
    hydroname <- as.expression(colnames(df[i]))  
    
    fit.linear <- lm(var ~ hydro, data = df)
    fit.quad <- lm(var ~ hydro + I(hydro^2), data = df)
    
    r2.linear <- signif(summary(fit.linear)$r.squared, 5)
    pval.linear <- anova(fit.linear)[1,"Pr(>F)"]
    fstat.linear <- signif(summary(fit.linear)$fstatistic[1], 4)
    
    r2.quad <- signif(summary(fit.quad)$r.squared, 5)
    pval.quad <- anova(fit.quad)[1,"Pr(>F)"]
    fstat.quad <- signif(summary(fit.quad)$fstatistic[1], 4)

    
    x <- cbind(pval.linear, r2.linear, fstat.linear, pval.quad, r2.quad, fstat.quad)
    
    x <- as.data.frame(x)
    
    x <- cbind(as.character(hydroname), x)
    
    colnames(x) <- c("metric", "pval.linear", "r2.linear", "f statistic linear", "pval.quad", "r2.quad", "f statistic quad")
    
      y <- rbind(x,y)
    
  }
  
  var <- deparse(substitute(var))
    
  return(y)
  
}








