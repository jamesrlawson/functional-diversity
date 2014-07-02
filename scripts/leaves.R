## leaf area ##

library(plyr)

leaves <- read.csv("data/leaves.csv", header=TRUE, stringsAsFactors=FALSE)




deartifact.leafscans <- function(x) {
  
    a <- ddply(leaves, .(Label, Site), summarise,
                       low.boundary = mean(Area)/5)
    
    b <- merge(x, a)
    
    c <- ddply(b, .(Label, Site), summarise,
                        keep = Area > low.boundary)
    
    leaves.ordered <- leaves[order(leaves$Label),]
    
    d <- cbind(leaves.ordered, "keep"= c$keep)
    
    leaves.deart <- subset(d, keep == "TRUE")
    
    leaves.area <- ddply(leaves.deart, .(Label, Site), summarise, 
                         meanArea = mean(Area),
                         sdArea = sd(Area),
                         count = length(Area)
    )
    
  return(leaves.area)  
} 




leaves.area <- ddply(leaves, .(Label, Site), summarise, 
                     meanArea = mean(Area),
                     sdArea = sd(Area),
                     count = length(Area)
                    )