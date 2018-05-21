year.ranges<-list(c(1975,1980), c(1981, 1985), c(1986, 1990), c(1991, 1995), 
                  c(1996, 2000), c(2001, 2005), c(2006, 2010), c(2011, 2015), c(2016, 2017))

getMaxRangeVar <- function(range, fname){
  
  #print(range[1])

  fname=paste(sep="", fname, range[1], "_", range[2], ".csv")

  vardf <- read.csv(fname, header = TRUE )[,-1]
  vardf[vardf==0] <- NA
  
  max1 <- max(apply(vardf, MARGIN=1, FUN=max, na.rm=TRUE), na.rm=TRUE)
  #View(max1)
  #max2 <- sapply(max1, FUN=max, simplify=TRUE)
  
  return(max1)
}

getMinRangeVar <- function(range, fname){
  
  #print(range[1])
  
  fname=paste(sep="", fname, range[1], "_", range[2], ".csv")
  
  vardf <- read.csv(fname, header = TRUE )[,-1]
  vardf[vardf==0] <- NA
  
  max1 <- min(apply(vardf, MARGIN=1, FUN=min, na.rm=TRUE), na.rm=TRUE)
  #View(max1)
  #max2 <- sapply(max1, FUN=max, simplify=TRUE)
  
  return(max1)
}

getMeanRangeVar <- function(range, fname){
  
  #print(range[1])
  
  fname=paste(sep="", fname, range[1], "_", range[2], ".csv")
  
  vardf <- read.csv(fname, header = TRUE )[,-1]
  vardf[vardf==0] <- NA
  
  max1 <- mean(apply(vardf, MARGIN=1, FUN=mean, na.rm=TRUE), na.rm=TRUE)
  #View(max1)
  #max2 <- sapply(max1, FUN=max, simplify=TRUE)
  
  return(max1)
}

maxlist <- sapply(year.ranges, FUN=getMaxRangeVar, "threshold_var_")
maxdf <- data.frame(max.var=maxlist)
row.names(maxdf) <- sapply(year.ranges, FUN=function(x) { return(paste(sep="", x[1],"-", x[2]))})

minlist <- sapply(year.ranges, FUN=getMinRangeVar, "threshold_var_")
mindf <- data.frame(min.var=minlist)
row.names(mindf) <- sapply(year.ranges, FUN=function(x) { return(paste(sep="", x[1],"-", x[2]))})

meanlist <- sapply(year.ranges, FUN=getMeanRangeVar, "threshold_var_")
meandf <- data.frame(mean.var=meanlist)
row.names(meandf) <- sapply(year.ranges, FUN=function(x) { return(paste(sep="", x[1],"-", x[2]))})

statsdf <- cbind(mindf, maxdf, meandf)

maxlist <- sapply(year.ranges, FUN=getMaxRangeVar, "thresholds_1pc_")
maxdf <- data.frame(max.threshold=maxlist)
row.names(maxdf) <- sapply(year.ranges, FUN=function(x) { return(paste(sep="", x[1],"-", x[2]))})

minlist <- sapply(year.ranges, FUN=getMinRangeVar, "thresholds_1pc_")
mindf <- data.frame(min.threshold=minlist)
row.names(mindf) <- sapply(year.ranges, FUN=function(x) { return(paste(sep="", x[1],"-", x[2]))})

meanlist <- sapply(year.ranges, FUN=getMeanRangeVar, "thresholds_1pc_")
meandf <- data.frame(mean.threshold=meanlist)
row.names(meandf) <- sapply(year.ranges, FUN=function(x) { return(paste(sep="", x[1],"-", x[2]))})

statsdf <- cbind(statsdf, mindf, maxdf, meandf)

View(statsdf)
