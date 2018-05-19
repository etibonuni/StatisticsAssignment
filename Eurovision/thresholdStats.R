year.ranges<-list(c(1975,1980), c(1981, 1985), c(1986, 1990), c(1991, 1995), 
                  c(1996, 2000), c(2001, 2005), c(2006, 2010), c(2011, 2015), c(2016, 2017))

getMaxRangeVar <- function(range){
  
  #print(range[1])

  fname=paste(sep="", "threshold_var_", range[1], "_", range[2], ".csv")

  vardf <- read.csv(fname, header = TRUE )[,-1]

  max1 <- max(apply(vardf, MARGIN=1, FUN=max))
  #View(max1)
  #max2 <- sapply(max1, FUN=max, simplify=TRUE)
  
  return(max1)
}


getMeanRangeVar <- function(range){
  
  #print(range[1])
  
  fname=paste(sep="", "threshold_var_", range[1], "_", range[2], ".csv")
  
  vardf <- read.csv(fname, header = TRUE )[,-1]
  
  max1 <- mean(apply(vardf, MARGIN=1, FUN=mean))
  #View(max1)
  #max2 <- sapply(max1, FUN=max, simplify=TRUE)
  
  return(max1)
}

maxlist <- sapply(year.ranges, FUN=getMaxRangeVar)
maxdf <- data.frame(max.var=maxlist)
row.names(maxdf) <- sapply(year.ranges, FUN=function(x) { return(paste(sep="", x[1],"-", x[2]))})

meanlist <- sapply(year.ranges, FUN=getMeanRangeVar)
meandf <- data.frame(mean.var=meanlist)
row.names(meandf) <- sapply(year.ranges, FUN=function(x) { return(paste(sep="", x[1],"-", x[2]))})

View(maxdf)
View(meandf)