library(stats)
library(reshape2)
library(dplyr)
library(ggplot2)
library(zoo)
library(data.table)
library(corrplot)

correlationTable = function(graphs) {
  cross = matrix(nrow = length(graphs), ncol = length(graphs))
  print(cross)
  for(graph1Id in 1:length(graphs)){
    graph1 = graphs[,graph1Id]
    print(graph1Id)
    for(graph2Id in 1:length(graphs)) {
      graph2 = graphs[,graph2Id]
      if(graph1Id == graph2Id){
        break;
      } else {
        correlation = ccf(graph1, graph2, plot = FALSE, lag.max=7, na.action = na.pass)#, lag.max = 0)
        print(correlation)
        print(order(correlation$acf, decreasing = TRUE))
        cortn = correlation$acf[order(correlation$acf, decreasing = TRUE)[1]]
        cross[graph1Id, graph2Id] = cortn
      }
    }
  }
  return(cross)
}

findCorrelated = function(orig, highCorr){
  match = highCorr[highCorr[,1] == orig | highCorr[,2] == orig,]
  match = as.vector(match)
  match[match != orig]
}
extractClusters<-function(correlationTable){
  highCorr <- which(cortbl > 0.90 , arr.ind = TRUE)
  
  clusters=list()
  for (origin in 1:nrow(correlationTable)){
    
    match <- findCorrelated(origin, highCorr)
    
    match2 <- unique(unlist(sapply(match, findCorrelated, highCorr, simplify = T)))
    
    clusters[[origin]]<-match2
  }
  
  return(clusters)
}
df = read.csv(file="data/crypto-markets.csv", header=TRUE)

# convert the column to Date
df$date <- as.Date(df$date, "%Y-%m-%d")

# Extract the top 20 currencies and take a sane date range
filtereddf <- df %>% select(name, date, ranknow, close) %>% filter(ranknow <= 50, date >= "2016-01-01")

print(ggplot(filtereddf) + geom_line(aes(x = date, y = close, colour = name)))

pivotdf <- dcast(filtereddf, date ~ name)
pivotdf[,-1] <- scale(pivotdf[,-1])
#zdf <- zoo(pivotdf[,-1], order.by=pivotdf[,1])
#diffdf <- as.data.frame(diff(zdf))
#setDT(diffdf, keep.rownames = TRUE)[]
#diffdf <- as.data.frame(diffdf)

#colnames(diffdf) <- colnames(pivotdf)
#diffdf$date <- as.Date(diffdf$date, "%Y-%m-%d")

#tempdf = melt(diffdf, id.vars="date")

#print(ggplot(tempdf) + geom_line(aes( x=date, value, colour=variable)))

#cortbl <-  correlationTable(diffdf[,-1])
cortbl <-correlationTable(pivotdf[,-1])

#ccf(diffdf$Bitcoin, diffdf$Ethereum)
#ccf(diffdf$Bitcoin, diffdf$Ripple)
#ccf(diffdf$Ethereum, diffdf$Ripple)




highCorr = which(cortbl > 0.90 , arr.ind = TRUE)
match = findCorrelated(4, highCorr)
match # print 6 12 23 42 44 45  3

tempdf = melt(pivotdf[, c(1, 4, 6, 12, 23, 42, 44, 45, 3)], id.vars="date")
print(ggplot(tempdf) + geom_line(aes(x = date, y = value, colour = variable)))

print(ggplot(melt(pivotdf[, c(1, 9, 5)], id.vars="date")) + geom_line(aes(x = date, y = value, colour = variable)))
corrplot::corrplot(cortbl)

m=extractClusters(cortbl)

tempdf = melt(pivotdf[, m[[6]]], id.vars="date")
print(ggplot(tempdf) + geom_line(aes(x = date, y = value, colour = variable)))
