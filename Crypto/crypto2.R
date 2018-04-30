library(stats)
library(reshape2)
library(dplyr)
library(ggplot2)
library(zoo)
library(data.table)

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
        correlation = cor(graph1, graph2, use="pairwise.complete.obs")#ccf(graph1, graph2, plot = FALSE, lag.max=7, na.action = na.pass)#, lag.max = 0)
        print(correlation)
        cross[graph1Id, graph2Id] = correlation
      }
    }
  }
  return(cross)
}
df = read.csv(file="data/crypto-markets.csv", header=TRUE)

# convert the column to Date
df$date <- as.Date(df$date, "%Y-%m-%d")

# Extract the top 20 currencies and take a sane date range
filtereddf <- df %>% select(name, date, ranknow, close) %>% filter(ranknow <= 50, date >= "2017-10-01")

print(ggplot(filtereddf) + geom_line(aes(x = date, y = close, colour = name)))

pivotdf <- cast(filtereddf, date ~ name)
pivotdf[,-1] <- scale(pivotdf[,-1])
#zdf <- zoo(pivotdf[,-1], order.by=pivotdf[,1])
#diffdf <- as.data.frame(diff(zdf))
#setDT(diffdf, keep.rownames = TRUE)[]
#diffdf <- as.data.frame(diffdf)

#colnames(diffdf) <- colnames(pivotdf)
#diffdf$date <- as.Date(diffdf$date, "%Y-%m-%d")

#tempdf = melt(diffdf, id.vars="date")

#print(ggplot(tempdf) + geom_line(aes( x=date, value, colour=variable)))

cortbl2 <-  correlationTable(pivotdf[,-1])
#ccf(diffdf$Bitcoin, diffdf$Ethereum)
#ccf(diffdf$Bitcoin, diffdf$Ripple)
#ccf(diffdf$Ethereum, diffdf$Ripple)


