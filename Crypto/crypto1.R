library(stats)
library(reshape2)
library(dplyr)
library(ggplot2)
library(zoo)
library(data.table)
library(corrplot)

Sys.setlocale("LC_TIME", "en_US.UTF-8")

correlationTable = function(graphs) {
  cross = matrix(nrow = length(graphs), ncol = length(graphs))
  row.names(cross)=colnames(graphs)
  colnames(cross)=colnames(graphs)
  print(cross)
  for(graph1Id in 1:length(graphs)){
    graph1 = graphs[,graph1Id]
    print(graph1Id)
    for(graph2Id in 1:length(graphs)) {
      graph2 = graphs[,graph2Id]
#      correlation = ccf(graph1, graph2, plot = FALSE, lag.max=7, na.action = na.pass)#, lag.max = 0)
#      cortn = correlation$acf[order(correlation$acf, decreasing = TRUE)[1]]
#      p.value <- 2 * (1 - pnorm(abs(cortn), mean = 0, sd = 1/sqrt(correlation$n.used)))
#      if (p.value <= 0.05) {
#        cross[graph1Id, graph2Id] <- cortn        
#      }
#      else
#      {
#        cross[graph1Id, graph2Id] <- 0
#      }
        crtn <- cor.test(graph1, graph2, method = "spearman")
        if (crtn$p.value < 0.05){
          cross[graph1Id, graph2Id] <- crtn$estimate
        }
        else{
          cross[graph1Id, graph2Id] <- 0
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
    
    #match2 <- unique(unlist(sapply(match, findCorrelated, highCorr, simplify = T)))
    
    clusters[[origin]]<-match
  }
  
  return(clusters)
}
df = read.csv(file="data/crypto-markets.csv", header=TRUE)

# convert the column to Date
df$date <- as.Date(df$date, "%Y-%m-%d")

# Extract the top 20 currencies and take a sane date range...
filtereddf <- df %>% select(name, date, ranknow, close) %>% filter(ranknow <= 20, date >= "2016-01-01")

# ... and plot them,
print(ggplot(filtereddf) + geom_line(aes(x = date, y = close, colour = name)))

# Pivot the dataframe in order to process it more easily
pivotdf <- dcast(filtereddf, date ~ name)

# Scale the values so as to be able to calculate the correlation
#pivotdf[,-1] <- scale(pivotdf[,-1])

# Generate the pairwise correlation table between the 20 crypto-currencies
cortbl <-correlationTable(data.frame(scale(pivotdf[,-1])))

# Find the currencies that are correlated to within 95% of Bitcoin
highCorr = which(cortbl > 0.95 , arr.ind = TRUE)
match = findCorrelated(3, highCorr)
print(colnames(pivotdf[match+1])) # Print the names of the matching currencies

# Melt the resulting currencies so they can be plotted by ggplot
tempdf = melt(pivotdf[, c(1, 3, match+1)], id.vars="date")
# Plot them
print(ggplot(tempdf) + geom_line(aes(x = date, y = value, colour = variable)))

# Plot the correlation matrix
corrplot::corrplot(cortbl, type="lower")

# Cluster the currencies with correlation > 95%
m=extractClusters(cortbl)

plotCluster <- function(clusterNumber, clusters, currencyTable) {
  currencyNames <- colnames(currencyTable)[-1]
  tempdf = melt(currencyTable[, c(1, clusters[[clusterNumber]]+1)], id.vars="date")
  print(ggplot(tempdf) + geom_line(aes(x = date, y = value, colour = variable)) + ggtitle(paste("Currencies correlated to", currencyNames[clusterNumber])))
}

# Plot some example clusters
plotCluster(1, m, pivotdf)
plotCluster(2, m, pivotdf)

##### Load conventional currencies ########################
df.eur = read.csv(file="data/EUR_USD Historical Data.csv", header=TRUE)
df.eur$Date <- as.Date(df.eur$Date, "%b %d, %Y")
df.eur <- df.eur[,c("Date", "Price")]
df.eur$Price <- as.numeric(df.eur$Price)
colnames(df.eur) <- c("Date", "Price.eur")

df.gbp = read.csv(file="data/GBP_USD Historical Data.csv", header=TRUE)
df.gbp$Date <- as.Date(df.gbp$Date, "%b %d, %Y")
df.gbp <- df.gbp[,c("Date", "Price")]
df.gbp$Price <- as.numeric(df.gbp$Price)
colnames(df.gbp) <- c("Date", "Price.gbp")

df.jpy = read.csv(file="data/JPY_USD Historical Data.csv", header=TRUE)
df.jpy$Date <- as.Date(df.jpy$Date, "%b %d, %Y")
df.jpy <- df.jpy[,c("Date", "Price")]
df.jpy$Price <- as.numeric(df.jpy$Price)
colnames(df.jpy) <- c("Date", "Price.jpy")

# Take bitcoin
conv.currencies <- pivotdf[,c("date", "Bitcoin")]
colnames(conv.currencies)[1] <- "Date"
conv.currencies$Bitcoin <- as.numeric(conv.currencies$Bitcoin)

# Add our conventional currencies
conv.currencies <- merge(conv.currencies, merge(df.eur, merge(df.gbp, df.jpy, by="Date", all=TRUE), by="Date", all=TRUE), by="Date", all=TRUE)
# Impute NA's with the previous value in the dataframe
conv.currencies[,-1] <- conv.currencies[,-1] %>% do(na.locf(.))

conv.currencies.scaled <- conv.currencies
conv.currencies.scaled[-1] <- scale(conv.currencies[-1], scale=TRUE)

# Melt the resulting currencies so they can be plotted by ggplot
tempdf = melt(conv.currencies.scaled, id.vars="Date")

# Plot them
print(ggplot(tempdf) + geom_line(aes(x = Date, y = value, colour = variable)))

