library(stats)
library(reshape2)
library(dplyr)
library(ggplot2)
library(zoo)
library(data.table)
library(ggcorrplot)

Sys.setlocale("LC_TIME", "en_US.UTF-8")

correlationTable = function(graphs) {
  cross = matrix(nrow = length(graphs), ncol = length(graphs))
  p.mat = matrix(nrow=length(graphs), ncol=length(graphs))
  
  row.names(cross)=colnames(graphs)
  colnames(cross)=colnames(graphs)
  #print(cross)
  for(graph1Id in 1:length(graphs)){
    graph1 = graphs[,graph1Id]
    #print(graph1Id)
    for(graph2Id in 1:length(graphs)) {
      graph2 = graphs[,graph2Id]
      ccf1 = ccf(graph1, graph2, plot = FALSE, lag.max=7, na.action = na.pass)#, lag.max = 0)
      
      ind.max <- which(abs(ccf1$acf)==max(abs(ccf1$acf)))[1]
      max.cor <- ccf1$acf[ind.max]
      #print(max.cor)
      lag.opt <- ccf1$lag[ind.max] 
      
#      cortn = correlation$acf[order(correlation$acf, decreasing = TRUE)[1]]
      p.value <- 2 * (1 - pnorm(abs(max.cor), mean = 0, sd = 1.0/sqrt(ccf1$n.used)))
      cross[graph1Id, graph2Id] <- max.cor
      p.mat[graph1Id, graph2Id] <- p.value
      # cor()
      #   crtn <- cor.test(graph1, graph2, method = "kendall")
      #   cat("p=", crtn$p.value, "\n")
      #   if (crtn$p.value < 0.05){
      #     cross[graph1Id, graph2Id] <- crtn$estimate
      #   }
      #   else{
      #     cross[graph1Id, graph2Id] <- 0
      #   }
    }
  }
  return(list(corr=cross, p.mat=p.mat))
}

# correlationTable = function(graphs) {
#   #graphs[is.na(graphs)] <- 0
#   logrets<-data.frame(
#            diff(as.matrix(log(graphs)))
#        )
#   cross <- cor(x=logrets, method = "spearman", use="na.or.complete")
#   pmat <- cor_pmat(logrets)
#   
#   return(list(corr=cross, p.mat=pmat))
# }
findCorrelated = function(orig, highCorr){
  match = highCorr[highCorr[,1] == orig | highCorr[,2] == orig,]
  match = as.vector(match)
  match[match != orig]
}
extractClusters<-function(correlationTable, sig.level){
  # Take off the fully correlated series (will probably be diagonal)
  correlationTable.adj <- correlationTable[abs(correlationTable)!=1]
  # find the highest correlation level
  max.corr <- max(correlationTable.adj)

  # Take the top 5th percentile
  minCorr <- quantile(as.vector(correlationTable.adj), probs=c(sig.level))

  highCorr <- which(correlationTable > minCorr , arr.ind = TRUE)

  clusters=list()
  for (origin in 1:nrow(correlationTable)){
    
    match <- findCorrelated(origin, highCorr)
    
    #match2 <- unique(unlist(sapply(match, findCorrelated, highCorr, simplify = T)))
    
    clusters[[origin]]<-unique(match)
  }
  
  return(clusters)
}
df = read.csv(file="data/crypto-markets.csv", header=TRUE)

# convert the column to Date
df$date <- as.Date(df$date, "%Y-%m-%d")

# Extract the top 20 currencies and take a sane date range...
filtereddf <- df %>% select(name, date, ranknow, close) %>% filter(ranknow <= 20, date >= "2017-01-01")

# ... and plot them,
print(ggplot(filtereddf) + geom_line(aes(x = date, y = close, colour = name)))

# Pivot the dataframe in order to process it more easily
pivotdf <- dcast(filtereddf, date ~ name, value.var="close")

# Scale the values so as to be able to calculate the correlation
pivotdf[,-1] <- scale(pivotdf[,-1])
logrets<-data.frame(
              diff(as.matrix(log(pivotdf[,-1])))
          )
#tempdf = melt(cbind(data.frame(date=pivotdf$date[-1]), logrets)[,c(1,2,3)], id.vars="date")
#print(ggplot(tempdf) + geom_line(aes(x = date, y = value, colour = variable)))
# Generate the pairwise correlation table between the 20 crypto-currencies
#cortbl <-correlationTable(data.frame(scale(pivotdf[,-1])))
#cortbl <-correlationTable(pivotdf[,-1])
cortbl <- correlationTable(logrets)

# Plot the correlation matrix
#corrplot::corrplot(cortbl, type="lower")
print(ggcorrplot(cortbl$corr,  method="square", lab=TRUE, lab_size=2, p.mat=cortbl$p.mat)+
        labs(title="Cryptopcurrency Correlation (p-value < 0.05)"))

# Cluster the currencies with correlation > 95%
clusters=extractClusters(cortbl$corr, 0.95)
print(clusters)

plotCluster <- function(clusterNumber, clusters, currencyTable) {
  currencyNames <- colnames(currencyTable)[-1]
  tempdf = melt(currencyTable[, c(1, clusterNumber+1, clusters[[clusterNumber]]+1)], id.vars="date")
  print(ggplot(tempdf) + geom_line(aes(x = date, y = value, colour = variable)) + ggtitle(paste("Currencies correlated to", currencyNames[clusterNumber])))
}

# Plot some example clusters
plotCluster(1, clusters, cbind(data.frame(date=pivotdf$date), data.frame(scale(pivotdf[,-1]))))
plotCluster(2, clusters, cbind(data.frame(date=pivotdf$date), data.frame(scale(pivotdf[,-1]))))
plotCluster(3, clusters, cbind(data.frame(date=pivotdf$date), data.frame(scale(pivotdf[,-1]))))
plotCluster(4, clusters, cbind(data.frame(date=pivotdf$date), data.frame(scale(pivotdf[,-1]))))
plotCluster(7, clusters, cbind(data.frame(date=pivotdf$date), data.frame(scale(pivotdf[,-1]))))
plotCluster(17, clusters, cbind(data.frame(date=pivotdf$date), data.frame(scale(pivotdf[,-1]))))

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

# Find crypto with most correlations
max.crypto <- order(sapply(clusters, length), decreasing=TRUE)[1:2]
# Take bitcoin
conv.currencies <- pivotdf[,c(1, max.crypto)]
colnames(conv.currencies)[1] <- "Date"
conv.currencies[,2] <- as.numeric(conv.currencies[,2])

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

conv.corr <- correlationTable(conv.currencies[-1])
#corrplot(conv.corr$corr, type="lower", addCoef.col = "blue")
print(ggcorrplot(conv.corr$corr, method="circle", lab=TRUE, p.mat=conv.corr$p.mat))

