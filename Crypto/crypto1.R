library(stats)
library(reshape2)
library(dplyr)
library(ggplot2)
library(zoo)
library(data.table)

df = read.csv(file="data/crypto-markets.csv", header=TRUE)

# convert the column to Date
df$date <- as.Date(df$date, "%Y-%m-%d")

# Extract the top 20 currencies and take a sane date range
filtereddf <- df %>% select(name, date, ranknow, close) %>% filter(ranknow <= 3, date >= "2017-10-01")

print(ggplot(filtereddf) + geom_line(aes(x = date, y = close, colour = name)))

pivotdf <- cast(filtereddf, date ~ name)
pivotdf[,-1] <- scale(pivotdf[,-1])
zdf <- zoo(pivotdf[,-1], order.by=pivotdf[,1])
diffdf=as.data.frame(diff(zdf))
setDT(diffdf, keep.rownames = TRUE)[]

colnames(diffdf) <- colnames(pivotdf)
diffdf$date <- as.Date(diffdf$date, "%Y-%m-%d")

tempdf = melt(diffdf, id.vars="date")

print(ggplot(tempdf) + geom_line(aes( x=date, value, colour=variable)))

ccf(diffdf$Bitcoin, diffdf$Ethereum)
ccf(diffdf$Bitcoin, diffdf$Ripple)
ccf(diffdf$Ethereum, diffdf$Ripple)


