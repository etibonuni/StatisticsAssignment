library(dplyr)
library(data.table)
library(VennDiagram)
library(gplots)
library(eulerr)
library(diagram)
library(RNeo4j)

trials <- 500
possible.votes <- c(12, 10, 8, 7, 6, 5, 4, 3, 2, 1)

year.range.all<-seq(1975,2017)
year.ranges<-list(c(1975,1980), c(1981, 1985), c(1986, 1990), c(1991, 1995), 
                  c(1996, 2000), c(2001, 2005), c(2006, 2010), c(2011, 2015), c(2016, 2017))


simulate.voting <- function(voting.history, start.year, end.year, iters=10000){
  #year.votes <- voting.history %>% filter(Year==year)
  donor.countries <- unique(voting.history$From.country)
  recip.countries <- unique(voting.history$To.country)
  
  # Precompute voting countries per year
  voting.countries.count <- sapply(X=seq(start.year,end.year), simplify=TRUE, FUN=function(year) {    
    year.votes <- voting.history %>% 
    filter(Year==year)
    voting.countries <- unique(year.votes$From.country)
  
    S<-length(voting.countries)
    
    return(S)
  }) 
  
  voting.countries <- sapply(X=seq(start.year,end.year), simplify=FALSE, FUN=function(year) {    
    year.votes <- voting.history %>% 
      filter(Year==year)
    voting.countries <- unique(as.character(year.votes$From.country))
    
    return(voting.countries)
  }) 
  
  voted.countries <- sapply(X=seq(start.year,end.year), simplify=FALSE, FUN=function(year) {    
    year.votes <- voting.history %>% 
      filter(Year==year)
    voted.countries <- unique(as.character(year.votes$To.country))
    
    return(voted.countries)
  }) 
  simulated.votes.thresh <- data.frame(matrix(data=0, nrow=length(donor.countries), ncol=length(recip.countries)))
  row.names(simulated.votes.thresh) <- donor.countries
  colnames(simulated.votes.thresh) <- recip.countries
  
  for (from.country in donor.countries){
    cat("From ", from.country, "\n")
    for (to.country in recip.countries[recip.countries!=from.country]) {
      if (from.country != to.country)
      {
        #cat("To ", to.country, "\n")
        average_simulation=vector()
        for (iter in 1:iters){
          one_simulation=vector()
          for (year in start.year:end.year) {
            
            if ((from.country %in% voting.countries[[year-start.year+1]]) &&
                (to.country %in% voted.countries[[year-start.year+1]]))
            {
              S<-voting.countries.count[year-start.year+1]
              position <- ceiling(runif(1, 1, S))
              vote <- ifelse(position <= 10, possible.votes[position], 0)
              one_simulation <- c(one_simulation, vote)
            }
          }
          if (length(one_simulation)>0){
            average_simulation<-c(average_simulation, mean(one_simulation))
          }
        }
        thresh <- quantile(average_simulation, probs=c(0.99), names=FALSE)[1]
        if (!is.na(thresh)) {
          simulated.votes.thresh[from.country,to.country] <- thresh
        }
      }
    }
  }
  return(simulated.votes.thresh)
}  

get.average.votes <- function(voting.history, start.year, end.year){
  donor.countries <- unique(voting.history$From.country)
  recip.countries <- unique(voting.history$To.country)
  
  average.votes <- data.frame(matrix(data=0, nrow=length(donor.countries), ncol=length(recip.countries)))
  row.names(average.votes) <- donor.countries
  colnames(average.votes) <- recip.countries

  year.votes <- voting.history %>% 
    filter(between(Year, start.year, end.year), Duplicate!="x") %>%
    select(From.country, To.country, Points) %>%
    group_by(From.country, To.country) %>% summarise_all(mean)
  
  for (r in 1:nrow(year.votes)){
    fromCountry <- as.character(year.votes$From.country[r])
    toCountry <- as.character(year.votes$To.country[r])
    votes <- as.numeric(year.votes$Points[r])
    average.votes[fromCountry, toCountry] <- votes
  }
  
  return(average.votes)
}

find.mutual.voters <- function(average.votes, statistically.significant, year){
  donor.countries = row.names(average.votes)
  recip.countries = colnames(average.votes)
  
  mutual.voters <- data.frame(country.1=character(), country.2=character(), average.vote.fwd=double(), average.vote.bwd=double(), year=integer())
  
  for (donor in donor.countries){
    for (recip in recip.countries) {
      if (statistically.significant[donor, recip]){
        if ( (recip %in% donor.countries) && (donor %in% recip.countries)) {
          if (statistically.significant[recip, donor]) {
            if (nrow(mutual.voters[mutual.voters$country.2==donor,])==0){
              mutual.pair <- data.frame(country.1=donor, country.2=recip, average.vote.fwd=average.votes[donor,recip], average.vote.bwd=average.votes[recip,donor], year=year)
              mutual.voters <- rbind(mutual.voters, mutual.pair)
            }
          }
        }
      }
    }
  }
  
  return(mutual.voters)
}

find.mutual.voters2 <- function(average.votes, statistically.significant){
  donor.countries = row.names(average.votes)
  recip.countries = colnames(average.votes)
  neighbours<-average.votes
  #mutual.voters <- data.frame(country.1=character(), country.2=character(), average.vote.fwd=double(), average.vote.bwd=double())
  
  for (donor in donor.countries){
    for (recip in recip.countries) {
      if (statistically.significant[donor, recip]){
        if ( (recip %in% donor.countries) && (donor %in% recip.countries)) {
          if (!statistically.significant[recip, donor]) {
            neighbours[recip, donor] <- 0
            neighbours[donor, recip] <- 0
          }
        }
        else
        {
          neighbours[donor, recip] <- 0
        }
      }
      else
      {
        neighbours[donor, recip] <- 0
      }
    }
  }
  neighbours <- neighbours[rowSums(neighbours==0) != ncol(neighbours),]
  neighbours <- neighbours[,colSums(neighbours != 0) != 0]
  
  return(neighbours)
}


#start.date=2001
#end.date=2005

voting.history <- read.csv("Data/eurovision_song_contest_1975_2017v4.csv", header = TRUE  )
graph = startGraph("http://localhost:7474/db/data/")
clear(graph, input=FALSE)

for (year.range in year.ranges){
  start.date = year.range[1]
  end.date = year.range[2]
  
  cat("Running simulation for years ", start.date, " to ", end.date, "\n")
  
  average.votes <- get.average.votes(voting.history, start.date, end.date)
  
  vote.thresholds <- simulate.voting(voting.history, start.date, end.date, trials)
  
  statistically.significant <- average.votes>vote.thresholds
  
  mutual.voters <- find.mutual.voters(average.votes, statistically.significant, start.date)
  neighbours <- find.mutual.voters2(average.votes, statistically.significant)
  
  pp <- plotmat(neighbours, curve = 0, name = row.names(neighbours),
   lwd = 1, box.lwd = 2, cex.txt = 0.8,
   box.type = "circle", box.prop = 0.2, arr.type = "triangle",
   arr.pos = 0.4, shadow.size = 0.0,
   main = paste("Detected collusive voting at 1% significance level (",as.character(start.date),"-",as.character(end.date),")"))
  
  countryNodeType <- paste(sep="_", "Country", as.character(start.date))
  addConstraint(graph, countryNodeType, "name")
  
  addCollusionToGraph <- function(x, graph){
    print(x)
    country1 <- getOrCreateNode(graph, countryNodeType, name=x[1])
    country2 <- getOrCreateNode(graph, countryNodeType, name=x[2])
    
    createRel(country1, x[3], country2, avgVote=x[3], year=start.date)
    createRel(country2, x[4], country1, avgVote=x[4], year=start.date)
  }
  
  apply(mutual.voters, MARGIN=1, addCollusionToGraph, graph=graph)
}