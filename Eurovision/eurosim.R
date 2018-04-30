library(dplyr)

voting.history <- read.csv("Data/eurovision_song_contest_1975_2017v4.csv", header = TRUE  )

trials <- 100
possible.votes <- c(12, 10, 8, 7, 6, 5, 4, 3, 2, 1)

year.range<-seq(1975,2017)

#for (year in year.range){
#  year.votes <- voting.history %>% filter(Year==year)
#  donor.countries <- unique(year.votes$From.country)
#  recip.countries <- unique(year.votes$To.country)
  
#  S<-length(recip.countries)
#  p.0 <- (S-11)/(S-1)
#  p.x <- 1/(S-1)
#  
#  p <- c(p.0, rep(p.x, 10))


#simulate.voting <- function(voting.history, start.year, end.year, trials=100){
#  averages.df <- data.frame(from.country=character(), to.country=character(), vote=vector())
#
#  for (iter in 1:trials){
#    simulation.df <- data.frame(from.country=character(), to.country=character(), vote=double())
#    
#    for (year in start.year:end.year){
#      year.votes <- voting.history %>% filter(Year==year)
#      donor.countries <- unique(year.votes$From.country)
#      recip.countries <- unique(year.votes$To.country)
#      
#     for (from.country in donor.countries){
#        countries.to.vote=recip.countries[recip.countries!=from.country]
#        position <- sample(c(1:length(countries.to.vote)), length(countries.to.vote), replace=FALSE)
#        one.iter.df=data.frame(from.country=from.country, to.country=countries.to.vote[position], vote=c(possible.votes, rep(0, length(countries.to.vote)-10)))
#        simulation.df <- bind_rows(simulation.df, one.iter.df) %>% group_by(from.country, to.country) %>% summarise_all(mean)
#        
#  #      for (to.country in recip.countries) {
#  #        avg.vote <- mean(sample(x=possible.votes, size=trials, replace=TRUE, prob=p))
#  #        simulation.df <- rbind(simulation.df, data.frame(from.country=from.country, to.country=to.country, year=year, vote=avg.vote))
#        
#      }
#    }
#    #simulation.df$vote <- list(simulation.df$vote)
#    averages.df <- bind_rows(averages.df, simulation.df) %>% group_by(from.country, to.country) %>% summarise_all(c)
#  }
#  
#  
#  return(averages.df)
#}


simulate.voting <- function(voting.history, start.year, end.year, iters=10000){
  #year.votes <- voting.history %>% filter(Year==year)
  donor.countries <- unique(voting.history$From.country)
  recip.countries <- unique(voting.history$To.country)
  
  # Precompute voting countries per year
  voting.countries <- sapply(X=seq(start.year,end.year), simplify=TRUE, FUN=function(year) {    
    year.votes <- voting.history %>% 
    filter(Year==year)
    voted.countries <- unique(year.votes$From.country)
  
    S<-length(voted.countries)
    
    return(S)
  }) 
  
  simulated.votes.thresh <- data.frame(matrix(data=0, nrow=length(donor.countries), ncol=length(recip.countries)))
  row.names(simulated.votes.thresh) <- donor.countries
  colnames(simulated.votes.thresh) <- recip.countries
  
  for (from.country in donor.countries){
    cat("From ", from.country, "\n")
    for (to.country in recip.countries[recip.countries!=from.country]) {
      cat("To ", to.country, "\n")
      average_simulation=vector()
      for (iter in 1:iters){
        one_simulation=vector()
        for (year in start.year:end.year) {
          S<-voting.countries[year-start.year+1]
          position <- ceiling(runif(1, 1, S))
          vote <- ifelse(position <= 10, possible.votes[position], 0)
          one_simulation <- c(one_simulation, vote)
#          p.0 <- (S-11)/(S-1)
#          p.x <- 1/(S-1)
#          p <- c(p.0, rep(p.x, 10))
          
#          one_simulation<-c(one_simulation, sample(c(0,1,2,3,4,5,6,7,8,10,12), 1, prob=p))
        }
        average_simulation<-c(average_simulation, mean(one_simulation))
      }
      simulated.votes.thresh[from.country,to.country] <- quantile(average_simulation, probs=c(0.99), names=FALSE)[1]
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

find.mutual.voters <- function(average.votes, statistically.significant){
  donor.countries = row.names(average.votes)
  recip.countries = colnames(average.votes)
  
  mutual.voters <- data.frame(country.1=character(), country.2=character(), average.vote.fwd=double(), average.vote.bwd=double())
  
  for (donor in donor.countries){
    for (recip in recip.countries) {
      if (statistically.significant[donor, recip]){
        if ( (recip %in% donor.countries) && (donor %in% recip.countries)) {
          if (statistically.significant[recip, donor]) {
            mutual.pair <- data.frame(country.1=donor, country.2=recip, average.vote.fwd=average.votes[donor,recip], average.vote.bwd=average.votes[recip,donor])
            mutual.voters <- rbind(mutual.voters, mutual.pair)
          }
        }
      }
    }
  }
  
  return(mutual.voters)
}

average.votes <- get.average.votes(voting.history, 1975, 1980)

vote.thresholds <- simulate.voting(voting.history, 1975, 1980, 1000)

statistically.significant <- average.votes>vote.thresholds

mutual.voters <- find.mutual.voters(average.votes, statistically.significant)
