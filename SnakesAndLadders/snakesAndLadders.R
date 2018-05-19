library(markovchain)
library(igraph)
library(jpeg)

# This function is taken from the demo code for the markovchain package at https://github.com/spedygiorgio/markovchain
# It is used to return the components of a canonic form transition matrix:
# +---+---+
# | Q | R | Q = transitional states, R = Absorbing states
# +---+---+
# | 0 | I | 0 = zero matrix, I = identity matrix
# +---+---+
# N = Funcdamental matrix = (I - Q)^-1 = Q.Q.Q...
# NR = Final absorbtion probabilities
extractMatrices <- function(mcObj) {
  require(matlab)
  mcObj <- canonicForm(object = mcObj)
  #get the indices of transient and absorbing
  transIdx <- which(states(mcObj) %in% transientStates(mcObj))
  absIdx <- which(states(mcObj) %in% absorbingStates(mcObj))
  #get the Q, R and I matrices
  Q <- as.matrix(mcObj@transitionMatrix[transIdx,transIdx])
  R <- as.matrix(mcObj@transitionMatrix[transIdx,absIdx])
  I <- as.matrix(mcObj@transitionMatrix[absIdx, absIdx])
  #get the fundamental matrix
  N <- solve(eye(size(Q)) - Q)
  #computing final absorbion probabilities
  NR <- N %*% R
  #return
  out <- list(
    canonicalForm = mcObj,
    Q = Q,
    R = R,
    I = I,
    N=N,
    NR=NR
  )
  return(out)
}

# Define the positions of our ladders and snakes
ladderBottoms <- c( 1,  4,  9, 21, 28, 51, 71, 80, 110)
ladderTops <-    c(38, 14, 31, 42, 84, 67, 91, 100, 110)

laddersByBottom <- list()
laddersByTop <- list()
for (i in seq(1, length(ladderBottoms))){ 
  laddersByBottom[[ ladderBottoms[i] ]] <- ladderTops[i]
  laddersByTop[[ ladderTops[i] ]] <- ladderBottoms[i]
}

snakeHeads <- c(17, 64, 87, 54, 64, 93, 95, 98, 110) 
snakeTails <- c( 7, 19, 24, 34, 60, 73, 75, 79, 110) 

snakesByHead <- list()
snakesByTail <- list()
for (i in seq(1, length(snakeHeads))){ 
  snakesByHead[[ snakeHeads[i] ]] <- snakeTails[i]
  snakesByTail[[ snakeTails[i] ]] <- snakeHeads[i]
}

# Propagate the probabilities for an initial state.
# Parameters:
#   transitionMatrix : the matrix of transition probabilities forming the Markov Chain
#   state: The initial state from which the probabilities to other states have to be computed
#   stateOffset: Used only if re-rolling on 6 is enabled (see doubleRollOn6). If subsequent rolls are required propagateProps is called
#                recursively using stateOffset to indicate where the re-roll occurred
#   totalProb: The total probability (0-1.0) that is to be distributed amongst the transitional states. Can be less than 1.0 on re-rolls
#   finishExect: Boolean flag indicating whether an exact roll onto square 100 is required to win the game
#   doubleRollOn6: Boolean flag indicating that the player should take another roll every time he rolls a 6.
propagateProps <- function(transitionMatrix, state, stateOffset, totalProb, finishExact, doubleRollOn6){
  # Terminating condition
  if (state>nrow(transitionMatrix)) {
    return (transitionMatrix)
  }
  
  # Decide from where to start counting
  stateBase = state+stateOffset
  
  # Check if we are at a ladder bottom or snake head - if we are move immedately to the ladder top or the snake tail
  if (state>1){
    if ( (state-1) %in% ladderBottoms ){
      ladderTop <- laddersByBottom[[state-1]]
      transitionMatrix[state, ladderTop+1] <- totalProb
      return (transitionMatrix)
    }
    if ( (state-1) %in% snakeHeads ){
      snakeTail <- snakesByHead[[state-1]]
      transitionMatrix[state, snakeTail+1] <- totalProb
      return (transitionMatrix)
    }  
  }
  
  # Go through the 6 possible dice roll outcomes
  for (j in seq(1, 6)){
    destState<-0
    destProb<-0
    
    # Check if the roll took us to a ladder bottom or a snake head
    # totalProb/6 is the probability to be distributed 
    if ((stateBase+j-1) %in% ladderBottoms){
      ladderTop <- laddersByBottom[[stateBase+j-1]]
      destState <- ladderTop+1
      destProb <- (totalProb/6)
    } else if ((stateBase+j-1) %in% snakeHeads) {
      snakeTail <- snakesByHead[[stateBase+j-1]]
      destState <- snakeTail+1
      destProb <- (totalProb/6)
    }
    else { 
      destState <- stateBase+j
      destProb <- totalProb/6
    }
    
    # Have we gone beyond the end of the board?
    if (destState > nrow(transitionMatrix)){
      if (finishExact) {
        # We need an exact roll to win
        transitionMatrix[state, stateBase] = transitionMatrix[state, stateBase] + destProb
      } else {
        # We don't need an exact roll
        transitionMatrix[state, nrow(transitionMatrix)] <- transitionMatrix[state, nrow(transitionMatrix)] + destProb
      }
    }
    else {
      if ((j==6) && doubleRollOn6){
        # Handle double-rolls by calling ourselves recursively
        transitionMatrix <- propagateProps(transitionMatrix, state, destState-state, destProb, finishExact, doubleRollOn6)
      } else {
        transitionMatrix[state, destState] <- transitionMatrix[state, destState] + destProb
      }
    }
  }
  
  return(transitionMatrix)
}
  
# Construct a transition matrix 
# Parameters:
#   finishExact: Boolean flag indicating that an exact roll onto square 100 is required to win the game
#   doubleRollOn6: Boolean flag indicating that a player has to roll again upon rolling a 6
constructTransitionMatrix <- function(finishExact, doubleRollOn6) {
  transitionMatrix <- matrix(data=rep(0, 101*101), nrow=101, ncol=101, byrow=TRUE)
  
  # build the matrix row by tow
  for (i in seq(1, 101)){
    transitionMatrix <- propagateProps(transitionMatrix, i, 0, 1.0, finishExact, doubleRollOn6)
  }
  
  return(transitionMatrix)
}

paramMatrix = matrix(c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE), byrow=TRUE, ncol=2)

for (paramNdx in 1:nrow(paramMatrix)){
  finishExact <- paramMatrix[paramNdx,1]
  doubleRollOn6 <- paramMatrix[paramNdx,2]
  
  subTitle <- ""
  fNameSuffix <- ""
  if (finishExact) {
    subTitle <- paste(sep="", subTitle, "Exact finish")
    fNameSuffix <- paste(sep="", fNameSuffix, "_exact")
  } else {
    subTitle <- paste(sep="", subTitle, "Non-exact finish")
    fNameSuffix <- paste(sep="", fNameSuffix, "_onexact")
  }
  
  if (doubleRollOn6) {
    subTitle <- paste(sep="", subTitle, ", re-roll on 6")
    fNameSuffix <- paste(fNameSuffix, "_reroll")
  } else {
    subTitle <- paste(sep="", subTitle, ", no re-roll on 6")
    fNameSuffix <- paste(sep="", fNameSuffix, "_noreroll")  
  }
  
  cat("\nCalculating probabilities for: ", subTitle, "\n")
  transitionMatrix <- constructTransitionMatrix(finishExact, doubleRollOn6)
  
  # Initialise a MarkovChain object with our transition matrix
  mcSNL <- new("markovchain", transitionMatrix = transitionMatrix, name = "SnakesNLadders") 
  
  # Extract the matrix components and fundamental matrix
  mats <- extractMatrices(mcSNL)
  
  expected.visits = mats$N[1, ]
  
  plot(1:length(expected.visits), expected.visits, type="l", main="Expected visits per square", sub=subTitle, xlab="Square Number", ylab="Number of visits")
  
  # Set up a vector representing the intial game state with the counter off the board
  One = matrix(rep(1, 100))
  
  # Average number of moves to win
  # Every element (i,j) of the fundamental matrix N is the expected number of times that
  # state j is visited started from state i, therefore summing over j gives the expected
  # game length starting at each state i. 
  avgMovesToWin <- (mats$N %*% One)[1]
  cat("Mean number of moves to win (",subTitle,"): ", avgMovesToWin, "\n")
  
  # Calculate the probability of winning vs. number of moves
  # The vector of probabilities at the starting state
  initProb <- rep(0, 101)
  initProb[1] <- 1
  
  havingWonProbs=c() # probability of having won after n moves
  winInNProbs=c() # Probability of winning on the nth move
  
  # Acuumulate the transition matrix probabilties saving the probability of
  # having reached the ending state
  maxMoves <- 250
  cumul = initProb
  for (k in seq(1, maxMoves)){
    cumul <- cumul * mcSNL
    
    #probs <- initProb * cumul
    
    havingWonProbs <- c(havingWonProbs, cumul[101])
    if (k>1){
      # The probability of winning on the kth move is the probability of having won on the kth - probability of having one on the k-1th move
      winInNProbs <- c(winInNProbs, cumul[101]-havingWonProbs[k-1])
    } else {
      winInNProbs <- c(0)
    }
  }
  
  medianMovesToWin <- which.min(abs(havingWonProbs-0.5))
  cat("Median number of moves to win: (",subTitle,") ", medianMovesToWin, "\n")
  
  modalMovesToWin <- which.max(winInNProbs)
  cat("Modal number of moves to win: (",subTitle,") ", modalMovesToWin, "\n")
  
  # Plot the probability of having won after N moves
  plot(seq(1,maxMoves), havingWonProbs, type="l", main="Cumulative winning probability", xlab="Number of moves", ylab="Probability", sub=subTitle)
  pdf(file = paste(sep="", "cumul_prob", fNameSuffix, ".pdf"))
  plot(seq(1,maxMoves), havingWonProbs, type="l", main="Cumulative winning probability", xlab="Number of moves", ylab="Probability", sub=subTitle)
  dev.off()
  
  # Plot the probability of winning in N moves
  plot(seq(1,maxMoves), c(0, diff(havingWonProbs)), type="l", main="Winning probability", xlab="Move number", ylab="Probability", sub=subTitle)
  pdf(file = paste(sep="", "win_prob", fNameSuffix, ".pdf"))
  plot(seq(1,maxMoves), c(0, diff(havingWonProbs)), type="l", main="Winning probability", xlab="Move number", ylab="Probability", sub=subTitle)
  dev.off()
  
  # reverse even rows so as to make matrix scan left/right like the Snakes and Ladders board.
  adjustStateMatrix <- function(stateMat){
    rowsToReverse = seq(2, 10, 2)
    
    for (row in rowsToReverse){
      stateMat[row,] <- rev(stateMat[row,])
    }
    
    return(stateMat)
  }
  
  # 
  snlgraph <- graph_from_adjacency_matrix(transitionMatrix>0, mode="directed")
  shortest_state <- rep(0, 101)
  
  options(warn=-1)
  shortest_state[get.shortest.paths(snlgraph, from=1, mode="out")$vpath[[101]]]=1
  
  shortest_state_mat = adjustStateMatrix(matrix(shortest_state[2:length(shortest_state)], nrow=10, ncol=10, byrow = TRUE))
  
  pdf(file = paste(sep="", "shortest_game", fNameSuffix, ".pdf"))
  anImage <- readJPEG("snakesnLadders3.jpg")
  # Set up a plot area with no plot
  plot(x=0:1, y=0:1, type='n', main="", xlab="x", ylab="y")
  par.old <- par()
  par(mar=rep(0,4))
  # Get the plot information so the image will fill the plot box, and draw it
  lim <- par()
  rasterImage(anImage, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4],)
  
  image(t(apply(shortest_state_mat[nrow(shortest_state_mat):1,], 2, rev)), col=c("transparent", 1), axes=FALSE, add=TRUE, useRaster=TRUE)
  dev.off()
  par(par.old)
  options(warn=0)
}