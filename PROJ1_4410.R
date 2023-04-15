# Paige Reynolds
# Intro to Stochastic
# Project 1

# Problem 1
# Game 1: P(at least 1 six in 4 rolls) = 1 - P(no six in 4 rolls)
# = 1 - (5/6)^4 = .518

# Problem 2
# Game 2: P(at least 1 double six in 24 rolls) = 1 - P(no double six in
# 24 rolls) = 1 - (35/36)^24 = .491

# Problem 3

# This function generates a random number that represents
# rolling one fair, six sided dice 4 times
dice1 <- function() {
  d = sample(1:6, 4, replace=TRUE)
  return(d)
  
}

dice1()

# This function runs the first experiment once and returns TRUE1
# for when there is a 6 in the 4 dice rolls or 0 when there is not
game1 <- function() {
  diceroll <- dice1()
  win <- sum(diceroll == 6) > 0
  return(win)
}

# This function is used to replicates the experiment above n times (n being
# the first argument). The replication allows for a Monte Carlo simulation because
# game1 uses a random set of 4 dice rolls each n experiments.
# The variable MonteCarlo will be length n and only
# hold values TRUE(1) (a 6 was rolled) or FALSE(0) (a 6 was not rolled) depending 
# on the outcome of each experiment.
# The sum of successes (1) divided by n will give the probability
# of success (rolling a 6). As n increases, the  calculated probability gets
# closer to the true probability calculated in Problem 1.

MonteCarlo <- replicate(10, game1())
sum(MonteCarlo)/length(MonteCarlo)

MonteCarlo <- replicate(100, game1())
sum(MonteCarlo)/length(MonteCarlo)

MonteCarlo <- replicate(1000, game1())
sum(MonteCarlo)/length(MonteCarlo)

MonteCarlo <- replicate(10000, game1())
sum(MonteCarlo)/length(MonteCarlo)

MonteCarlo <- replicate(100000, game1())
sum(MonteCarlo)/length(MonteCarlo)


# These functions are used to roll 2 separate, fair, six sided dice 24
# times. 
dice2 <- function() {
  d = sample(1:6, 24, replace=TRUE)
  return(d)
}

dice3 <- function() {
  x = sample(1:6, 24, replace=TRUE)
  return(x)
}

# This function is used to run the experiment for game 2 one time.
# It rolls both of the dice and returns 1 for a double 6
game2 <- function(){
  die1 <- dice2()
  die2 <- dice3()
  win <- sum((die1 == 6) & (die1 == die2)) > 0
  return(win)
}

# This function is used to replicate the experiment above n times (n being
# the first argument). Just like MonteCarlo, MonteCarlo2 is used to calculate
# the probability of rolling a double 6 given n replications. 
# When n is 10000, the calculated probability is very close to the true 
# probability.

MonteCarlo2 <- replicate(10, game2())
sum(MonteCarlo2)/length(MonteCarlo2)

MonteCarlo2 <- replicate(100, game2())
sum(MonteCarlo2)/length(MonteCarlo2)

MonteCarlo2 <- replicate(1000, game2())
sum(MonteCarlo2)/length(MonteCarlo2)

MonteCarlo2 <- replicate(10000, game2())
sum(MonteCarlo2)/length(MonteCarlo2)
