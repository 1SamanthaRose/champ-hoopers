#reads the box score data, and subsets it into a data set with only
#the team names and the six predictors that we will be using
#box <- read.csv('boxs.csv')
box6 <- box[,c(1, 35, 34, 15, 32, 20, 21)]

#the weights of the six predictors, as determined by the
#standardized importance scores given by the random forest,
#rounded to two decimal places
w1 = 0.39 #EFGP
w2 = 0.18 #TOVP
w3 = 0.11 #FTP
w4 = 0.14 #DREBP
w5 = 0.14 #STL
w6 = 0.04 #BLK

#separates each playoff team into its own dataset, for
#easier access later
mia <- box6[ which(box6$team=='MIA'),]
atl <- box6[ which(box6$team=='ATL'),]
phi <- box6[ which(box6$team=='PHI'),]
tor <- box6[ which(box6$team=='TOR'),]
mil <- box6[ which(box6$team=='MIL'),]
chi <- box6[ which(box6$team=='CHI'),]
bos <- box6[ which(box6$team=='BOS'),]
bkn <- box6[ which(box6$team=='BKN'),]
phx <- box6[ which(box6$team=='PHX'),]
nop <- box6[ which(box6$team=='NOP'),]
dal <- box6[ which(box6$team=='DAL'),]
uta <- box6[ which(box6$team=='UTA'),]
gsw <- box6[ which(box6$team=='GSW'),]
den <- box6[ which(box6$team=='DEN'),]
mem <- box6[ which(box6$team=='MEM'),]
min <- box6[ which(box6$team=='MIN'),]

simSeries <- function(teamA, teamB) { #insert teams of choice into here
  #Keeps track of numbers of series won so far by each team
  ASerWon = 0 
  BSerWon = 0
  seriesCount = 0
  while (seriesCount < 10000) { #iterates 10,000 times
    #Keeps track of number of games won so far by each team
    AGameWon = 0 
    BGameWon = 0
    while ((AGameWon < 4) & (BGameWon < 4)) { #iterates until one team has won 4 games 
        #randomly samples a regular season game for team A
        gameA = teamA[sample(1:nrow(teamA), 1),] 
        #the values of each predictor from team A's sampled game
        p1A = gameA$efgp
        p2A = 100 - gameA$tovp #gives the percentage of possesions NOT resulting in a turnover
        p3A = gameA$ftp
        p4A = gameA$drebp
        p5A = gameA$stl
        p6A = gameA$blk
        if (p6A == 0) {
        p6A = 0.0000000000001 #occasionally both teams have zero blocks, 
        #so this offsets a potential division by zero error later on
        } 
        #does the same as above for team B
        gameB = teamB[sample(1:nrow(teamB), 1),]
        p1B = gameB$efgp
        p2B = 100 - gameB$tovp
        p3B = gameB$ftp
        p4B = gameB$drebp
        p5B = gameB$stl
        p6B = gameB$blk
        if (p6B == 0) {
            p6B = 0.0000000000001
        }
        #For each predictor, this determines the value of that predictor for Team A's in proportion 
        #to the sum of that value for both teams, weighing that predictor appropriately. Then this
        #sums all of the weighted proportions. This will always sum to an amount from 0-1.
        score = (
          (p1A / (p1A + p1B)) * w1
        + (p2A / (p2A + p2B)) * w2
        + (p3A / (p3A + p3B)) * w3
        + (p4A / (p4A + p4B)) * w4
        + (p5A / (p5A + p5B)) * w5
        + (p6A / (p6A + p6B)) * w6
        )
        #Team A wins the simulated if score is greater than 0.5, Team B wins the game if score is 
        #less than 0.5. On the off chance that score is exactly 0.5, nothing is incremented.
        if (score > 0.5) {
            AGameWon = AGameWon + 1
        }
        if (score < 0.5) {
            BGameWon = BGameWon + 1
        }
    #nested while loop ends here once a team has won four sim games in the series
    }
    #increments the total series won for the team that won four games
    if (AGameWon > BGameWon) {
        ASerWon = ASerWon + 1
    }
    if (AGameWon < BGameWon) {
        BSerWon = BSerWon + 1
    }
    seriesCount = seriesCount + 1
  #outer loop ends once 10,000 series have been simulated
  }
  #this reports each team's win probability, based on the proportion of 
  #series won by that team
  if (ASerWon > BSerWon) {
    winner = teamA
    loser = teamB
  }
  if (ASerWon < BSerWon) {
    winner = teamB
    loser = teamA
  }
  if (ASerWon != BSerWon) {
    winProb = max(ASerWon,BSerWon) / 100
    loseProb = min(ASerWon,BSerWon) / 100
    paste(winner[1,]$team, 'has a', winProb, '% win probability',
    'and', loser[1,]$team, 'has a', loseProb, '% win probability')
  } else {
    print('Could go either way, happy gambling!')
    }
}#end of sim function


