box <- read.csv('boxs.csv')

w1 = 0.55
w2 = 0.19
w3 = 0.10
w4 = 0.07
w5 = 0.05
w6 = 0.04
#it may be cleaner to instead just manually input the
  #weight values into the score formula seen later
#these are just hypothetical weights right now for testing
  #the code

#separates each team into its own dataset
  #there is probably a cleaner way to do this
mia <- box[ which(box$team=='MIA'),]
atl <- box[ which(box$team=='ATL'),]
phi <- box[ which(box$team=='PHI'),]
tor <- box[ which(box$team=='TOR'),]
mil <- box[ which(box$team=='MIL'),]
chi <- box[ which(box$team=='CHI'),]
bos <- box[ which(box$team=='BOS'),]
bkn <- box[ which(box$team=='BKN'),]

phx <- box[ which(box$team=='PHX'),]
nop <- box[ which(box$team=='NOP'),]
dal <- box[ which(box$team=='DAL'),]
uta <- box[ which(box$team=='UTA'),]
gsw <- box[ which(box$team=='GSW'),]
den <- box[ which(box$team=='DEN'),]
mem <- box[ which(box$team=='MEM'),]
min <- box[ which(box$team=='MIN'),]


seriesCount = 0
ASerWon = 0 #Keeps track of numbers of series won so far by each team
BSerWon = 0
while (seriesCount < 10000) { #iterates 10,000 times
  teamA = mia
  teamB = atl
    #insert teams of choice into here
    #the above part probably needs to be a function of some 
      #sort, but I haven't figured out how to do that yet
  AGameWon = 0 #Keeps track of number of games won so far by
    #each team
  BGameWon = 0
  while ((AGameWon < 4) & (BGameWon < 4)) { #iterates until one 
    #team has won 4 games 
  #chooses a reg season game at random for each team from the box score dataset
    gameA = teamA[sample(1:nrow(teamA), 1),] 
    gameB = teamB[sample(1:nrow(teamB), 1),]
  #the values of important predictors as indicated by the forest, for that game
  #the predictors listed there right now are just for testing
    #this code, not necessarily the ones that will be used
  #might not necessarily use exactly 4
    p1A = gameA$efgp
    p2A = gameA$tovp
    p3A = gameA$stl
    p4A = gameA$drebp
    p5A = gameA$blk
    p6A = gameA$ftp
    p1B = gameB$efgp
    p2B = gameB$tovp
    p3B = gameB$stl
    p4B = gameB$drebp
    p5B = gameB$blk
    p6B = gameB$ftp
  #this adds together all of Team A's stats in proportion to the total stats
  #for both teams, weighing them appropriately. This will always sum to an amount from 0 to 1.
  #w1-4 are the standardized weights for the respective predictors
    score = ((p1A / (p1A + p1B)) * w1
+ (p2A / (p2A + p2B)) * w2
+ (p3A / (p3A + p3B)) * w3
+ (p4A / (p4A + p4B)) * w4
+ (p5A / (p5A + p5B)) * w5
+ (p6A / (p6A + p6B)) * w6)
#Team A wins if the value is greater than 0.5, Team B wins if the value is less
#than 0.5, on the off chance the score is exactly 0.5 just run it again and 
#that game doesn't count.
    if (score > 0.5) {
      AGameWon = AGameWon + 1
    }
    if (score < 0.5) {
      BGameWon = BGameWon + 1
    }
#nested while loop ends here once a team has won 4 sim games in the series
  }
  if (AGameWon > BGameWon) {
    ASerWon = ASerWon + 1
  }
  if (BGameWon > AGameWon) {
    BSerWon = BSerWon + 1
  }
  seriesCount = seriesCount + 1
}#outer loop ends once 10,000 series have been simulated
ASerWon
BSerWon
#this reports the favored team and said team's win probability
#right now it displays the message 82 times, i haven't figured out yet
  #how to get it to display properly
if (ASerWon > BSerWon) {
  winProb = ASerWon / 100
  paste(teamA$team, 'has a', winProb, '% win probability')
}
if (BSerWon > ASerWon) {
  winProb = BSerWon / 100
  paste(teamB$team, 'has a', winProb, '% win probability')
}
if (ASerWon == BSerWon) {
  print('Could go either way, happy gambling!')
}
