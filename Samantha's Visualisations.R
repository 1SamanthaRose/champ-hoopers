#Samantha's Visualisations

#########Team Stats########


####WEST CONF####
# phnx vs. neworleans                PHX
# 78.14% vs. 21.87 %
                                            #PHX
#dallas vs. utah
# 29.24%    70.77%                   UTAH
                                                     #MEM
#gsw     vs  denver
# 66.87%     33.14%                  GSW     

                                            #MEM
#mem     vs    min                   MEM
#59.74         40.27               

#####EAST CONF######                                                #MEMPHIS CHAMPIONS

#ALT    VS    MIA                   ATL
#57.39%      42.6
                                            #PHI
#PHI    VS    TOR                   PHI
#60.58%       39.43%

                                                   #PHI
#MIL    VS   CHI
#57.82%      42.19%                 MIL

                                           #BOS
#BOS    VS  BKN                     BOS
#62.38%     37.63%



##############Accuracy Check##############

#Ten trials. This shows accuracy.

###MEMPHIS VS MINNEAPOLIS##

###########1 
#60.21  v. 39.8


###########2
#60.25%   vs  39.76%

###########3
#59.71     v.    40.3%

###########4
#59.84      v.     40.17


############5
#59.77      v.   40.24

##########6
#60.62        v.        39.39


###########7
#60.35       v.     39.66

############8
#59.51        v.     40.5

###########9
#60.38        v.      39.63

#############10
#60.24        v.      39.77


#Creating out df to plot each trial

library(ggplot2)

Trial <- c(1,2,3,4,5,6,7,8,9,10)
Memphis_Probability <- c(60.21, 60.25,59.71, 59.84, 59.77, 60.62, 60.35, 59.51, 60.38, 60.24)
Minneapolis_Probability <- c(39.80, 39.76, 40.30, 40.17, 40.24, 39.39, 39.66, 40.5, 39.63, 39.77)
trialdf <- data.frame(Trial, Memphis_Probability, Minneapolis_Probability)

View(trialdf)

#Histogram of The Trials

q <- ggplot(trialdf) +
  geom_line(aes(Trial, Memphis_Probability), color = "purple", size = 1) +
  geom_line(aes(Trial, Minneapolis_Probability), color = "blue", size = 1) +
  coord_cartesian(ylim = c(0,100)) +
  labs(title = "Comparing Run Results-How Precise Are We?", xlab = "Probability") +
  theme(panel.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
        panel.grid.major = element_line(color = 'black', linetype = 'dotted'),
        panel.grid.minor = element_line(color = 'ivory', linetype = 'dotted'))
q


g <- ggplot(trialdf) +
  geom_line(aes(Trial, Memphis_Probability), color = "purple", size = 1) +
  geom_line(aes(Trial, Minneapolis_Probability), color = "blue", size = 1) +
  coord_cartesian(ylim = c(35,65)) +
  labs(title = "Comparing Run Results-How Precise Are We?", xlab = "Probability") +
  theme(panel.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
        panel.grid.major = element_line(color = 'black', linetype = 'dotted'),
        panel.grid.minor = element_line(color = 'ivory', linetype = 'dotted'),
        text = element_text(family = "Utopia"))
g

