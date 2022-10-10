# installing packages
install.packages("randomForest")

# accessing packages
library(randomForest)
library(tidyverse)
library(dplyr)

# reading in excel file
new <- read_csv("newstats.csv")
head(new)

#summary of data set to see if there is any null values
summary(new) 

#removing any columns from data if needed
#new <- new %>%
  #select()

#removing any rows from data if needed
#new <- new[-c(31),]

#reusing summary to see if data needs anymore adjustment
summary(new)

#data set now cleaned and prep to start training
#choosing variables that seem significant
#new <- new %>%
  #select()

#checking type of variables of each columns
str(new)

#checking dimension of data frame
dim(new)

#setting random seed
set.seed(303)

#### starting Random Forest ####

#can specify type of random forest using type="regression"
#can alter number of trees using ntree=#
#using wins as the response variable
rf <- randomForest(wins ~ stl + off_tovp + off_efgp + ftp +
                     blk + def_rp,data=new,ntree=500,)
print(rf)

#Evaluate variable importance before best mtry
importance(rf)
varImpPlot(rf)

#number of variables selected at each split is 'mtry'
mtry <- tuneRF(new[-1], new$wins, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)

#building model using best mtry value
#can change mtry manually if needed
set.seed(303)
rf <-randomForest(wins ~ stl + off_tovp + off_efgp + ftp +
                    blk + def_rp, data=new, mtry=best.m, importance=TRUE, ntree=500)
print(rf)

###################################  final model  ###################################
rf<-randomForest(wins ~ stl + off_tovp + off_efgp + ftp + blk
+ def_rp, data=new, mtry=2, importance=TRUE, ntree=500)

print(rf)

#Evaluate variable importance
importance(rf)
varImpPlot(rf)






