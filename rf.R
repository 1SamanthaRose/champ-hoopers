# installing packages
install.packages("randomForest")
install.packages("ROCR")
install.packages("randomForestExplainer")

# accessing packages
library(randomForest)
library(tidyverse)
library(dplyr)
library(ROCR)
library(randomForestExplainer)

# reading in excel file
new <- read_csv("newstats.csv")
head(new)

#summary of data set to see if there is any null values
summary(new) 

#shows that there is no null values in all columns 

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
set.seed(234578)

#changing dependent variable to categorical if using classification
#box$column_name = as.factor(box$column_name)

#starting Random Forest

#can specify type of random forest using type="regression"
#can alter number of trees using ntree=#
#using wins as the response variable
rf <- randomForest(wins~.,data=new,ntree=500,)
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
#can change mtry manually
set.seed(234578)
rf <-randomForest(wins~., data=new, mtry=best.m, importance=TRUE, ntree=500)
print(rf)

#final model
rf <-randomForest(wins ~ stl + off_tovp + off_efgp + blk
+ def_efgp + def_tovp + def_rp + ftp, data=new, mtry=2, importance=TRUE, ntree=500)

#Evaluate variable importance
importance(rf)
varImpPlot(rf)


################################################################

#using random forest explainer

#measuring variable importance
#importance_frame <- measure_importance(rf)

#ploting multi-way importance plot for variables
#plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

#comparing measures
#plot_importance_ggpairs(importance_frame)

#comparing different rankings
#plot_importance_rankings(importance_frame)

################################################################

#prediction and calculation of performance metrics
#used only to see accuracy if using classification method
#pred1=predict(rf,type = "prob")
#perf = prediction(pred1[,], box$pts)

# 1. Area under curve
#auc = performance(perf, "auc")
#auc

# 2. True Positive and Negative Rate
#pred3 = performance(perf, "tpr","fpr")

# 3. Plot the ROC curve
#plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
#abline(a=0,b=1,lwd=2,lty=2,col="gray")

###########################################################

#starting simulation




