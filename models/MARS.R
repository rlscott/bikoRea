# -----------------------------------------------------------------
#
#   Stat 602 - Machine Learning
#   Final Project - bikoRea
#   MARS 2 F, 1 repeats CV | Rebekah Scott 
#
# -----------------------------------------------------------------

# load in data
load("data/bike_clean.Rdata")

# setup 
library(caret)
library(earth)
set.seed(1999)


# Function for unregistering parallel computing; use after parallel computing
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

# Use parallel computing To speed up computations
library(doParallel)                    
cl<-makePSOCKcluster(detectCores())    
registerDoParallel(cl) # socket cluster with 8 nodes on host ‘localhost’

date()
MARS.grid<-expand.grid(nprune=c(1:30), degree=c(1:3))
MARSTune <-train(y=bike_train$rented_bikes, x=bike_train[,2:15], method="earth",
                    preProcess = c("center","scale"),  ## standardize input variables
                    tuneGrid=MARS.grid,
                 trControl=trainControl(method="repeatedcv", repeats=10, number=10))
date()
stopCluster(cl)
unregister_dopar()

MARSTune$results
plot(MARSTune)
MARSTune$bestTune
# nprune degree
# 85     25      3
min(MARSTune$results$RMSE)
# [1] 293.8558

MARSTune$finalModel

# Notes from Dr. Wu: 
## degree: Maximum degree of interaction (Friedman’s mi). Default is 1, 
##         meaning to build an additive model (i.e., no interaction terms).
## nprune: Maximum number of terms (including intercept) in the pruned model. 