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
set.seed(1999)





## Install the earth package in R
install.packages("earth")
library(earth)

## Function for unregistering parallel computing; use unregister_dopar() 
## after parallel computing
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

## Use parallel computing in R to speed up computations
library(doParallel)                    ## for parallel computing
cl<-makePSOCKcluster(detectCores())    ## for parallel computing  
## socket cluster with 8 nodes on host ‘localhost’
registerDoParallel(cl)                 ## for parallel computing

date()
MARS.grid<-expand.grid(nprune=c(1:30), degree=c(1:3))
MARSTune.LOO<-train(y=housing_dat[,1], x=housing_dat[,2:12], method="earth",
                    preProcess = c("center","scale"),  ## standardize input variables
                    tuneGrid=MARS.grid,               
                    trControl=trainControl(method="LOOCV"))
date()
stopCluster(cl)                       ## for parallel computing
unregister_dopar()

MARSTune.LOO$results
plot(MARSTune.LOO)
MARSTune.LOO$bestTune
#    nprune degree
# 43     15      1
min(MARSTune.LOO$results$RMSE)
# [1] 15864.31

## degree: Maximum degree of interaction (Friedman’s mi). Default is 1, 
##         meaning to build an additive model (i.e., no interaction terms).
## nprune: Maximum number of terms (including intercept) in the pruned model. 