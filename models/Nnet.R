load("data/bike_clean.Rdata")

library(caret)
library(tictoc)
library(tidyverse)
set.seed(7)

# To stop Parallell Computing
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
library(tictoc)
library(caret)

## Use parallel computing in R to speed up computations
library(doParallel)                    ## for parallel computing
cl<-makePSOCKcluster(detectCores())    ## for parallel computing  
## socket cluster with 8 nodes on host ‘localhost’
registerDoParallel(cl)                 ## for parallel computing

tic()
set.seed(9973)
nn10F <- train(
  y = bike_train$rented_bikes,
  x = bike_train[,2:15],
  method = "avNNet",
  trace=FALSE, linout=TRUE,
  preProc = c("center","scale"), maxit=500, repeats=5, 
  tuneGrid = expand.grid(
    size = seq(1,20,length.out=10),     # Number of neurons in the hidden layer
    decay = seq(0.2,0.9,length.out=8),
    bag=FALSE),
  trControl=trainControl(method = "repeatedcv",repeats=2,number=10))
toc() # 19588.83 sec

stopCluster(cl)   ## for parallel computing
unregister_dopar()

nn10F # size=20, decay=.3, RMSE= 284.4240
plot(nn10F)

nn_test_pred <- predict(nn10F, bike_test)
nn_test_rmse <- RMSE(nn_test_pred, bike_test$rented_bikes)
nn_test_rmse # 
