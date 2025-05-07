load("data/top5Pred.Rdata")
load("data/top5Pred_test.Rdata")
load("data/yhats_BART.Rdata")
library(caret)
library(tictoc)
library(tidyverse)
library(randomForest)
library(xtable)

preddf_test<-data.frame(top5Pred_test,BART=yhats_BART_test)
preddf<-data.frame(top5Pred[,-2])

set.seed(777)
tic()
stacked<-train(y=preddf$y, x=preddf[,2:6], method="rf",
              tuneGrid=data.frame(.mtry=1:5),importance=T,
              trControl=trainControl(method="oob"),nodesize=5) # m OOB selected, nmin=5
toc() # 642.35 seconds

stacked # RMSE = 23.89370  

RF_test_pred <- predict(stacked, preddf_test)
RF_test_rmse <- RMSE(RF_test_pred, bike_test$y)
RF_test_rmse # 
