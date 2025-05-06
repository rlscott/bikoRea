# -----------------------------------------------------------------
#
#   Stat 602 - Machine Learning
#   Final Project - bikoRea
#   shrinkage regressions - ben 
#
# -----------------------------------------------------------------

# load in data
load("C://Users//blj12//OneDrive//STAT S25//STAT 6020//bike_clean.Rdata")

# setup 
library(caret)

# 10 fold CV 
set.seed(1999)
lasso_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=as.matrix(bike_train[,2:15]),
                               nfold=10,lambda=exp(seq(-6,6,.01)))
lasso_10F # best is lambda=.092, RMSE=sqrt(190064)=435.9633

set.seed(428062)  
ridge_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=as.matrix(bike_train[,2:15]),
                               nfold=10,lambda=exp(seq(-6,6,.01)),
                               alpha=0)
ridge_10F # best is lambda=3.49, RMSE=sqrt(189937)=435.8176

set.seed(1800415)
enet.25_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=as.matrix(bike_train[,2:15]),
                                 nfold=10,lambda=exp(seq(-6,6,.01)),
                                 alpha=.25)
enet.25_10F 
# Lambda Index Measure   SE Nonzero
# min   1.08   593  190090 5541      14
set.seed(78234)
enet.5_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=as.matrix(bike_train[,2:15]),
                                nfold=10,lambda=exp(seq(-6,6,.01)),
                                alpha=.5)
enet.5_10F 
# Lambda Index Measure   SE Nonzero
# min  0.463   678  190118 5124      14
enet.75_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=as.matrix(bike_train[,2:15]),
                                 nfold=10,lambda=exp(seq(-6,6,.01)),
                                 alpha=.75)
enet.75_10F 
# Lambda Index Measure   SE Nonzero
# min   0.11   822  190109 3193      14


set.seed(8100964)
lm_10F <- train(y=bike_train$rented_bikes,x=bike_train[,2:15],
                 method="lm",
                 preProcess=c("center","scale"),
                 trControl=trainControl(method="repeatedcv", 
                                        repeats=10, number=10))

lm_10F$results # RMSE=435.7456 

# repeat with some feature engineering... adding the second order terms

form <- as.formula(paste0(' ~ .^2 + ', paste(sapply(colnames(bike_train)[-1], \(x) paste0('I(', x, '^2)')), collapse=' + ')))
XX <- model.matrix(form, bike_train[,-1])
set.seed(8364201)
lasso_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=XX,
                               nfold=10,lambda=exp(seq(-6,6,.01)))
lasso_10F # best is lambda=0.0025, RMSE=sqrt(129977)=360.5232

set.seed(1940955)
ridge_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=XX,
                               nfold=10,lambda=exp(seq(-6,6,.01)),
                               alpha=0)
ridge_10F # best is lambda=.002, RMSE=sqrt(129618)=360.025

set.seed(1800415)
enet.25_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=XX,
                                 nfold=10,lambda=exp(seq(-6,6,.01)),
                                 alpha=.25)
enet.25_10F 
# Lambda Index Measure   SE Nonzero
# min  0.002  1201  131277 5463     114

set.seed(78234)
enet.5_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=XX,
                                nfold=10,lambda=exp(seq(-6,6,.01)),
                                alpha=.5)
enet.5_10F 
# Lambda Index Measure   SE Nonzero
# min 0.0025  1201  129022 3680     114
enet.75_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=XX,
                                 nfold=10,lambda=exp(seq(-6,6,.01)),
                                 alpha=.75)
enet.75_10F 
# Lambda Index Measure   SE Nonzero
# min  0.002  1201  130718 5712     114

set.seed(48794)
keep <- apply(X=XX,MARGIN=2,FUN=var)!=0
lm_10F <- train(y=bike_train$rented_bikes,x=XX[,keep],
                method="lm",
                preProcess=c("center","scale"),
                trControl=trainControl(method="repeatedcv", 
                                       repeats=10, number=10))
lm_10F$results # RMSE=357.5606 


### repeat this all with poisson glm
set.seed(2584275)
lasso_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=XX,
                               nfold=10,lambda=exp(seq(-6,6,.01)),
                               family="poisson",type.measure="mse")
lasso_10F # 0.4966, RMSE=sqrt(115775)=340.2573

set.seed(9842712)
ridge_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=XX,
                               nfold=10,lambda=exp(seq(-6,6,.01)),
                               alpha=0,family="poisson",type.measure="mse")
ridge_10F # 2.945, RMSE=sqrt(114632)=338.5735

set.seed(1800415)
enet.25_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=XX,
                                 nfold=10,lambda=exp(seq(-6,6,.01)),
                                 alpha=.25,family="poisson",type.measure="mse")
enet.25_10F 
# Lambda Index Measure   SE Nonzero
# min  4.393   453  118924 4571      78
set.seed(2139783)
enet.5_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=XX,
                                nfold=10,lambda=exp(seq(-6,6,.01)),
                                alpha=.5,family="poisson",type.measure="mse")
enet.5_10F 
# Lambda Index Measure   SE Nonzero
# min  2.075   528  118062 3377      77
enet.75_10F <- glmnet::cv.glmnet(y=bike_train$rented_bikes,x=XX,
                                 nfold=10,lambda=exp(seq(-6,6,.01)),
                                 alpha=.75,family="poisson",type.measure="mse")
enet.75_10F 
# Lambda Index Measure   SE Nonzero
# min  3.096   488  121782 2269      57


##### xgboost
set.seed(9973)
XGB.grid<-expand.grid(nrounds=(2:4)*50, max_depth=1:3, eta=c(0.05,0.1,0.2),  
                      colsample_bytree=c(0.3,0.4,0.5), subsample=c(0.7,0.8,0.9),
                      gamma=c(0,1,2), min_child_weight=1)
XGBTune<-train(y=bike_train$rented_bikes, x=XX[,keep],method="xgbTree",
               verbose=FALSE, verbosity=0, tuneGrid=XGB.grid, 
               ## trControl=trainControl(method = "LOOCV"))
               trControl=trainControl(method="repeatedcv",repeats=1,number=10))
XGBTune
XGBTune$results[which.min(XGBTune$results$RMSE),]
# eta max_depth gamma colsample_bytree min_child_weight subsample nrounds     RMSE  Rsquared
# 0.2         3     2              0.5                1       0.9     200 232.6101 0.8704037
# MAE   RMSESD RsquaredSD    MAESD
# 152.7971 16.67524 0.01650026 8.283144

set.seed(964973)
XGB.grid<-expand.grid(nrounds=(2:4)*50, max_depth=1:3, eta=c(0.05,0.1,0.2),  
                      colsample_bytree=c(0.3,0.4,0.5), subsample=c(0.7,0.8,0.9),
                      gamma=c(0,1,2), min_child_weight=1)
XGBTune<-train(y=bike_train$rented_bikes, x=bike_train[,2:15],method="xgbTree",
               verbose=FALSE, verbosity=0, tuneGrid=XGB.grid, 
               ## trControl=trainControl(method = "LOOCV"))
               trControl=trainControl(method="repeatedcv",repeats=1,number=10))
XGBTune
vI <- varImp(XGBTune)
xtable::xtable( varImp(XGBTune)$importance )
XGBTune$results[which.min(XGBTune$results$RMSE),]
# eta max_depth gamma colsample_bytree min_child_weight subsample nrounds     RMSE  Rsquared
# 0.2         3     2              0.5                1       0.9     200 238.3112 0.8637425
# MAE   RMSESD RsquaredSD    MAESD
# 158.7957 8.433503 0.01413286 4.223523

##### BART
library(BART)

set.seed(2350187)
MSEs <- rep(NA,10)
folds <- sample(1:10,size=length(bike_train$rented_bikes),replace=T)
preds <- rep(NA,length(bike_train$rented_bikes)); idx <- 1
for(k in 1:10){
  bart <- wbart(x.train=bike_train[folds!=k,2:15], 
                y.train=bike_train$rented_bikes[folds!=k],
                x.test=bike_train[folds==k,2:15])
  MSEs[k] <- mean( (bart$yhat.test.mean-bike_train$rented_bikes[folds==k])^2 ) 
  preds[idx:(idx+sum(folds==k)-1)] <- bart$yhat.test.mean; idx <- idx+sum(folds==k)
}
sqrt( mean(MSEs) )# 234.865

set.seed(2350187)
MSEs <- rep(NA,10)
folds <- sample(1:10,size=length(bike_train$rented_bikes),replace=T)
preds <- rep(NA,length(bike_train$rented_bikes)); idx <- 1
for(k in 1:10){
  bart <- wbart(x.train=bike_train[folds!=k,2:15], 
                y.train=bike_train$rented_bikes[folds!=k],
                x.test=bike_train[folds==k,2:15],
                sparse=T)
  MSEs[k] <- mean( (bart$yhat.test.mean-bike_train$rented_bikes[folds==k])^2 ) 
  preds[idx:(idx+sum(folds==k)-1)] <- bart$yhat.test.mean; idx <- idx+sum(folds==k)
}
sqrt( mean(MSEs) ) #232.7578


## CV over 27 parameter combos, using sparse=T
set.seed(463905)
bases <- seq(.75,.95,.1)
ntrees <- seq(100,300,100)
ks <- c(1,2,3)
results <- data.frame(base=c(),ntrees =c(),k=c(), MSE=c())
for(b in 1:length(bases)){
  for(nt in 1:length(ntrees)){
    for(kk in 1:length(ks)){
      MSEs <- rep(NA,10)
      folds <- sample(1:10,size=length(bike_train$rented_bikes),replace=T)
      for(k in 1:10){
        print(paste0("BEGIN base=",bases[b],", ntree=",ntrees[nt],", k=",ks[kk],", fold=",k))
        bart <- wbart(x.train=bike_train[folds!=k,2:15], 
                      y.train=bike_train$rented_bikes[folds!=k],
                      x.test=bike_train[folds==k,2:15],
                      sparse=T,
                      base=bases[b],
                      ntree=ntrees[nt],
                      k=ks[kk])
        MSEs[k] <- mean( (bart$yhat.test.mean-bike_train$rented_bikes[folds==k])^2 ) 
      }
      results <- rbind(results, c( bases[b] , ntrees[nt] , ks[kk], mean(MSEs) ) ) 
    }
  }
}
results
# X0.75 X100 X1 X55482.8521052717
# 1   0.75  100  1          55482.85
# 2   0.75  100  2          55032.12
# 3   0.75  100  3          54677.08
# 4   0.75  200  1          54638.68
# 5   0.75  200  2          54823.48
# 6   0.75  200  3          53372.03 # best model, RMSE=231.0239
# 7   0.75  300  1          54772.95
# 8   0.75  300  2          53624.08
# 9   0.75  300  3          54189.81
# 10  0.85  100  1          57084.64
# 11  0.85  100  2          54125.55
# 12  0.85  100  3          54512.13
# 13  0.85  200  1          53900.57
# 14  0.85  200  2          54652.36
# 15  0.85  200  3          53610.87
# 16  0.85  300  1          53670.18
# 17  0.85  300  2          53612.80
# 18  0.85  300  3          54098.40
# 19  0.95  100  1          56263.97
# 20  0.95  100  2          54694.84
# 21  0.95  100  3          54213.33
# 22  0.95  200  1          54191.63
# 23  0.95  200  2          53896.84
# 24  0.95  200  3          54682.37
# 25  0.95  300  1          54911.27
# 26  0.95  300  2          53755.41
# 27  0.95  300  3          53418.65


## CV over 27 parameter combos, using sparse=F
set.seed(463905)
bases <- seq(.75,.95,.1)
ntrees <- seq(100,300,100)
ks <- c(1,2,3)
results <- data.frame(base=c(),ntrees =c(),k=c(), MSE=c())
for(b in 1:length(bases)){
  for(nt in 1:length(ntrees)){
    for(kk in 1:length(ks)){
      MSEs <- rep(NA,10)
      folds <- sample(1:10,size=length(bike_train$rented_bikes),replace=T)
      for(k in 1:10){
        print(paste0("BEGIN base=",bases[b],", ntree=",ntrees[nt],", k=",ks[kk],", fold=",k))
        bart <- wbart(x.train=bike_train[folds!=k,2:15], 
                      y.train=bike_train$rented_bikes[folds!=k],
                      x.test=bike_train[folds==k,2:15],
                      sparse=F,
                      base=bases[b],
                      ntree=ntrees[nt],
                      k=ks[kk])
        MSEs[k] <- mean( (bart$yhat.test.mean-bike_train$rented_bikes[folds==k])^2 ) 
      }
      results <- rbind(results, c( bases[b] , ntrees[nt] , ks[kk], mean(MSEs) ) ) 
    }
  }
}
results
# X0.75 X100 X1 X56915.0580563588
# 1   0.75  100  1          56915.06
# 2   0.75  100  2          55373.50
# 3   0.75  100  3          54624.59
# 4   0.75  200  1          55982.44
# 5   0.75  200  2          55008.09
# 6   0.75  200  3          55064.87
# 7   0.75  300  1          56377.08
# 8   0.75  300  2          54797.45
# 9   0.75  300  3          55082.23
# 10  0.85  100  1          56766.24
# 11  0.85  100  2          55621.29
# 12  0.85  100  3          54572.09
# 13  0.85  200  1          55850.13
# 14  0.85  200  2          55094.10
# 15  0.85  200  3          54489.75
# 16  0.85  300  1          55517.12
# 17  0.85  300  2          54261.69
# 18  0.85  300  3          54524.42
# 19  0.95  100  1          57822.75
# 20  0.95  100  2          55058.49
# 21  0.95  100  3          56179.60
# 22  0.95  200  1          54921.57
# 23  0.95  200  2          54867.32
# 24  0.95  200  3          54064.98
# 25  0.95  300  1          55641.92
# 26  0.95  300  2          55278.73
# 27  0.95  300  3          53374.22, best RSME=231.0286

set.seed(4804355)
yhats_BART_train <- wbart(x.train=bike_train[,2:15], 
              y.train=bike_train$rented_bikes,
              sparse=F,
              base=.75,
              ntree=200,
              k=3)$yhat.train.mean
yhats_BART_test <- wbart(x.train=bike_test[,2:15], 
                          y.train=bike_test$rented_bikes,
                          sparse=F,
                          base=.75,
                          ntree=200,
                          k=3)$yhat.train.mean
save(yhats_BART_train,
     yhats_BART_test,
     file="C://Users//blj12//OneDrive//STAT S25//STAT 6020//yhats_BART.Rdata")


