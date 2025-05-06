# -----------------------------------------------------------------
#
#   Stat 602 - Machine Learning
#   Final Project - bikoRea
#   Scatter plot matrix for top five preforming models | Rebekah Scott 
#
# -----------------------------------------------------------------

# load in data ----
load("data/bike_clean.Rdata")

# 1: random forest ----
# mtry = 6, nmin = 3

require(randomForest)
rf_bike <-randomForest(y=bike_train[,1],x=bike_train[,2:15],mtry=6, ntree = 500, nmin = 3)
rfPred <- rf_bike$predicted # OOB prediction

# 2: BART TO DO----
require(BART)
# bart_bike

bartPred <- bike_train[,1] # TO FIX!! 
# 3: XGBoost ----
# eta max_depth gamma colsample_bytree min_child_weight subsample nrounds 
# 0.2         3     2              0.5                1       0.9     200 
# Use double precision to avoid potential numerical issues in XGBoost.
num_bike_train <- bike_train
for (i in 1:12) {
  num_bike_train[,i]<-as.double(bike_train[,i])
}
num_bike_train <- as.matrix(num_bike_train)

require(xgboost)
xgb_bike <- xgboost(data=num_bike_train, label=num_bike_train[,1], 
                    objective = "reg:linear", nrounds=200, max_depth=3, 
                    eta=0.2, gamma=2, colsample_bytree=0.5, 
                    min_child_weight=1, subsample=0.9)
xgbPred <- predict(xgb_bike, num_bike_train)

# 4: CART ----
# cp = 0.00001
require(rpart)
tree_bike <- rpart(rented_bikes~., data=data.frame(bike_train), method="anova",
                   control=rpart.control(cp=0.00001)) 
treePred <- predict(tree_bike, data.frame(bike_train))

# 5: neural network ----
require(caret)
## avNNet (in caret package) aggregates several neural network models, 
## mainly based on (but better than) nnet() in nnet package.
avNNet_bike <- avNNet(y=bike_train[,1],x=bike_train[,2:15],linout=TRUE, 
                      repeats=50, maxit=500, size=20, decay=0.3, 
                      bag=FALSE, trace=FALSE)
avNNetPred <- predict(avNNet_bike, bike_train[,2:15])

# OLS ----
OLSPred <- lm(rented_bikes~., data = bike_train)$fitted

# put it together ----
top5Pred <- cbind(bike_train[,1], OLSPred, rfPred, bartPred, xgbPred, treePred, avNNetPred)
colnames(top5Pred)<-c("y", "OLS", "RF", "BART", "XGBoost", "Tree", "NNet")
top5Pred

# save the predicted values (for stacked model) 
save(file = "data/top5Pred.Rdata", top5Pred)

# Scatterplot matrix ----
library(GGally)
pairs_plot <- ggpairs(top5Pred) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme_bw()
pairs_plot

ggsave("figures/top5_scatter_plot_mat.jpg", pairs_plot, width = 10, height = 10, dpi = 300)

# get RF Test RMSE ----
rf_test_pred <- predict(rf_bike, bike_test)
rf_test_rmse <- RMSE(rf_test_pred, bike_test$rented_bikes)
rf_test_rmse # 233.3468 is test RMSE


# var importance table ----

# RF 
rf_import <- varImp(rf_bike)
colnames(rf_import) <- "RF"
rf_import <- round(rf_import / max(rf_import) * 100, 2) # put on a scale of 100

# BART (to do)
# bart_import <- varImp(bart_bike) # to do

# XGBoost (doesn't work)
xgb_import <- varImp(xgb_bike) # ask Liam about this 

# Tree
tree_import <- varImp(tree_bike) 
colnames(tree_import) <- "Regression Tree"
tree_import <- round(tree_import / max(tree_import) * 100, 2)

# Nnet is weird 
# avNNet_import <- varImp(avNNet_bike)

# put it together  
import_together <- merge(rf_import, tree_import, by = 0)
import_together <- import_together[order(-import_together$RF), ] # order it
print(xtable::xtable(import_together), include.rownames = FALSE) # turn into LaTeX table
                                   