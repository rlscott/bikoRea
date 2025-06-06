# -----------------------------------------------------------------
#
#   Stat 602 - Machine Learning
#   Final Project - bikoRea
#   knn 10 F, 10 repeats CV | Rebekah Scott 
#
# -----------------------------------------------------------------

# load in data
load("data/bike_clean.Rdata")

# setup 
library(caret)
set.seed(1999)

# 10 fold CV
knn_10F <- train(y=bike_train$rented_bikes,x=bike_train[,2:15],
                 method="knn", tuneGrid=expand.grid(k=1:30),
                 preProcess=c("center","scale"),
                 trControl=trainControl(method="repeatedcv", 
                                        repeats=10, number=10))

which.min(knn_10F$results$RMSE)
min(knn_10F$results$RMSE)
knn_10F$finalModel
# k = 7 is best, 10F CV RMSE of 303.6568

library(ggplot2)
knn_10F_plot <- ggplot(knn_10F) + labs(title = "knn 10-Fold CV", y = "RMSE") + 
  theme_bw() +  theme(plot.title = element_text(hjust = 0.5))

ggsave("figures/knn_10FCV.pdf", knn_10F_plot, width = 7, height = 5, dpi = 300)

# # calculate test RMSE 
# knn_test_pred <- predict(knn_10F, bike_test)
# knn_test_rmse <- RMSE(knn_test_pred, bike_test$rented_bikes)
# knn_test_rmse #  296.9907 is test RMSE 
