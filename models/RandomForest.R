load("data/bike_clean.Rdata")
library(caret)
library(tictoc)
library(tidyverse)
library(randomForest)
library(xtable)

set.seed(7)
tic()
RFOOBF<-train(y=bike_train$rented_bikes, x=bike_train[,2:15], method="rf",
              tuneGrid=data.frame(.mtry=1:14),importance=T,
              trControl=trainControl(method="oob"),nodesize=3) # m OOB selected, nmin=5
toc() # 642.35 seconds

RFOOBF #mtry = 8

RFOOBF_plot <- ggplot(RFOOBF) + labs(title = "RF OOB", y = "RMSE") + 
  theme_bw() +  theme(plot.title = element_text(hjust = 0.5))
RFOOBF_plot
ggsave("figures/RF_OOB.pdf", RFOOBF_plot, width = 7, height = 5, dpi = 300)

# rpart.plot(RFOOBF$finalModel)

RF_test_pred <- predict(RFOOBF, bike_test)
RF_test_rmse <- RMSE(RF_test_pred, bike_test$rented_bikes)
RF_test_rmse # 232.0626
# ?predict

# xtable(varImp(RFOOBF)$importance)
# xtable(RFOOBF$finalModel$importance)

importance_vals <- varImp(RFOOBF)

# Extract the dataframe
importance_df <- importance_vals$importance
importance_df$Variable <- rownames(importance_df)

# Optional: Arrange by descending importance
importance_df <- importance_df[order(-importance_df$Overall), ]

# Turn into LaTeX table
xtable(importance_df)
