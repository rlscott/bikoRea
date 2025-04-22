load("data/bike_clean.Rdata")

library(caret)
library(tictoc)
library(tidyverse)
set.seed(7)
tic()
CART10F<-train(y=bike_train$rented_bikes, x=bike_train[,2:15], method="rpart",
                    tuneGrid=data.frame(cp=(1:100)/100000),
                    trControl=trainControl(method="repeatedcv",
                                           repeats = 10,number=10))
toc() # 80.09 seconds

CART10F
CART10F_plot <- ggplot(CART10F) + labs(title = "CART 10-Fold CV", y = "RMSE") + 
  theme_bw() +  theme(plot.title = element_text(hjust = 0.5))
CART10F_plot
ggsave("figures/CART_10FCV.pdf", CART10F_plot, width = 7, height = 5, dpi = 300)

# rpart.plot(CART10F$finalModel)

CART_test_pred <- predict(CART10F, bike_test)
CART_test_rmse <- RMSE(CART_test_pred, bike_test$rented_bikes)
CART_test_rmse # 285.9258
