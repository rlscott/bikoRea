# -----------------------------------------------------------------
#
#   Stat 602 - Machine Learning
#   Final Project - bikoRea
#   knn LOOCV | Rebekah Scott 
#
# -----------------------------------------------------------------

# load in data
load("data/bike_clean.Rdata")

# filler code, change later to use with bike data 
library(caret)
set.seed(1999)
# knn_LOOCV <- train(y=housing_dat$price,x=housing_dat[,2:12],
#                    method="knn", tuneGrid=data.frame(k=1:30),
#                    preProcess=c("center","scale"),
#                    trControl=trainControl(method="LOOCV"))
# 
# knn_LOOCV$results
# which.min(knn_LOOCV$results$RMSE)
# knn_LOOCV$finalModel
# 
# 
# LOOCV_plot <- ggplot(knn_LOOCV) + 
#   labs(title = "k-nn LOOCV", y = "RMSE") + 
#   theme_bw() +  theme(plot.title = element_text(hjust = 0.5))
# 
# ggsave("figures/hw4_CE_knn.jpg", LOOCV_plot, width = 4, height = 3, dpi = 300)