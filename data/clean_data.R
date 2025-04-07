# -----------------------------------------------------------------
#
#   Stat 602 - Machine Learning
#   Final Project - bikoRea
#   Data Cleaning | Rebekah Scott 
#
# -----------------------------------------------------------------

# Data from UC Irvine Machine Learning Repository
# Seoul Bike Sharing Demand, Donated on 2/29/2020
# https://archive.ics.uci.edu/dataset/560/seoul+bike+sharing+demand

# read in csv (from unzipped downloaded file)
bike_raw <- read.csv("data/SeoulBikeData.csv", header=TRUE, comment.char="#", 
                 col.names = c("date", "rented_bikes", "hour", "temp", 
                               "humidity", "wind", "visibility", "dew_point", 
                               "solar", "rain", "snow", "season", "holiday", 
                               "functional"))

# clean data - put into desired format 
library(dplyr, magrittr)

bike_all <- bike_raw %>% mutate(
  # preserve the spring and year (2017 or 2017)
  date = lubridate::dmy(date),
  year_2018 = ifelse(lubridate::year(date) == "2018", 1, 0),
  winter = ifelse(season == "Winter", 1, 0), 
  spring = ifelse(season == "Spring", 1, 0), 
  summer = ifelse(season == "Summer", 1, 0), 
  
  # make holiday and functional binary
  holiday = ifelse(holiday == "Holiday", 1, 0), 
  functional = ifelse(functional == "Yes", 1, 0) 
  ) %>% select(!c(date,season)) 


# functional denotes if bike rentals available / functioning 
# (all 0 counts are on non-functional days)
# bike_raw %>% filter(functional == "No") %>% nrow()
# bike_raw %>% filter(functional == "No" & rented_bikes == 0) %>% nrow()
# bike_raw %>% filter(rented_bikes == 0) %>% nrow()

n <- nrow(bike_all)
test_size <- n * .2 #  80/20 train / test split  

set.seed(1999)
test_indx <- sample(1:n, test_size)

bike_test <- bike_all[test_indx, ]
bike_train<- bike_all[-test_indx,]

# save the file 
save(file = "data/bike_clean.Rdata", bike_all, bike_test, bike_train)
