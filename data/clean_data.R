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

# read in csv (from unzipped dowloaded file)
bike_raw <- read.csv("data/SeoulBikeData.csv", header=TRUE, comment.char="#", 
                 col.names = c("date", "rented_bikes", "hour", "temp", 
                               "humidity", "wind", "visibility", "dew_point", 
                               "solar", "rain", "snow", "season", "holiday", 
                               "functional"))

# clean data - put into desired format 
library(dplyr, magrittr, lubridate)

bike <- bike_raw %>% mutate(
  # clean up the date https://lubridate.tidyverse.org/
  date = lubridate::dmy(date),
  month = lubridate::month(date, label = TRUE),
  year = lubridate::year(date), 
  
  # make season, holiday, functional numeric 
  season = as.factor(season), # how to encode??? use case_when?? dummy var??
  holiday = ifelse(holiday == "Holiday", 1, 0), 
  functional = ifelse(functional == "Yes", 1, 0)
  ) %>% relocate(date, year, month)

# do we need dummy variables? 
# https://stackoverflow.com/questions/49276914/mutating-dummy-variables-in-dplyr

# gut checks
str(bike)
head(bike)

save(file = "data/bike_clean.Rdata", bike)
