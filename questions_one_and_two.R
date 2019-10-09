setwd("/Users/shirleyshen/Documents/FALL3. MD&ML/HW1/hw1_Gao_Liang_Shen")

#Q1
library(tidyverse)

trips <- readr::read_csv("/Users/shirleyshen/Documents/FALL3. MD&ML/HW1/data_hw1/trip_data_8.csv")
fares <- readr::read_csv("/Users/shirleyshen/Documents/FALL3. MD&ML/HW1/data_hw1/trip_fare_8.csv")

#Q2
trips <- trips[grep('2013-08-15',trips$pickup_datetime),]
fares <- fares[grep('2013-08-15',fares$pickup_datetime),]

write.csv(trips, file.path ("data", "question_two_trips.csv"))
write.csv(fares, file.path ("data", "question_two_fares.csv"))

