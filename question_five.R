library(tidyverse)

#read data
taxi_data <- readr::read_csv("/Users/shirleyshen/Documents/FALL3. MD&ML/HW1/data_hw1/joined_taxi_data.csv")
taxi_data <- mutate(taxi_data, medallion = as.character(medallion))

#total_trips: the total number of trips.
taxi_data <- taxi_data %>% group_by(medallion) %>% mutate(total_trips = n())

#total_passengers: the total number of passengers.
taxi_data <- taxi_data %>% group_by(medallion) %>% mutate(total_passengers = sum(passenger_count)) 

#total_time_with_passengers: the total time spent carrying passengers.
taxi_data <- taxi_data %>% group_by(medallion) %>% mutate(total_time_with_passengers = sum(trip_time_in_secs)) 

#total_distance: the total distance traveled.
taxi_data <- taxi_data %>% group_by(medallion) %>% mutate(total_distance = sum(trip_distance)) 

#total_earnings: the total amount of money earned.
taxi_data <- taxi_data %>% group_by(medallion) %>% mutate(total_earnings = sum(total_amount)) 

#save selected columns
taxi_data <- distinct(select(taxi_data, medallion, total_trips, total_passengers, total_time_with_passengers,
                    total_distance, total_earnings))
write.csv(taxi_data, file.path ("data", "question_five.csv"))

