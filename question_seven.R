library(tidyverse)
library(lubridate)

#read data
taxi_data <- readr::read_csv("/Users/shirleyshen/Documents/FALL3. MD&ML/HW1/data_hw1/joined_taxi_data.csv")
attr(taxi_data$pickup_datetime, "tzone") <- "America/New_York"
attr(taxi_data$dropoff_datetime, "tzone") <- "America/New_York"

taxi_hour <- crossing(taxi_data, hour = c(0:23)) 
taxi_hour <- taxi_hour %>% 
             #regard those drop off on 8/16 as "2013-08-16 00:00:00" because we only consider the time period on 8/15
             mutate(dropoff_datetime=case_when(day(dropoff_datetime)==15~dropoff_datetime,
                                               day(dropoff_datetime)==16~as.POSIXct("2013-08-16 00:00:00")))

# calculate time for the hour containing pickup_datetime
taxi_hour1 <- taxi_hour %>% filter(hour == hour(pickup_datetime)) %>% 
  mutate(time_in_corresponding_hour = pmin(dropoff_datetime - pickup_datetime, 
                            floor_date(pickup_datetime + 3600, unit = "hour") - pickup_datetime)) %>%
  mutate(mile_hour = as.numeric(trip_distance/trip_time_in_secs*time_in_corresponding_hour)) %>% 
  mutate(earnings_hour = as.numeric(total_amount/trip_time_in_secs*time_in_corresponding_hour)) %>% 
  select(hack_license, hour, pickup_datetime, dropoff_datetime, time_in_corresponding_hour,
         mile_hour, earnings_hour)

# calculate time for the hour containing dropoff_datetime
taxi_hour2 <- taxi_hour %>% filter(hour == hour(dropoff_datetime)) %>% 
  mutate(time_in_corresponding_hour = pmin(dropoff_datetime - pickup_datetime, 
                                           dropoff_datetime - floor_date(dropoff_datetime, unit = "hour"))) %>% 
  mutate(mile_hour = as.numeric(trip_distance/trip_time_in_secs*time_in_corresponding_hour)) %>% 
  mutate(earnings_hour = as.numeric(total_amount/trip_time_in_secs*time_in_corresponding_hour)) %>% 
  select(hack_license, hour, pickup_datetime, dropoff_datetime, time_in_corresponding_hour,
         mile_hour, earnings_hour)

# calculate time for the hour containing no time with passengers
taxi_hour3 <- taxi_hour %>% filter(hour < hour(pickup_datetime) | hour > hour(dropoff_datetime)) %>% 
  mutate(time_in_corresponding_hour = 0) %>% 
  mutate(mile_hour = as.numeric(trip_distance/trip_time_in_secs*time_in_corresponding_hour)) %>% 
  mutate(earnings_hour = as.numeric(total_amount/trip_time_in_secs*time_in_corresponding_hour)) %>% 
  select(hack_license, hour, pickup_datetime, dropoff_datetime, time_in_corresponding_hour,
         mile_hour, earnings_hour)

# calculate time for the hour between the pickup_datetime and dropoff_datetime
taxi_hour4 <- taxi_hour %>% filter(hour > hour(pickup_datetime) & hour < hour(dropoff_datetime)) %>% 
  mutate(time_in_corresponding_hour = 1) %>% 
  mutate(mile_hour = as.numeric(trip_distance/trip_time_in_secs*time_in_corresponding_hour)) %>% 
  mutate(earnings_hour = as.numeric(total_amount/trip_time_in_secs*time_in_corresponding_hour)) %>% 
  select(hack_license, hour, pickup_datetime, dropoff_datetime, time_in_corresponding_hour,
         mile_hour, earnings_hour)
# combine the subsets to gain the whole data
# use distinct because there might be duplicates in taxi_hour1 and taxi_hour2 if pickup_datetime 
# and dropoff_datetime are in the same hour
taxi_hour <- distinct(union_all(taxi_hour1, taxi_hour2, taxi_hour3, taxi_hour4))

# group hack license and hour
# transfer format of time to speed up
taxi_hour$time_in_corresponding_hour <- as.numeric(taxi_hour$time_in_corresponding_hour)
# sum and output the result
taxi_hour <- taxi_hour %>% group_by(hack_license, hour) %>% 
  mutate(total_time_with_passengers = sum(time_in_corresponding_hour)) %>% 
  mutate(miles_with_passengers = sum(mile_hour)) %>% 
  mutate(earnings = sum(earnings_hour)) %>% 
  select(hack_license, hour, total_time_with_passengers, miles_with_passengers, earnings) %>% 
  arrange(hack_license, hour)
taxi_hour <- distinct(taxi_hour)
# remove trips with unreasonable time and miles in one hour 
taxi_hour <- filter(taxi_hour, total_time_with_passengers <= 3600 
                    & miles_with_passengers < 100)

# write data to csv
write.csv(taxi_hour, file.path ("data", "question_seven.csv"))
