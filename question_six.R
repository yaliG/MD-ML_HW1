library(tidyverse)
library(lubridate)

#read data
taxi_data <- readr::read_csv("/Users/shirleyshen/Documents/FALL3. MD&ML/HW1/data_hw1/joined_taxi_data.csv")
attr(taxi_data$pickup_datetime, "tzone") <- "America/New_York"
attr(taxi_data$dropoff_datetime, "tzone") <- "America/New_York"

taxi_data <- taxi_data %>% mutate(hour=hour(pickup_datetime))

total_passengers_picked_up <- taxi_data %>%
                                group_by(hack_license,hour) %>%
                                  summarise(total_passengers_picked_up=sum(passenger_count))

trips_started <- taxi_data %>%
                  group_by(hack_license,hour) %>%
                    summarise(trips_started=n())

question_six <- total_passengers_picked_up %>%
                  left_join(trips_started)

write.csv(question_six, file.path ("data", "question_six.csv"))
