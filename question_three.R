library(tidyverse)
#read data
trips<-readr::read_csv("data/question_two_trips.csv")
fares<-readr::read_csv("data/question_two_fares.csv")

##trips

#discard error 1: unreasonable reported meter distance
#calculate the straight line distance
R=3958.8
dlon = (trips$pickup_longitude - trips$dropoff_longitude)*pi/180
dlat = (trips$pickup_latitude - trips$dropoff_latitude)*pi/180
lat1 = pi*trips$pickup_latitude/180
lat2 = pi*trips$dropoff_latitude/180
a = sin(dlat/2)^2 + sin(dlon/2)^2 * cos(lat1) * cos(lat2)
c = 2 * atan2( sqrt(a), sqrt(1-a) )
trips<-mutate(trips, strl_distance = R * c)
#discard the trips with unreasonable reported meter distance
trips<-filter(trips, trip_distance > trips$strl_distance)
#remove staight line distance variable
trips<-select(trips, -strl_distance)

#discard error 2: calculate trip time with pickup time and dropoff time to replace the original record
trips$pickup_datetime<-as.POSIXct(trips$pickup_datetime)
trips$dropoff_datetime<-as.POSIXct(trips$dropoff_datetime)
trips<-mutate(trips, trip_time_in_secs = dropoff_datetime - pickup_datetime)

#discard error 3: remove the trips with impossile coordinates
trips <- filter(trips, pickup_latitude != 0 & pickup_longitude != 0 
               & dropoff_latitude != 0 & dropoff_longitude != 0)

#discard error 4: remove the trips with impossile trip time 
trips <- filter(trips, trip_time_in_secs > 10)

#discard error 5: remove the trips with impossile trip velocities 
trips <- trips %>% mutate(velocity = trip_distance/as.numeric(trip_time_in_secs)*3600) %>% 
  filter(velocity < 100)

##fares

#discard error 6: remove the negative payments
fares<-filter(fares, fare_amount >= 0, surcharge >= 0, mta_tax >= 0,
       tip_amount >= 0, tolls_amount >= 0, total_amount >= 0)

#write data to csv
write.csv(trips, file.path ("data", "question_three_trips.csv"))
write.csv(fares, file.path ("data", "question_three_fares.csv"))

