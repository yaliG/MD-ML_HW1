library(ggplot2)

# (1)
# read data
taxi_data <- readr::read_csv("data/question_five.csv")

# 4 scatterplots
p <- ggplot(data=taxi_data, aes(x=total_trips, y=total_earnings)) +
  geom_point()
p + ggtitle("Total trips v.s. total earnings") +
  xlab("Total trips, times") + ylab("Total earnings, dollar")
ggsave("question_eight_trips.png", path = "figures")

p <- ggplot(data=taxi_data, aes(x=total_passengers, y=total_earnings)) +
  geom_point()
p + ggtitle("Total passengers v.s. total earnings") +
  xlab("Total passengers, number of people") + ylab("Total earnings, dollar")
ggsave("question_eight_passengers.png", path = "figures")

p <- ggplot(data=taxi_data, aes(x=total_time_with_passengers, y=total_earnings)) +
  geom_point()
p + ggtitle("Total time with passengers v.s. total earnings") +
  xlab("Total time with passengers, second") + ylab("Total earnings, dollar")
ggsave("question_eight_time_with_passengers.png", path = "figures")

# remove the impossible total_distance
taxi_data <- filter(taxi_data, total_distance <= 100*24)
p <- ggplot(data=taxi_data, aes(x=total_distance, y=total_earnings)) +
  geom_point()
p + ggtitle("Total distance v.s. total earnings") +
  xlab("Total distance, mile") + ylab("Total earnings, dollar")
ggsave("question_eight_distance.png", path = "figures")

# (2)
# Liang
taxi_data <- readr::read_csv("data/question_six.csv")

p1 <- ggplot(data=taxi_data)+
  geom_smooth(mapping=aes(x=hour,y=trips_started))+
  labs(title="Trips Started in 24h",x="Hour(h)",y="Trips Started(count)")
ggsave("question_eight_Liang_Yingtian1.png", path = "figures")

p2 <- ggplot(data=taxi_data)+
  geom_smooth(mapping=aes(x=hour,y=total_passengers_picked_up))+
  labs(title="Passengers in 24h",x="Hour(h)",y="Passengers(person)")
ggsave("question_eight_Liang_Yingtian2.png", path = "figures")

d1 <- taxi_data %>% group_by(hack_license) %>%
  summarize(total_trips=sum(trips_started),total_passengers=sum(total_passengers_picked_up))

p3 <- ggplot(d1)+
  geom_smooth(mapping=aes(x=total_trips,y=total_passengers))+
  labs(title="Trips and Passengers",x="Total trips(count)",y="Total passengers(person)")
ggsave("question_eight_Liang_Yingtian3.png", path = "figures")

# Shen
taxi_hour <- readr::read_csv("data/question_seven.csv")
p4 <- ggplot(data = filter(taxi_hour, earnings <100), 
             aes(x=total_time_with_passengers, y=earnings, color = as.factor(hour))) +
  geom_point(size = 1)+
  labs(title="Total time with passengers v.s. earnings for different hours",
       x="Total time with passengers, sec",
       y="Earnings")+
  guides(color = guide_legend(title = "hour"))
ggsave("question_eight_Shen_Xuechun.png", path = "figures", width = 20, height = 10, unit = "cm")

# Gao
p5 <- ggplot(taxi_hour, 
             aes(x=total_time_with_passengers, y=miles_with_passengers, color = as.factor(hour)))+
  geom_point(size = 1) + 
  labs(title="Total time with passengers v.s. miles with passengers for different hours",
       x="Total time with passengers, sec",
       y="Miles with passengers, mile")+
  guides(color = guide_legend(title = "hour", nrow = 12))
ggsave("question_eight_Gao_Yali.png", path = "figures", width = 20, height = 10, unit = "cm")

