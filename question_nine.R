library(tidyverse)
library(ggpubr)

taxi_data <- readr::read_csv("data/question_six.csv")
taxi_hour <- readr::read_csv("data/question_seven.csv")

p1 <- ggplot(data=taxi_data)+
        geom_smooth(mapping=aes(x=hour,y=trips_started))+
        labs(title="Trips Started in 24h",x="Hour(h)",y="Trips Started(count)")

p2 <- ggplot(data=taxi_data)+
        geom_smooth(mapping=aes(x=hour,y=total_passengers_picked_up))+
        labs(title="Passengers in 24h",x="Hour(h)",y="Passengers(person)")

d1 <- taxi_data %>% group_by(hack_license) %>%
              summarize(total_trips=sum(trips_started),total_passengers=sum(total_passengers_picked_up))

p3 <- ggplot(d1)+
        geom_smooth(mapping=aes(x=total_trips,y=total_passengers))+
        labs(title="Trips and Passengers",x="Total trips(count)",y="Total passengers(person)")

p4 <- ggplot(data = filter(taxi_hour, earnings <100), 
              aes(x=total_time_with_passengers, y=earnings, color = as.factor(hour))) +
        geom_point(size = 1)+
        labs(title="Total time with passengers v.s. earnings for different hours",
             x="Total time with passengers, sec",
             y="Earnings")+
        guides(color = guide_legend(title = "hour"))

p5 <- ggplot(taxi_hour, 
             aes(x=total_time_with_passengers, y=miles_with_passengers, color = as.factor(hour)))+
        geom_point(size = 1) + 
        labs(title="Total time with passengers v.s. miles with passengers for different hours",
             x="Total time with passengers, sec",
             y="Miles with passengers, mile")+
        guides(color = guide_legend(title = "hour", nrow = 12))

fig <- ggarrange(p1,p2,p3,p4,p5,ncol=2,nrow=3)
ggsave("question_nine.png", path = "figures" ,width = 40, height = 25, units = "cm")
