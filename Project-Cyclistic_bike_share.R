#Set up my environment
require(tidyverse)
require(janitor)
require(skimr)
require(plyr)
require(lubridate)

#import and combine files from folder into one dataframe
files = list.files(path = "datasets/", full.names = TRUE) 
trips_data <- ldply(files,read_csv)
?bind_rows
?rbind
trips_data %>% names()
#inspect dataframe
glimpse(trips_data)
trips_data %>% filter(is.na(trips_data))
skim_without_charts(trips_data)

trips_data %>% tibble()-> trips_data
trips_data %>% 
  mutate(membership = ifelse(member_casual=="member",1,0)) -> trips_data2

lr <- glm(membership ~.,trips_data2, family=binomial)

trips_data2 %>% names()
trips_data2 %>% with(table(rideable_type,membership)) %>%
  chisq.test()



#Transform data for effective analysis
##create a ride_length column to calculate the length of each ride
trip_length_df <- trips_data %>% 
  mutate(ride_length = as.numeric(as.character(ended_at - started_at)))

##create a day_of_week column to calculate the day of the week that each ride started
trip_day_df <- trip_length_df %>% 
  mutate(day_of_week = weekdays(started_at))

#inspect ride_length for anomalities
require(Hmisc)
describe(trip_day_df$ride_length)
###inspection shows lowest ride length in negative values, which means that some values of started_at are greater than ended_at

#Clean data
##filter to find outliers
outliers_trip_data <- trip_day_df %>% 
  filter(started_at > ended_at)
###10,697 observations of start times that are greater than end times
## filter out outliers
clean_trip_data <- trip_day_df %>% 
  filter(!(started_at > ended_at))

#Calculations:
##determine the mean, max and min ride_length.
trip_data_cals <- clean_trip_data %>% 
  dplyr::summarise(mean_ride_length=mean(ride_length),median_ride_length=median(ride_length),max_ride_length=max(ride_length),
                   min_ride_length=min(ride_length))
### median is far from the mean, which shows that the it is not a normal distribution
### therefore I shall be reporting the median and the IQR of ride_length

IQR(clean_trip_data$ride_length)
### IQR=982, Median= 762

##determine mode of day_of_week
find_mode <- function(x){
  ux = unique(x)
  tab = tabulate(match(x,ux))
  ux[tab==max(tab)]
}
find_mode(clean_trip_data$day_of_week)
#the most frequent day of the week is Saturday for both users

#pivot or summary tables
##determine the  average ride_length for members and casual riders
pivot_1 <- clean_trip_data %>% 
  group_by(member_casual) %>% 
  dplyr::summarise(Avg_ride_length=mean(ride_length))
### casual riders have an average ride length greater than member riders

##determine the average ride_length for users by day_of_week
pivot_2 <- clean_trip_data %>% 
  select(member_casual,day_of_week,ride_length) %>% 
  group_by(member_casual,day_of_week) %>% 
  dplyr::summarise(avg_ride_length=mean(ride_length))
##OR to put columns = day_of_week; Rows =member_casual; Values = Average of ride_length
require(reshape2)
pivot_2.1 <-
  dcast(clean_trip_data,member_casual~day_of_week,value.var="ride_length",fun.aggregate = mean)
###Casual riders have higher average ride lengths than member riders in all days of the week

##determine  the number of rides for users by day_of_week by adding Count of trip_id to Values
pivot_3 <- clean_trip_data %>% 
  select(member_casual,day_of_week,ride_id) %>% 
  group_by(member_casual,day_of_week) %>% 
  dplyr::summarise(count_rides=n())
##OR
###install.packages("pivottabler)
require(pivottabler)
qpvt(clean_trip_data,"member_casual","day_of_week","n()")
##member riders have more ride counts all through the week except for weekends(Saturdays and Sundays) where casual riders have significant higher ride counts than member riders

# determine the relationship between users and rideable type
clean_trip_data %>% 
  select(member_casual, rideable_type) %>% 
  group_by(member_casual,rideable_type) %>% 
  dplyr::summarise(counts=n())
#OR
qpvt(clean_trip_data,"member_casual","rideable_type","n()")
### member riders use classic_bike significantly higher than casual riders,
### they also use more electric bikes than casual riders.
###Finally, the use of docked bikes by both groups is almost equal, with member rider using slightly more.

#Bivariate analysis of member_casual and reideable_type
table(clean_trip_data$rideable_type, clean_trip_data$member_casual) %>% 
  prop.table()*100
#OR
trips_data %>%  
  select(rideable_type,member_casual) %>% 
  table() %>% 
  prop.table()*100
#Differences observed
#more members hire classic bikes and electric bike than casuals

#Visualizations
#Plot of counts of rides by weekday
clean_trip_data %>% 
  ggplot() +
  geom_bar(aes(x=day_of_week,fill=member_casual))+
  labs(title = "Total rides by Week day",
       x="Days of week",
       y="Count of Rides")
## the graph clearly shows that member riders have more ride counts all through the week except for weekends(Saturdays and Sundays) where casual riders have significant higher ride counts than member riders

# Column plot to show average ride length per day for both subscribers
pivot_2 %>% 
  ggplot()+
  geom_col(aes(x=day_of_week, y=avg_ride_length, fill=member_casual))+
  labs(title = "Average ride by week day",
       x="Day of Week",
       y="Average ride length")
### the graph shows that casual riders have higher average ride length than member riders call through the week
ggsave("lastplot.png")
