library(tidyverse)
library(lubridate)
library(janitor)
library(readr)
# Loading the 2021 trip data
jan <- read_csv("cycle data/jan.csv")
feb <- read_csv("cycle data/feb.csv")
mar <- read_csv("cycle data/mar.csv")
april <- read_csv("cycle data/april.csv")
may <- read_csv("cycle data/may.csv")
june <- read_csv("cycle data/june.csv")
july <- read_csv("cycle data/july.csv")
august <- read_csv("cycle data/august.csv")
september <- read_csv("cycle data/september.csv")
october <- read_csv("cycle data/october.csv")
november <- read_csv("cycle data/november.csv")
december <- read_csv("cycle data/december.csv")
#Look into the data
compare_df_cols_same(jan,feb,mar,april,may,june,july,august,september,october,
                     november,december)
#combine all data
trips2021 <- rbind(jan,feb,mar,april,may,june,july,august,september,october,
                   november,december)
head(trips2021)
summary(trips2021)
glimpse(trips2021)
# Checking for duplicate rows
nrow(distinct(trips2021)) == nrow(trips2021)
# remove Nulls
trips2021_v2 <- drop_na(trips2021)
glimpse(trips2021_v2)

#renaming some columns
trips2021_v2 <- trips2021_v2 %>% 
  mutate(ride_duration = round(difftime(ended_at, started_at, units = "mins"))) %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  mutate(start_hour = hour(started_at)) %>% 
  mutate(route = str_c(start_station_name, end_station_name, sep = " -to- "))

glimpse(trips2021_v2)

tail(trips2021_v2, 5)

summary(trips2021_v2)

trips2021_v2 <- trips2021_v2 %>% 
  select(member_casual, rideable_type, ends_with("name"),route, ride_duration:start_hour) %>% 
  rename(rider_type = member_casual, bike_type = rideable_type)

head(trips2021_v2, 10)

# Checking for zero trip duration
trips2021_v2 %>% 
  filter(ride_duration == 0) 

# Checking for negative trip duration 
trips2021_v2 %>% 
  filter(ride_duration < 0)

#I noticed that there are some zero 
#and negative ride durations. For the zero ride durations, 
#where the ride duration is less than a minute, I gathered 
#that the bikes were taken out of circulation for repairs.
#For the negative ride duration, I am not quite sure why this happened.
#Maybe the trip start and end times were mistakenly swapped.

# the zero and negative ride durations
trips2021_cleaned <- trips2021_v2 %>% 
  filter(!ride_duration <= 0)

head(trips2021_cleaned)
summary(trips2021_cleaned)

#Analysis of data
trips2021_cleaned %>% 
  group_by(rider_type) %>% 
  tally() %>% 
  mutate(percentage = round(n/sum(n)*100)) %>% 
  ggplot(aes(x = 1, y = percentage, fill = rider_type)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = str_c(rider_type, str_c(percentage, "%"), sep = "\n")), 
            position = position_stack(vjust = 0.5), color = "white", size = 8) + 
  labs(title = "Riders Distribution", fill = "Rider type") + 
  coord_polar(theta = "y") +
  theme_void()
#Members use Cylistic bikes more than the casual riders. 
#Members account for 55% of the total rides while
#casual riders completed 45% of the total rides.


#Let's examine the average duration of the trips
# Average ride duration
trips2021_cleaned %>% 
  group_by(rider_type) %>% 
  summarise(avg_ride_duration = round(mean(ride_duration)))%>% 
  ggplot(aes(x = rider_type, y = avg_ride_duration)) + 
  geom_col(position = "dodge", fill = "black") +
  labs( x = "Rider type", y = "Average ride duration (mins)")
#It is interesting to see that despite that members account
#for most of the rides, casual members ride the bikes longer than members. 
#The average ride duration for casual riders is more than twice for members.



#Let's compute and visualize the monthly ride distribution
trips2021_cleaned %>% 
  group_by(rider_type, month) %>% 
  summarise(total_rides = n()) %>% 
  arrange(month) %>% 
  ggplot(aes(x = month, y = total_rides, fill = rider_type)) +
  geom_col(position = "dodge") +
  labs(title = "Monthly bike rides by rider type",
       x = "Month", y = "Number of rides", fill = "Rider type")


#Let's examine how the bikes are used across the week to uncover
#usage patterns by members and casual riders.
trips2021_cleaned %>% 
  group_by(rider_type, weekday) %>% 
  summarise(total_rides = n()) %>% 
  ggplot(aes(x = weekday, y = total_rides, fill = rider_type)) +
  geom_col(position = "dodge") +
  labs(title = "Weekly bike usage distribution", 
       fill = "Rider type", x = "Weekday",
       y = "Number of rides")

#Let's look at how the trips are distributed across the hours of the day
trips2021_cleaned %>% 
  group_by(rider_type, start_hour) %>% 
  summarise(total_rides = n()) %>% 
  ggplot(aes(x = start_hour, y = total_rides, fill = rider_type)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = c(0:23))+
  labs(title = "Hourly bike rides", 
       fill = "Rider type", x = "Hour",
       y = "Number of rides")

#let's look at the average ride duration across the hours of the day
trips2021_cleaned %>% 
  group_by(rider_type, start_hour) %>% 
  summarise(avg_duration = mean(ride_duration)) %>% 
  ggplot(aes (x = start_hour, y = avg_duration, fill = rider_type)) + 
  geom_col(position ="dodge") + 
  scale_x_continuous(breaks = c(0:23)) +
  labs(title = "Average ride duration per hour",
       x = "Hour", y = "Average ride duration (mins)")

# Top 10 start stations
trips2021_cleaned %>% 
  group_by(rider_type, start_station_name) %>% 
  summarise(Number_of_trips = n()) %>% 
  arrange(desc(Number_of_trips)) %>% 
  head(10)
