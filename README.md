# Cycle-Bike-Analysis
This analysis is done using R programme language.
### Introduction to Problem: 
A bike-share company in Chicago features more
than 5,800 bicycles and 600 docking stations. Cyclist sets itself
apart by also offering reclining bikes, hand tricycles, and cargo bikes,
making bike-share more inclusive to people with disabilities and riders
who can’t use a standard two-wheeled bike. The majority of riders opt
for traditional bikes; about 8% of riders use the assistive options.
Users are this program are categorized into two ways: Members, who owns
the annual membership and the casual riders, who don't own the membership
but pays hourly based on the time they use the bike.\

The director of marketing team at cyclist, believes the company’s
future success depends on maximizing the number of annual memberships.
Therefore, I want to understand how casual riders and annual members use
Cyclist bikes differently. From these insights, my team will design a
new marketing strategy to convert casual riders into annual members.\

### Approach 
To get the results, I followed all the phases of data
analysis which are ASK, PREPARE, PROCESS, ANALYSE, SHARE, ACT.\

### ASK 
To find out how annual members and casual riders use Cyclist
bikes differently. This insights would help the team to design a
marketing strategy targeted at converting casual riders to members to
promote the growth of Cyclist.

### PREPARE 
The data has been made available by Motivate International
Inc. under [this license](https://divvybikes.com/data-license-agreement).
I will be using the trip data from January 2021 to December 2021.
### PROCESS
I have used R to analyze the data. I chose R programming
language because of its flexibility in data manipulation and
visualization. Following is the list of codes that I performed for solving this problem.
## Loading the required libraries
```{r Loading the required libraries}
library(tidyverse)
library(lubridate)
library(janitor)
library(readr)
```
## Loading the 2021 trip data
```{r Loading the 2021 trip data}
jan <- read_csv("jan.csv")
feb <- read_csv("feb.csv")
mar <- read_csv("mar.csv")
april <- read_csv("april.csv")
may <- read_csv("may.csv")
june <- read_csv("june.csv")
july <- read_csv("july.csv")
august <- read_csv("august.csv")
september <- read_csv("september.csv")
october <- read_csv("october.csv")
november <- read_csv("november.csv")
december <- read_csv("december.csv")
```
## Explore data
```{r Explore data}
compare_df_cols_same(jan,feb,mar,april,may,june,july,august,september,october,
                     november,december)
```
## Combine all months data to a single file
```{r Combine all months data to a single file}
trips2021 <- rbind(jan,feb,mar,april,may,june,july,august,september,october,
                   november,december)
```

## Visualise the data
```{r Visualise the data}
head(trips2021)
summary(trips2021)
glimpse(trips2021)
```
## Checking for duplicate rows
```{r Checking for duplicate rows}
nrow(distinct(trips2021)) == nrow(trips2021)
```


## Remove Nulls
```{r Remove Nulls}
trips2021_v2 <- drop_na(trips2021)
glimpse(trips2021_v2)
```
## Renaming some columns
```{r Renaming some columns}
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

```
## Checking for zero trip duration
```{r Checking for zero trip duration}
trips2021_v2 %>% 
  filter(ride_duration == 0) 
```
## Checking for negative trip duration
```{r Checking for negative trip duration }
trips2021_v2 %>% 
  filter(ride_duration < 0)
```
## Remove the rows where the ride duration is either zero or negative
```{r Remove the rows where the ride duration is either zero or negative}
trips2021_cleaned <- trips2021_v2 %>% 
  filter(!ride_duration <= 0)

### Analysis
The analysis is done using R programme language. Code are available in the R or html file.
## 1. Riders distribution
```{r Riders distribution}
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
```

<img width="677" alt="image" src="https://github.com/10guptapallavi/Cycle-Bike-Analysis/assets/157853035/c8cf10ea-67c1-40ce-9a03-7840d43fafd3">

### Share
The key insights gleaned from the analysis are as follows:

* Members use Cyclist bikes more than the casual riders. Members account for 55% of the total rides while casual riders completed 45% of the total rides. However, casual riders ride the bikes longer than members. The average ride duration for casual riders is more than twice for members.

* Casual riders use the Cyclist bikes more than members in the summer months, July to September. Members use the bikes more than casual riders in the remaining months of the year (outside summer months).

* Casual riders seem to use the bikes more for leisure while the members seem more likely to use the bike to commute to and from work. Casual riders used the bikes far more on weekends. Their usage starts to rise on Fridays and moves up significantly on Saturdays and Sundays from the fairly consistent level on weekdays. Members' usage is fairly consistent throughout the week.

* The bikes are mostly used during the day by both categories of users. Members use the bikes significantly more than the casual riders from 6 a.m to 9 a.m in the morning and between 4 p.m to 7 p.m in the evening. Casual riders use the bikes more than members in the night from 9pm and at odd hours.

* The top 2 most popular stations where casual riders start their trips from are close to leisure centres. Pointing us to the conclusion that they use Cyclist bikes primarily for leisure.

* Casual members generally ride the bikes longer than members throughout the hours of the day. The average ride duration is fairly constant for members throughout the day. It is interesting to note that despite that fewer rides happen at odd hours from 12 midnight to 4 a.m, casual riders ride the bikes for longer duration during those periods.

