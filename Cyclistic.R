# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization


library(tidyverse)  
library(lubridate) 
library(ggplot2)  
getwd()
setwd("/Users/Gabriel/Documents/Formation_Google/Case_study_1/Data_bike_csv") 



dffev <- read_csv('202202-divvy-tripdata.csv')
dfmar <- read_csv('202203-divvy-tripdata.csv')
dfapr <- read_csv('202204-divvy-tripdata.csv')
dfmay <- read_csv('202205-divvy-tripdata.csv')
dfjun <- read_csv('202206-divvy-tripdata.csv')
dfjul <- read_csv('202207-divvy-tripdata.csv')
dfaug <- read_csv('202208-divvy-tripdata.csv')
dfsep <- read_csv('202209-divvy-tripdata.csv')
dfoct <- read_csv('202210-divvy-tripdata.csv')
dfnov <- read_csv('202211-divvy-tripdata.csv')
dfdec <- read_csv('202212-divvy-tripdata.csv')
dfjan <- read_csv('202301-divvy-tripdata.csv')

nom_df <- c('dffev')


colnames(dffev)
colnames(dfmar)
colnames(dfapr)
colnames(dfmay)
colnames(dfjun)
colnames(dfjul)
colnames(dfaug)
colnames(dfsep)
colnames(dfoct)
colnames(dfnov)
colnames(dfdec)
colnames(dfjan)



filesn <- list(dffev,dfmar,dfapr,dfmay,dfjun,dfjul,dfaug,dfsep,dfoct,dfnov,dfdec,dfjan)

for (i in filesn){
  str(i)
}



 

# Stack individual data frames into one big data frame
all_trips <- bind_rows(dffev,dfmar,dfapr,dfmay,dfjun,dfjul,dfaug,dfsep,dfoct,dfnov,dfdec,dfjan)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

# Inspect the new table that has been created
colnames(all_trips)  
nrow(all_trips) 
dim(all_trips) 
head(all_trips)  
str(all_trips)  
summary(all_trips)  

table(all_trips$start_station_name)


all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")



all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)


str(all_trips)


is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)



all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
all_trips_v2 <-all_trips_v2 %>% drop_na()
all_trips <- all_trips %>% drop_na()





mean(all_trips_v2$ride_length) 
median(all_trips_v2$ride_length) 
max(all_trips_v2$ride_length) 
min(all_trips_v2$ride_length) 


summary(all_trips_v2$ride_length)


aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("dimanche", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi"))


aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							 
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday)							


all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


counts <- aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
write.csv(counts, file = '~avg_ride_length.csv')
