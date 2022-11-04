library(tidyverse)
library(lubridate)

setwd("/Users/reibertoldi/Documents/UBC/EOSC 510/Final Project/")

groundwater <- read_csv("data/measurements.csv")
stations <- read_csv("data/stations.csv")
monthly <- read_csv("data/gwl-monthly.csv")
daily <- read_csv("data/gwl-daily.csv")

head(groundwater)
head(stations)

ggplot(stations) +
  geom_point(aes(x = longitude, y = latitude))

merge_df <- left_join(groundwater, stations, by = "site_code") %>% 
  mutate(msmt_date_mdy = as.POSIXct(msmt_date,format = "%m/%d/%Y")) 

count_stations <- merge_df %>% 
  group_by(site_code) %>% 
  summarise(count = n()) %>% 
  filter(count >= 1000) 
stations_use <- unique(count_stations$site_code)
  
merge_use <- merge_df %>% 
  filter(site_code %in% stations_use)

explore_dates <- merge_use %>% 
  group_by(site_code) %>% 
  summarise(min = min(msmt_date_mdy),
            max = max(msmt_date_mdy))

daily_station_count <- daily %>% 
  group_by(STATION) %>% 
  summarise(count = n())

explore_dates_daily <- daily %>% 
  mutate(msmt_date_mdy = format(as.Date(MSMT_DATE), "%Y-%m"))

nrow(filter(groundwater, is.na(gwe)))

length(unique(groundwater$site_code))
length(unique(stations$site_code))
# 1282 stations not in stations (we dont have the locations for these?)
length(unique(merge_df$site_code))

one_station <- groundwater %>% 
  filter(site_code == "344644N1192081W001")

ggplot(one_station) +
  geom_line(aes(x = msmt_date, y = gwe))
# do we need all the stations to have the same time period? probably yes...


one_station2 <- groundwater %>% 
  filter(site_code == "344554N1192085W001")
ggplot(one_station2) +
  geom_line(aes(x = msmt_date, y = gwe))

# need to decide if i am getting the mean of daily? one measurment per day?