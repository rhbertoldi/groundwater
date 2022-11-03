library(tidyverse)

setwd("/Users/reibertoldi/Documents/UBC/EOSC 510/Final Project/")

groundwater <- read_csv("data/measurements.csv")
stations <- read_csv("data/stations.csv")
monthly <- read_csv("data/gwl-monthly.csv")
daily <- read_csv("data/gwl-daily.csv")

head(groundwater)
head(stations)

ggplot(stations) +
  geom_point(aes(x = longitude, y = latitude))


merge_df <- left_join(groundwater, stations, by = "site_code")


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