library(tidyverse)
library(lubridate)

setwd("/Users/reibertoldi/Documents/UBC/EOSC 510/Final Project/")

groundwater <- read_csv("data/measurements.csv")
stations <- read_csv("data/stations.csv")
# monthly <- read_csv("data/gwl-monthly.csv")
# daily <- read_csv("data/gwl-daily.csv")

ggplot(stations) +
  geom_point(aes(x = longitude, y = latitude))

merge_df <- left_join(groundwater, stations, by = "site_code") %>% 
  mutate(msmt_date_ymd = format(as.Date(msmt_date), "%Y-%m-%d")) %>% 
  mutate(msmt_date_ym = format(as.Date(msmt_date), "%Y-%m")) 

count_stations <- merge_df %>% 
  group_by(site_code) %>% 
  summarise(count = n()) %>% 
  filter(count >= 100) 
stations_use <- unique(count_stations$site_code)

year_month_merge <- merge_df %>% 
  separate(col = msmt_date_ym,
           into = c("year", "month"),
           sep = "-")

test_numbers <- year_month_merge %>%
  group_by(site_code) %>% 
  filter(site_code %in% stations_use) %>% 
  summarise(max = max(year),
            min = min(year),
            count = n()) %>% 
  filter(min <= 2015, 
         max >= 2020)

test_stations <- test_numbers$site_code

years <- c(seq(2015, 2020, 1))

# this column is getting the monthly averages for the dates specified by the code above
monthly_gwe <- year_month_merge %>% 
  filter(site_code %in% test_stations) %>% 
  filter(year %in% years) %>% 
  mutate(msmt_date_ym = format(as.Date(msmt_date), "%Y-%m")) %>% 
  group_by(site_code, msmt_date_ym) %>% 
  summarise(mean_gwe = mean(gwe)) %>% 
  rename("dates" = "msmt_date_ym")

dates <- seq(as.Date("2015/01/01"), by = "month", length.out = 72)
sites <- data.frame(site_code = unique(monthly_gwe$site_code))

rep_codes <- data.frame(site_code = sites[rep(seq_len(nrow(sites)), length(dates)),
                                     c('site_code')])
dates_df <- data.frame(dates = dates)
rep_dates <- data.frame(dates = dates_df[rep(seq_len(nrow(dates_df)), each = 3426), ])
rep_df <- cbind(rep_codes, rep_dates) %>% 
  mutate(dates = format(as.Date(dates), "%Y-%m"))  

filled_dates <- left_join(rep_df, monthly_gwe, by = c("site_code", "dates"))

filled_dates %>% 
  group_by(site_code) %>% 
  summarise(count = sum(is.na(mean_gwe)))
# decide how many of these I want to keep? 

# seeing what the distribution of stations looks like
year_month_merge %>% 
  filter(site_code %in% test_stations) %>% 
  ggplot() +
  geom_point(aes(x = longitude, y = latitude))

#merge_use <- merge_df %>% 
#  filter(site_code %in% stations_use)
#
# explore_dates <- merge_use %>% 
#   group_by(site_code) %>% 
#   summarise(min = min(msmt_date),
#             max = max(msmt_date))
# 
# getting average month value 
#
#month_means <- merge_df %>% 
#  group_by(site_code, msmt_date_mdy) %>% 
#  summarise(average_mean = mean(gwe))
#
#stations_year <- month_means %>% 
#  group_by(site_code) %>% 
#  summarise(count = n(),
#            max = max(msmt_date_mdy),
#            min = min(msmt_date_mdy)) %>% 
#  filter(count >= 24) %>% 
#  filter()
#
# looking for year ranges with the most data
#
# temp %>% 
#   group_by(site_code) %>% 
#   summarise(max = max(year),
#             min = min(year),
#             count = n()) %>%
#   filter(count >= 60) %>% 
#   select(-count) %>% 
#   gather(type, years, c("max", "min"), factor_key = TRUE) %>% 
#   ggplot() +
#   geom_line(aes(y = site_code, x = years, group = site_code)) + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
#
# hopefully there is no bias in the way that these sites have been selected
# maybe larger groundwater sites are sampled more often
# can I look at the number of times sampled versus the level?
#
#nintyfour_eighteen <- temp %>% 
#  filter(year >= 1994, 
#         year <= 2018)
#
#nintyfour_eighteen %>% 
#  mutate(msmt_date_mdy = format(as.Date(msmt_date), "%Y-%m")) %>% 
#  group_by(site_code, msmt_date_mdy) %>% 
#  summarise(average_mean = mean(gwe))
#  complete(Date = seq.Date(min(Date), max(Date), by="day"))
#
# maybe I can make the data and then merge it?
# I really need to be careful about how it gets merged 
#
# now I need to find a way to fill the data with NAs if they are missing 
#
## exploring daily and monthly data
#
#daily_station_count <- daily %>% 
#  group_by(STATION) %>% 
#  summarise(count = n())
#
#explore_dates_daily <- daily %>% 
#  mutate(msmt_date_mdy = format(as.Date(MSMT_DATE), "%Y-%m"))
#
#nrow(filter(groundwater, is.na(gwe)))
#
#length(unique(groundwater$site_code))
#length(unique(stations$site_code))
## 1282 stations not in stations (we dont have the locations for these?)
#length(unique(merge_df$site_code))
#
#one_station <- groundwater %>% 
#  filter(site_code == "371563N1182885W001")
#
#ggplot(one_station) +
#  geom_line(aes(x = msmt_date, y = gwe))
## do we need all the stations to have the same time period? probably yes...
#
#sataions_year %>% 
#  mutate(year_max = mutate(msmt_date_mdy = format(as.Date(max), "%Y")))
#
#ggplot(sataions_year) +
#  geom_point(aes(x = max, y = min))
#
#one_station2 <- groundwater %>% 
#  filter(site_code == "344554N1192085W001")
#ggplot(one_station2) +
#  geom_line(aes(x = msmt_date, y = gwe))
#
## need to decide if i am getting the mean of daily? one measurement per day?#