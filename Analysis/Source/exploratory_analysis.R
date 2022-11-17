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
