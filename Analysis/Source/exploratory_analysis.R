library(tidyverse)
library(lubridate)
library(sf)

setwd("/Users/reibertoldi/Documents/UBC/EOSC 510/Final Project/")

groundwater <- read_csv("data/measurements.csv")
stations <- read_csv("data/stations.csv")
monthly <- read_csv("data/gwl-monthly.csv")
monthly_stations <- read_csv("data/gwl-stations.csv")

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

monthly_gse <- year_month_merge %>% 
  filter(site_code %in% test_stations) %>% 
  filter(year %in% years) %>% 
  mutate(msmt_date_ym = format(as.Date(msmt_date), "%Y-%m")) %>% 
  group_by(site_code, msmt_date_ym) %>% 
  summarise(mean_gse = mean(gse_gwe)) %>% 
  rename("dates" = "msmt_date_ym")

dates <- seq(as.Date("2015/01/01"), by = "month", length.out = 72)
sites <- data.frame(site_code = unique(monthly_gse$site_code))

rep_codes <- data.frame(site_code = sites[rep(seq_len(nrow(sites)), length(dates)),
                                     c('site_code')])
dates_df <- data.frame(dates = dates)
rep_dates <- data.frame(dates = dates_df[rep(seq_len(nrow(dates_df)), each = 3426), ])
rep_df <- cbind(rep_codes, rep_dates) %>% 
  mutate(dates = format(as.Date(dates), "%Y-%m"))  

filled_dates <- left_join(rep_df, monthly_gse, by = c("site_code", "dates"))

missing_years <- filled_dates %>% 
separate(col = dates,
         into = c("year", "month"),
         sep = "-") %>% 
  group_by(site_code, year) %>% 
  summarise(missing = sum(is.na(mean_gse)))

missing_years_ids <- missing_years %>% filter(missing >= 1)
missing_years_ids <- unique(missing_years_ids$site_code)

imputable_ids <- filled_dates %>% 
  filter(!site_code %in% missing_years_ids)

length(unique(imputable_ids$site_code))
# 983 sites to use 
# 111 to use if no missing data

predict_ids <- filled_dates %>% 
  filter(site_code %in% missing_years_ids)

filled_dates %>% 
  mutate(predictable = ifelse(site_code %in% missing_years_ids, 0, 1)) %>% 
  left_join(., stations, by = c("site_code")) %>%
  ggplot() +
  geom_point(aes(x = longitude, y = latitude, color = predictable))

imputable_ids %>% 
  filter(site_code == "357944N1190845W001") %>% 
  ggplot() +
  geom_point(aes(x=dates, y=mean_gse))

#write.csv(imputable_ids, "Analysis/Build/monthly_good_sites.csv", row.names = FALSE)

# buffering and intersecting stations

complete_join <- left_join(imputable_ids, stations, by = "site_code")
complete_join_sites <- data.frame(unique(complete_join$site_code)) %>% 
  rename("site_id" = "unique.complete_join.site_code.")

complete_join_sf <- complete_join %>%
  distinct(., site_code, .keep_all = TRUE) %>% 
  rownames_to_column("good_site") %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326, remove = FALSE) %>% 
  st_transform(3857) %>% # rough km projection
  st_buffer(10000) # about 10km

predict_join_sf <- left_join(predict_ids , stations, by = "site_code") %>% 
  distinct(., site_code, .keep_all = TRUE) %>% 
  rownames_to_column("bad_site") %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326, remove = FALSE) %>% 
  st_transform(3857)

out <- st_intersects(predict_join_sf, complete_join_sf)
dfs <- lapply(out, data.frame, stringsAsFactors = FALSE)
good_bad_site_intersections <- map_dfr(dfs, .f = cbind, .id = 'index') %>% 
  rename("bad_site" = "index",
         "good_site" = "X..i..")

good_index <- complete_join_sf %>% select(site_code, good_site) %>% 
  mutate(good_site = as.numeric(good_site)) %>% st_set_geometry(NULL)
bad_index <- predict_join_sf %>% select(site_code, bad_site) %>% 
  mutate(bad_site = as.numeric(bad_site)) %>% st_set_geometry(NULL)

final_intersections <- left_join(good_bad_site_intersections, good_index, by = "good_site") %>% 
  mutate(bad_site = as.numeric(bad_site)) %>% 
  rename("good_site_code" = "site_code") %>% 
  left_join(., bad_index, by = "bad_site") %>% 
  rename("bad_site_code" = "site_code")

temp <- final_intersections %>% 
  group_by(bad_site_code) %>% 
  summarise(count = n())

filled_dates %>% 
  filter(site_code == "388677N1213397W001") %>% 
  ggplot() +
  geom_point(aes(x=dates, y=mean_gse))

# lst <- lengths(st_intersects(predict_join_sf, complete_join_sf)) > 0
# 
# pt_intersections <- data.frame(lst) %>% 
#   rename("intersects" = "lst") %>% 
#   rownames_to_column("index")
# 
# intersections <- predict_join_sf %>% 
#   st_set_geometry(NULL) %>%
#   rownames_to_column("index") %>% 
#   left_join(., pt_intersections, by=c("index")) %>% 
#   filter(intersects == TRUE) %>% 
#   st_as_sf(coords = c("longitude", "latitude"),
#            crs = 4326, remove = FALSE) %>% 
#   st_transform(3857)
# 
# ggplot(intersections) +
#   geom_sf()

# exploring below
         
missing_dates <- filled_dates %>% 
  group_by(site_code) %>% 
  summarise(count = sum(is.na(mean_gwe)))

complete_sites <- missing_dates %>% filter(count < 12)
incomplete_sites <- missing_dates %>% filter(count > 60)

complete_coded <- year_month_merge %>% 
  filter(site_code %in% test_stations) %>% 
  filter(year %in% years) %>% 
  mutate(complete = case_when(site_code %in% complete_sites$site_code ~ 2,
                              site_code %in% incomplete_sites$site_code ~ 1,
                              TRUE ~ 0))

complete_coded %>% 
  filter(complete != 0) %>% 
  ggplot() +
  geom_point(aes(x = longitude, y = latitude, color = complete)) #+
  xlim(c(-123, -121)) + 
  ylim(c(37.5, 39))

# exploring monthly data
test_numbers <- monthly %>%
  group_by(STATION) %>% 
  summarise(max = max(MSMT_DATE),
            min = min(MSMT_DATE),
            count = n()) %>% 
  mutate(max_year = format(as.Date(max), "%Y"),
         min_year = format(as.Date(min), "%Y")) 

test_numbers %>% 
  ggplot() +
  geom_point(aes(x = max_year, y = min_year),
             alpha = .05)

useable_stations <- test_numbers %>% 
  filter(max_year >=2020,
         min_year <= 2010) 

monthly_station <- useable_stations$STATION

monthly_filtered <- monthly %>% 
  filter(STATION %in% monthly_station) %>% 
  rename("site_code" = "STATION",
         "dates" = "MSMT_DATE") %>% 
  filter(dates < as.Date("2021-01-01")) %>% 
  filter(dates >= as.Date("2010-01-01")) %>% 
  mutate(dates = format(as.Date(dates), "%Y-%m")) 
  
temp <- monthly_filtered %>% 
  group_by(site_code) %>% 
  summarise(count = n()) %>% 
  mutate(diff = 132 - count) %>% 
  summarise(sum(diff))

dates <- seq(as.Date("2010/01/01"), by = "month", length.out = 132)
sites <- data.frame(site_code = unique(monthly_filtered$site_code))

rep_codes <- data.frame(site_code = sites[rep(seq_len(nrow(sites)), length(dates)),
                                          c('site_code')])
dates_df <- data.frame(dates = dates)
rep_dates <- data.frame(dates = dates_df[rep(seq_len(nrow(dates_df)), each = 300), ])
rep_df <- cbind(rep_codes, rep_dates) %>% 
  mutate(dates = format(as.Date(dates), "%Y-%m"))

filled_dates <- left_join(rep_df, monthly_filtered, by = c("site_code", "dates"))
write.csv(filled_dates, "Analysis/Build/monthly_good_sites.csv", row.names = FALSE)

# now i need to interpolate the data

missing_dates <- filled_dates %>% 
  group_by(site_code) %>% 
  summarise(count = sum(is.na(mean_gwe)))
            