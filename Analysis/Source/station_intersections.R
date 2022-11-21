library(tidyverse)
library(lubridate)
library(sf)

setwd("/Users/reibertoldi/Documents/UBC/EOSC 510/Final Project/")

groundwater <- read_csv("data/measurements.csv")
stations <- read_csv("data/stations.csv")

ggplot(stations) +
  geom_point(aes(x = longitude, y = latitude))

merge_df <- left_join(groundwater, stations, by = "site_code") %>% 
  mutate(year = format(as.Date(msmt_date), "%Y"))

test_numbers <- merge_df %>%
  group_by(site_code) %>% 
  summarise(max = max(year),
            min = min(year),
            count = n()) %>% 
  filter(min <= 2015, 
         max >= 2020)

test_stations <- test_numbers$site_code
years <- c(seq(2015, 2020, 1))

monthly_gse <- merge_df %>% 
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
rep_dates <- data.frame(dates = dates_df[rep(seq_len(nrow(dates_df)), each = 8130), ])
rep_df <- cbind(rep_codes, rep_dates) %>% 
  mutate(dates = format(as.Date(dates), "%Y-%m"))  

filled_dates <- left_join(rep_df, monthly_gse, by = c("site_code", "dates"))

missing_years <- filled_dates %>% 
  separate(col = dates,
           into = c("year", "month"),
           sep = "-") %>% 
  group_by(site_code) %>% 
  summarise(missing = sum(is.na(mean_gse)))

complete_years_ids <- missing_years %>% filter(missing == 0)
complete_years_ids <- unique(complete_years_ids$site_code)

complete_ids <- filled_dates %>% 
  filter(site_code %in% complete_years_ids)

length(unique(complete_ids$site_code))

missing_years_ids <- missing_years %>% filter(missing >= 12, missing <= 24)
missing_years_ids <- unique(missing_years_ids$site_code)

predict_ids <- filled_dates %>% 
  filter(site_code %in% missing_years_ids)

# should I try to map this?

# buffering and intersecting stations

complete_join <- left_join(complete_ids, stations, by = "site_code")
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

intersection_counts <- final_intersections %>% 
  group_by(bad_site_code) %>% 
  summarise(count = n())

filled_dates %>% 
  filter(site_code == "389261N1213426W001") %>% 
  ggplot() +
  geom_point(aes(x=dates, y=mean_gse))
# 12 intersections 

filled_dates %>% 
  filter(site_code == "332486N1166558W001") %>% 
  ggplot() +
  geom_point(aes(x=dates, y=mean_gse))
# 4 intersections

