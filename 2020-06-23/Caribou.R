# Caribou Locations
# Ian Bell Tidy Tuesday June 23 2020

library(tidyverse)
library(lubridate)
library(rgbif)
library(moveVis)
library(move)

# Data
individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

locations_weekly <- locations %>%
  mutate(week = week(timestamp), year = year(timestamp), c_week = ceiling_date(timestamp, unit = "week")) %>%
  group_by(animal_id,c_week, study_site) %>%
  summarise(wk_lat = mean(latitude), wk_long = mean(longitude))

new_elev <- elevation(latitude = locations_weekly$wk_lat, longitude = locations_weekly$wk_long, username = "envirooviwc")

caribou_elev <- bind_cols(locations_weekly, new_elev) %>%
  mutate(year = year(c_week)) %>%
  filter(year >= 2001)

### Plot

Elev_plot <- ggplot(data = caribou_elev) + geom_line(aes(x=c_week, y= elevation_geonames, color = longitude)) + facet_wrap(~study_site)
Elev_plot


movedata <- df2move(locations, x= "latitude", y="longitude", proj = 4326, time = "timestamp", track_id = "animal_id")

data("locations", package = "moveVis")

m <- align_move(locations, res = 4 , unit = "mins")
