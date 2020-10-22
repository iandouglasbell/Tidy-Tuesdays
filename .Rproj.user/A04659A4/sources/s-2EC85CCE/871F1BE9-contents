#Beer Awards
#Ian Bell 
#Tidy Tuesday October 20 2020

library(tidyverse)
library(glue)
library(leaflet)
library(geobuffer)
library(sf)

#DATA
beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

us_cities_states_counties <- read_csv('https://raw.githubusercontent.com/kelvins/US-Cities-Database/main/csv/us_cities.csv')


#TIDY

locations <- inner_join(beer_awards, us_cities_states_counties, by = c("state" = "STATE_CODE" , "city" = "CITY")) %>%
  distinct(medal, beer_name, brewery, city, state, .keep_all = TRUE) %>%
  filter(medal == "Gold") %>%
  group_by(LATITUDE, LONGITUDE) %>%
  mutate(all_pubs = paste(unique(brewery), collapse = ", "), all_beers = paste(beer_name, collapse = ", "), all_years = paste(year, collapse = ", "))

cities <- st_as_sf(locations, coords=c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = 7801)

buffer_1km <- st_buffer(cities, 80467) %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 4326)

#PLOT

map <- leaflet() %>%
  addMeasure(primaryLengthUnit = "meters") %>%
  addTiles() %>%
  addPolygons(data = buffer_1km) %>%
  addCircleMarkers(data = locations, ~LONGITUDE, ~LATITUDE, fillOpacity = 0.5, radius=3, color = "purple",
                   popup=~paste("<strong>City:</strong>", city, state,
                                "<br>",
                                "<strong>Gold Medal Brewery:</strong>",all_pubs))
map
