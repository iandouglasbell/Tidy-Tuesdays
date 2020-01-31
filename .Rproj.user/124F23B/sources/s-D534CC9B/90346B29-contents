#San Francisco Trees 
#Tidy Tuesday   Ian Bell 
# Jan 27 2020

library(tidyverse)
library(rgdal)
library(lubridate)
library(osmdata)
library(sf)
library(ggmap)


sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

#Get OSM data for basemap

sf_map <- get_map(getbb("San Francisco"), maptype = "satellite", scale = "10", force=TRUE)

#plot data on background map

SF <- ggplot() + geom_point(sf_trees, mapping = aes(x=longitude, y=latitude))
SF

sf_plot <- ggmap(sf_map) + stat_density2d(
  aes(x = longitude, y = latitude, fill = ..level..),
  size = 0.01, , alpha = 0.5, bins = 30, data = sf_trees, geom = "polygon", contour=TRUE) + 
  scale_fill_viridis_c(option = "plasma")  + 
  labs(title = " 2017 Philadelphia Pennsylvania Handicap Parking Violations", subtitle = 'Tickets issued for counterfeit permits, blocking ramps, and illegally parking in handicap spaces' ,caption = 'Data by Open Data Philly\n#TidyTuesday • @Ian_Bellio') +
  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank()) + guides(fill = guide_colorbar(title = "Handicap-Related\n Violations")) + 
  theme(plot.title = element_text(size=22, family = "Source Sans Pro Semibold")) + theme(text = element_text(size=15, family = "Source Sans Pro"))
sf_plot  

ggsave("HP Violations.png", dpi = 300, width = 12, height = 10.5, units = "in")

