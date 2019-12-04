#Tidy Tuesday
#December 2, 2019 
#Ian Bell 

#Data about Philadelphia Parking Violations


library(rgdal)
library(lubridate)
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

#Pull in data
#Tidy Tuesday parking ticket data from 2017
tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

#Explore the types of violations

Violation_list <- tickets %>%
  count(violation_desc) %>%
  arrange(desc(n))

#Narrow down the violations to those that involve handicap violations

scummy  <-  tickets  %>%
  filter(violation_desc == "COUNTERFEIT HP PERM" | violation_desc == "FRAUD PARK HP SPACE" | violation_desc == "HP RAMP BLOCKED" | violation_desc == "HP RESERVED SPACE")

#Get OSM data for basemap

philly_map <- get_map(getbb("Philadelphia"), maptype = "stamen")

#plot data on background map

philly_plot <- ggmap(philly_map) + stat_density2d(
  aes(x = lon, y = lat, fill = ..level..),
  size = 0.01, , alpha = 0.5, bins = 30, data = scummy, geom = "polygon", contour=TRUE) + scale_fill_viridis_c(option = "plasma")  + labs(title = " 2017 Philadelphia Pennsylvania Handicap Parking Violations", subtitle = 'Tickets issued for counterfeit permits, blocking ramps, and illegally parking in handicap spaces' ,caption = 'Data by Open Data Philly\n#TidyTuesday • @Ian_Bellio')  +  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank()) + guides(fill = guide_colorbar(title = "Handicap-Related\n Violations")) + theme(plot.title = element_text(size=22, family = "Source Sans Pro Semibold")) + theme(text = element_text(size=15, family = "Source Sans Pro"))
philly_plot  

ggsave("HP Violations.png", dpi = 300, width = 12, height = 10.5, units = "in", bg = 'grey20')
  