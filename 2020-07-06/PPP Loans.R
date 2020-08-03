###PPP Plan data
# Ian Bell July 6 2020

library(tidyverse)
library(sf)
library(here)

setwd(here("2020-07-06"))

#Data
foia_150k_plus <- read_csv("~/Downloads/150k plus/foia_150k_plus.csv", 
                           col_types = cols(DateApproved = col_datetime(format = "%m/%d/%Y")))
View(foia_150k_plus)


#Wrangle

CDs <- st_read('tl_2018_us_cd116/tl_2018_us_cd116.shp')

CD_plot <- ggplot(CDs) + geom_sf()
CD_plot
