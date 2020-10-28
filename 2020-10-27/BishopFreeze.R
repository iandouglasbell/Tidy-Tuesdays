# Freeze History Bishop
# Ian Bell 
# October 2020

library(tidyverse)
library(rnoaa)
library(lubridate)
library(ggtext)

### DATA

stations <- ghcnd_stations(refresh = TRUE)
# Bishop airport 	USW00023157

bishop <- meteo_tidy_ghcnd( stationid = "USW00023157", keep_flags = FALSE)

#prcp: Precipitation, in tenths of millimeters
#tavg: Average temperature, in tenths of degrees Celsius
#tmax: Maximum temperature, in tenths of degrees Celsius
#tmin: Minimum temperature, in tenths of degrees Celsius
#AWDR = Average daily wind direction (degrees)
#AWND = Average daily wind speed (tenths of meters per second)

### TIDY

#convert key columns into normal units 

bishop_data <- bishop %>% 
  mutate(prcp_in = (prcp*.10) * 0.0393701, tavg_f = ((tavg*.10)*(9/5)) + 32, tmin_f = ((tmin*.10)*(9/5)) + 32, tmax_f = ((tmax*.10)*(9/5)) + 32, wind_mph = awnd*0.223694, year = year(date), month = month(date), decade = floor(year/10) * 10, yday = yday(date)) 

freeze_days <- bishop_data %>%
  group_by(year) %>%
  filter(tmin_f < 32) %>%
  filter(year >= 1944) %>%
  mutate(yday = as.Date(yday-1, origin = as.Date("2018-01-01")))

frost_first <- bishop_data %>%
  group_by(year, decade) %>%
  filter(tmin_f < 32, month > 7) %>%
  summarise(min = min(date))

frost_last <- bishop_data %>%
  group_by(year) %>%
  filter(tmin_f < 32, month < 7) %>%
  summarise(max = max(date))

bishop_freeze <- inner_join(frost_first, frost_last, by = "year") %>%
  inner_join(number_frost_days, by = "year") %>%
  mutate(first = yday(min), last = yday(max)) %>%
  mutate(first = as.Date(first-1, origin = as.Date("2018-01-01")), last = as.Date(last-1, origin = as.Date("2018-01-01"))) %>%
  filter(year >= 1944)

### PLOT 

freeze <- ggplot() +
  geom_point(data = freeze_days, aes(y=year, x = yday), color = "gray", size = 0.8)+
  geom_point(data=bishop_freeze, aes(y=year, x=last), size = 3, color = "purple")+
  geom_point(data=bishop_freeze, aes(y=year, x=first), size = 3, color = "blue")+
  scale_y_reverse(breaks= seq(1950, 2020, 10), minor_breaks = seq(1944, 2020, 2)) +
  scale_x_date(date_labels = "%b", breaks = "month") +
  theme_minimal() +
  labs(title = "<b>Bishop California Freeze Dates</b><br><span style = 'font-size:14pt'>Bishop, CA is a desert city located in the rainshadow of the Sierra Nevada.   The<span style = 'color:purple;'> last freeze of winter</span> can be anytime from March to June, while the <span style = 'color:blue;'>first freeze</span> occurs from September to November. Gray dots indicate days with a min temp below freezing.</span>", caption = "Data: NOAA GHCN | #TidyTuesday | @Ian_Bellio", x= "", y= "") +
  theme( plot.title.position = "plot",
plot.title = element_textbox_simple(
  size = 13,
  lineheight = 1,
  padding = margin(5.5, 5.5, 5.5, 5.5),
  margin = margin(0, 0, 5.5, 0),)) 
freeze

ggsave("BishopFreeze.png", height= 6.5, width = 8, dpi = 300)
