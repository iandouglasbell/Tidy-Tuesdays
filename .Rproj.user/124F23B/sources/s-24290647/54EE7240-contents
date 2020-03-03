# Austrailian Fires and Weather
# Tidy Tuesday   Ian Bell 
# January 7 2020

#Data 
library(tidyverse)
library(lubridate)
library(ggridges)

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

#Wrangle

SE_AUS_MaxTemps <- temperature %>%
  filter(city_name != "PERTH") %>%
  filter(city_name != "PORT") %>%
  filter(temp_type == "max") %>%
  mutate(year = year(date)) %>%
  mutate(decade = floor(year/10) * 10) %>%
  group_by(decade) 


#Plot

TempPlot <- ggplot(SE_AUS_MaxTemps, aes(x=temperature, y=decade, group= decade, fill=..x.., alpha = 0.4)) + geom_density_ridges_gradient(scale=5, alpha = 0.5) + scale_y_reverse(breaks=c(1900, 1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010),labels = c("1900" = "", "1910" = "1910s", "1920" = "1920s", "1930" = "1930s", "1940" = "1940s", "1950" = "1950s", "1960" = "1960s", "1970" = "1970s", "1980" = "1980s", "1990" = "1990s", "2000" = "2000s", "2010" = "2010s")) + scale_fill_viridis_c(option = "plasma") +  labs(title = "SE Australia Max Temps Are Shifting Warmer", subtitle = 'Maximum daily temperature distributions by decade from select weather stations in \n Adelaide, Brisbane, Canberra, Melbourne, and Sydney' , caption = 'Data from Australian Bureau of Meteorology \n Some decades do not have complete records\n#TidyTuesday • @Ian_Bellio', x= "Temperature (°C)", y= "") + guides(fill=guide_legend(title="°C")) + theme_minimal() + theme(plot.title = element_text(size=24, family = "Source Sans Pro Semibold")) + theme(text = element_text(size=18, family = "Source Sans Pro")) 
TempPlot

ggsave("SEAusTemps.png", dpi = 300, width = 11, height = 10, units = "in")
