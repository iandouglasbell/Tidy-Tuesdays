#Astronauts 
#Tidy Tuesday
#Ian Bell

library(tidyverse)
library(lubridate)
library(ggridges)
library(ggbeeswarm)

#Data
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

#Filter
astronauts <- astronauts %>%
  mutate(age = year_of_mission - year_of_birth, decade = floor(year_of_mission/ 10) * 10) %>%
  mutate(Country = case_when(nationality == "U.S."~ "USA", nationality == "U.S.S.R/Russia" ~ "Russia", TRUE ~ "Other"))


#plot

age_dist <- ggplot(astronauts, aes(x=age, y=decade, group= decade, fill=..x.., alpha = 0.4)) + geom_density_ridges_gradient( alpha = 0.5) + scale_y_reverse(breaks=c(1960,1970,1980,1990,2000,2010),labels = c( "1960" = "1960s", "1970" = "1970s", "1980" = "1980s", "1990" = "1990s", "2000" = "2000s", "2010" = "2010s")) + theme_minimal() + facet_grid(~Country)
age_dist

age <- ggplot(astronauts, aes(Country, age,  color = Country)) + geom_quasirandom(size=1.75, varwidth = TRUE, width  = 0.60) + coord_flip() + scale_color_manual(values=c("#999999", "#DA291C", "#56B4E9"))  + theme_minimal() + theme(legend.position = "none") + labs(title = "Astronaut Age Distribution", x= "", y="Age")
age

eva <- ggplot(astronauts, aes(eva_hrs_mission, year)) + geom_col()
