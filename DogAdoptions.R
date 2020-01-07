#Dog Adoptions
#Tidy Tuesdays

library(tidyverse)
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')


#Find most popular doggy name in each state

dog_names <- dog_descriptions %>%
  count(breed_primary)
