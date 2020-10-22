#Chopped String Practice 

library(tidyverse)
library(qualV)
library(fuzzyjoin)
library(stringi)

us_cities_states_counties <- read_delim("https://raw.githubusercontent.com/grammakov/USA-cities-and-states/master/us_cities_states_counties.csv", 
                                        "|", escape_double = FALSE, trim_ws = TRUE)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')


#Tidy

us_cities <- us_cities_states_counties %>%
  unite(city, "City alias","State short", sep = " ") %>%
  select(city)

chopped1 <- chopped %>%
  pivot_longer(cols = c("contestant1_info", "contestant2_info", "contestant3_info", "contestant4_info"), names_to = "contestant", values_to = "details") %>%
  mutate(hometown = word(details, start = -4, end = -1))

chopped2 <- chopped %>%
  pivot_longer(cols = c("contestant1_info", "contestant2_info", "contestant3_info", "contestant4_info"), names_to = "contestant", values_to = "details") %>%
  mutate(hometown = str_extract_all(details, "[:upper:][:upper:]"))

chopped3 <- chopped %>%
  pivot_longer(cols = c("contestant1_info", "contestant2_info", "contestant3_info", "contestant4_info"), names_to = "contestant", values_to = "details") %>%
  fuzzy_inner_join(us_cities, by = c("details" = "city"), match_fun = str_detect)



chopped_locations <- fuzzy_inner_join(chopped, us_cities, by = c("contestant2_info" = "city"), match_fun = str_detect)
