#Hotel Data
#Ian Bell 
#Tidy Tuesday Feb 11, 2020

library(tidyverse)
library(tsibble)
library(feasts)

#Data
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

countries <- readr::read_csv('https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv')

  
hotels <- inner_join(hotels, countries, by = c("country" = "alpha-3")) 

months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

#Tidy

busy_time <- hotels %>%
  filter(region == "Europe") %>%
  group_by(name) %>%
  count() %>%
  filter(n >= 4000) %>%
  ungroup()


  ungroup (name) %>%
  group_by(name, arrival_date_month) %>%
  count()


busy_time_plot <-  ggplot(busy_time, aes(x=arrival_date_month, y=n)) + geom_col() + facet_wrap(~name, scales = "free")

busy_time_plot


hotel_plot <- hotels %>% 
  filter(hotel == "City Hotel") %>%
  mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
         date = parse_date(date, format = "%Y-%B-%d")) %>% 
  select(date, everything()) %>% 
  arrange(date) %>% 
  count(date) %>% 
  rename(daily_bookings = n) %>% 
  tsibble::as_tsibble() %>% 
  model(STL(daily_bookings ~ season(window = Inf))) %>% 
  components() %>% autoplot()

hotel_plot
