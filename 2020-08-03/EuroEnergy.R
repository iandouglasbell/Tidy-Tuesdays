#European Energy
#Tidy Tuesday 
#Ian Bell August 4 2020

library(tidyverse)
library(gt)

#Data
energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')
population <- read_csv("2020-08-03/UNdata_Export_20200803_210212723.csv")

#Tidy
population <- population %>%
  mutate(Population = Value * 1000) %>%
  rename(country = `Country or Area`, Year = `Year(s)`) %>%
  select(country, Year, Population)

country_total <- country_totals %>%
  mutate(country_name = case_when(country == "EL" ~ "Greece", country == "UK" ~ "United Kingdom", country == "BA" ~ "Bosnia and Herzegovina", TRUE ~ country_name)) %>%
  filter(type == "Total net production") %>%
  pivot_longer(cols = c(5:7), names_to = "Year", values_to = "Energy") %>%
  mutate(Year = as.numeric(Year))%>%
  left_join(.,population, by = c("country_name" = "country", "Year" = "Year")) %>%
  mutate(per_cap = (Energy / Population) * 1000) %>%
  filter(Year == "2018") %>%
  arrange(-per_cap) %>%
  mutate(rank = 1:nrow(.)) %>%
  top_n(-10)
  

#Plot
table <- gt(country_total) %>%
  tab_header(title = "Per Capita European Energy Production", subtitle = "2018 total net energy production, top ten displayed") %>%
  cols_hide(c("country", "type", "level" , "Year")) %>%
  cols_move_to_start("rank") %>%
  cols_label(rank = "", country_name = "Country", Energy = "Net Energy Production (GWh)", per_cap = "Per Capita Energy Production (MWh/person)") %>%
  fmt_number(columns = c("Population", "Energy"), decimals = 0, use_seps = TRUE) %>%
  fmt_number(columns = "per_cap", decimals = 2, use_seps = TRUE) %>% 
  tab_options(table.background.color = "lightcyan") %>%
  tab_style(style = cell_text(style = "italic"), locations = cells_body("per_cap")) %>%
  tab_source_note(source_note = "Data from Eurostat and UN Data  |  #TidyTuesday  | @IanBellio") 

table
