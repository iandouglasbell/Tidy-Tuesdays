# Animal Crossing
#Tidy Tuesday April 5, 2020 Ian Bell

library(tidyverse)
library(lubridate)

#Data
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')
US_humans <- readr::read_csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/births/US_births_2000-2014_SSA.csv')


#Tidy
villagers_bdays <- villagers %>%
  separate(birthday, c("month", "day")) %>%
  mutate(date = make_date(2000, month, day)) %>%
  group_by(species, personality) %>%
  n_distinct()


human_bdays <- US_humans %>%
  mutate(date = make_date(2000, month, date_of_month)) %>%
  group_by(month, date_of_month, date) %>%
  summarise(births = sum(births)) %>%
  ungroup() %>%
  mutate(vs_avg = (births/mean(births)))

#graph
human_bday_plot <- ggplot(human_bdays) + geom_density(aes(x=date, y=vs_avg), stat = 'identity') + geom_hline(yintercept=1) +geom_text(data = data.frame(x = as.Date("2000-07-04"), y = 0.754613951076881,label = "4th of July"), mapping = aes(x = x, y = y, label = label),angle = 0L, lineheight = 1L, hjust = 0.5, vjust = 0.5, colour = "black",family = "sans", fontface = "plain", inherit.aes = FALSE, show.legend = FALSE)
human_bday_plot

animal_bday <- ggplot(villagers_bdays) + geom_bar(aes(x=date))
animal_bday
