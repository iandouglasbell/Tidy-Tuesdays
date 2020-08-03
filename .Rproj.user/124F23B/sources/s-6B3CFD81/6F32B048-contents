#Beach Volleyball
#Tidy Tues
#May 19 2020, Ian Bell


library(tidyverse)
library(lubridate)

#Data

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

US_humans <- readr::read_csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/births/US_births_2000-2014_SSA.csv')

#Tidy

vb_1 <- vb_matches %>%
  select(c("w_player1","w_p1_birthdate", "w_p1_country")) %>%
  rename(., c("player" = "w_player1", "birthdate" = "w_p1_birthdate", "country" = "w_p1_country"))

vb_2 <- vb_matches %>%
  select(c("w_player2","w_p2_birthdate", "w_p2_country")) %>%
  rename(., c("player" = "w_player2", "birthdate" = "w_p2_birthdate", "country" = "w_p2_country"))

vb_3 <- vb_matches %>%
  select(c("l_player2","l_p2_birthdate", "l_p2_country")) %>%
  rename(., c("player" = "l_player2", "birthdate" = "l_p2_birthdate", "country" = "l_p2_country"))

vb_4 <- vb_matches %>%
  select(c("l_player2","l_p2_birthdate", "l_p2_country")) %>%
  rename(., c("player" = "l_player2", "birthdate" = "l_p2_birthdate", "country" = "l_p2_country"))

vb_usa <- vb_1 %>%
  bind_rows(vb_2, vb_3, vb_4) %>%
  distinct(., .keep_all = FALSE) %>%
  filter(country == "United States") %>%
  mutate(month = month(birthdate), day = day(birthdate), yday = yday(birthdate)) %>%
  drop_na()

US <- US_humans %>%
  mutate(date = make_date(2000, month, date_of_month)) %>%
  mutate(yday = yday(date))

####Plot

plot1 <- ggplot() + geom_density(data = vb_usa, aes(x=yday)) 
plot1

