# Rap
# Tidy Tuesdays
# Ian Bell April 14, 2020

library(tidyverse)
library(genius)
library(fuzzyjoin)
library(lubridate)
library(ggridges)

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

rankings_lyrics <- add_genius(rankings, artist, title, type = "lyrics")

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

Rap_HH <- spotify_songs %>%
  distinct(track_name, track_artist, .keep_all = TRUE) %>%
  filter(playlist_genre == "rap") 
  
  
Rap_Lyrics <-  add_genius(Rap_HH, track_artist, track_name, type = "lyrics")

Rap_Lyrics_Count <- Rap_Lyrics %>%
  unnest_tokens(word, lyric) %>%
  count(word, sort = TRUE)

Drug_Names <- read_csv("2020-04-13/DEADrugNames.csv")

Join <- fuzzy_inner_join(Rap_Lyrics, Drug_Names, by = c("lyric" = "slang_code_word"), match_fun = str_detect)

Car_brands <- readr::read_csv('2020-04-13/CarBrands.csv')

Join2 <- fuzzy_inner_join(Rap_Lyrics, Car_brands, by = c("lyric" = "brand"), match_fun = str_detect) %>%
  mutate(year_released=ymd(track_album_release_date, truncated = 2L)) 

whip_counts <- Join2 %>%
  distinct(track_id, brand, .keep_all = TRUE) %>%
  filter(brand != "Geo") %>%
  filter(brand != "Eagle") %>%
  group_by(brand, year_released) %>%
  summarize(count = n())

carplot1 <- ggplot() + geom_point(data = whip_counts, aes(x=year_released, y=count, color=brand)) + theme(legend.position = "none") + facet_wrap(~brand)
carplot1


