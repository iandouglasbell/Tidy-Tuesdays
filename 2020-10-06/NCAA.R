#NCAA BBall Womens Stats
#TidyTuesday
#Oct 6 2020 Ian Bell

library(tidyverse)
library(ggthemes)
library(showtext)
library(ragg)

font_add_google("Roboto", "roboto")
font_add_google("Secular One", "so")
showtext_auto()

#DATA

tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

#TIDY

champs <- tournament %>%
  group_by(school) %>%
  filter(any(tourney_finish == "Champ")) %>%
  mutate(place = case_when(
    tourney_finish == "1st" ~ 1, 
    tourney_finish == "2nd" ~ 2,
    tourney_finish == "RSF" ~ 3, 
    tourney_finish == "RF" ~ 4, 
    tourney_finish == "NSF" ~ 5,
    tourney_finish == "N2nd" ~ 6, 
    TRUE ~ 7))



#PLOT

champ_plot <- ggplot(data = champs) + 
  geom_line(aes(year, place, color = school), size = 1.1) +
  geom_point(aes(year, place, color = school), size = 2) +
  facet_wrap(~school) +
  theme(plot.title = element_text(size=26, family = "so")) +
  theme_fivethirtyeight(base_size = 14, base_family = "roboto") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7), labels = c("1" = "1st Round", "2" = "2nd Round", "3" = "Sweet 16", "4" = "Elite Eight", "5" = "Semis", "6" = "Runner up", "7" = "Nat Champs")) +
  labs(title = "NCAA Women's Basketball Championship Programs", caption  = "Data:FiveThirtyEight   Graphic: @Ian_Bellio ") +
  theme(strip.background = element_rect(color="#7A7676", fill="#FDF7C0", size=0.5, linetype="solid"))
champ_plot

