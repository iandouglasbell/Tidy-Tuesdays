#Endangered Plants
#Tidy Tuesday Aug 17 2020

library(tidyverse)
library(waffle)

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

#Tidy

threat <- threats %>%
  filter( red_list_category == "Extinct" & threatened == "1" ) %>%
  group_by(continent, threat_type) %>%
  summarise(count = n()) %>%
  mutate(continent_f = factor(continent, levels = c('Africa', 'North America', 'South America', 'Asia', 'Oceania', 'Europe')))


# Plot ----
waffle_plot <-ggplot(data = threat, aes(fill = threat_type, values = count))  +
  geom_waffle(n_rows = 8, size = 1, color="white", flip = TRUE) +
  facet_wrap(~continent_f, nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  coord_equal() +
  theme_enhance_waffle() +
  labs(
    title = "Global Plant Extinctions",
    subtitle = "Documented Extinct Plant Species by Continent and Cause of Extinction",
    caption = "
    Data: IUCN Red list of Threatened Species \n Species with multiple extinction causes are represented multiple times \n Graphic: @Ian_Bellio",
    fill = "Extinction Cause"
  ) +
  theme(text = element_text(size=15)) +
  theme(strip.text = element_text(face="bold", size=12.5))
waffle_plot

ggsave("plant_extinctions.png", dpi = 300, width = 11, height = 10, units = c("in"))
