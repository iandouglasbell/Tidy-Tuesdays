#Friends Tidy Tuesday
#Ian Bell 
#September 8, 2002

library(tidyverse)
library(showtext)
library(ggtext)

#Data
friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')

font_add_google("Muli", "muli")
font_add_google("Permanent Marker", "pmarker")
showtext_auto()

#Tidy

friend <- friends %>%
  filter(speaker == "Scene Directions") %>%
  mutate(central_perk = str_detect(text, "Central Perk")) %>%
  filter(central_perk == TRUE)

#Viz

coffee <- ggplot() +
  geom_point(data = friend, aes(x=episode, y=season), size = 8, color = "mediumblue", shape = 19) +
  geom_point(data = friends, aes(x=episode, y=season), size = 8, color = "mediumblue", shape = 1) +
  scale_y_reverse(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_x_continuous(breaks = c(5,10,15,20,25)) +
  theme_minimal(base_size = 20, base_family = "muli") +
  labs(title = "<span style = 'font-size:24pt;font-family: pmarker'><b>F<span style = 'color:tomato'>.</span>R<span style = 'color:yellow'>.</span>I<span style = 'color:cornflowerblue'>.</span>E<span style = 'color:tomato'>.</span>N<span style = 'color:yellow'>.</span>D<span style = 'color:cornflowerblue'>.</span>S at Central Perk Coffee </b><br><span style = 'font-size:16pt;font-family:muli'>After the first season of Friends, Central Perk coffee shop became a fixture of the show, with seasons 3 and 6 having only two episodes that skipped a scene at the coffee shop. Towards the end of the show, the first and last episodes of each season had no scenes at Central Perk." ,caption = 'Solid dots indicate at least one scene at Central Perk\nData: ceros interactive article and Emil Hvitfeldt   Viz: I. Bell', x = 'Episode', y = 'Season') +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(plot.background = element_rect(fill = 'honeydew')) +
  theme(plot.caption = element_text(size = 11, face = "italic")) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = "honeydew"))
coffee

