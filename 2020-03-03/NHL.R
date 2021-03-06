#NHL Stats
#Ian Bell TidyTuesday Mar 3 2020

library(tidyverse)
library(gghighlight)
library(ggtext)

###Data

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')


###Tidy

player_cumulative <- season_goals %>%
  filter(league == "NHL") %>%
  filter(team != "TOT") %>%
  group_by(player) %>%
  mutate(Goals = cumsum(goals)) %>%
  mutate(c_assists = cumsum(assists)) %>%
  mutate(Points = cumsum(points)) %>%
  pivot_longer(
    cols = c("Goals", "Points"),
    names_to = "metric",
    values_to = "value")


###Plot

Goal_Age <- ggplot()+
  geom_line(player_cumulative, mapping = aes(x=age, y=value, color = player), size = 1.25)+
  facet_wrap(~metric)+
  gghighlight(player == "Wayne Gretzky" | player == "Alex Ovechkin"	, calculate_per_facet = TRUE, use_direct_label = FALSE)+
  theme_minimal()+
  labs(
    title = "<span style = 'font-size:20pt'><b>The Great One Could *Pass*</b><br><span style = 'font-size:16pt'>Current NHL star <span style = 'color:#F8766D;'>**Alex Ovechkin**</span> just scored his 700th career goal, approaching hockey legend <span style = 'color:#00BFC4;'>**Wayne Gretzky**</span>'s total goals record of 894. But when we look at total accumulated  **points** (goals + assists), no one past or present can touch Gretzky.", x= "Age", y = "", caption = "Gray lines represent top 250 NHL Goal Scorers\nData: Washington Post • #TidyTuesday • @Ian_Bellio") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = NULL
    ))+
  theme(text = element_text(size=18, family = "Helvetica"))+
  theme(strip.background = element_rect(color="black", size=1.25, linetype="solid"), )+
  theme(legend.position = "none")+
  theme(plot.caption = element_text(size = 12))

Goal_Age

ggsave("NHL.png", dpi = 300, width = 9.5, height = 6.5, units = "in")
