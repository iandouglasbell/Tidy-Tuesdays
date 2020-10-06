#Educatoin Spedning
#TidyMonday Ian Bell Septembet 14 2020

library(tidyverse)
library(showtext)

#Data
kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')
regions <- readr::read_csv('https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv')

font_add_google("Oswald", "Oswald", regular.wt = 400, bold.wt = 1000)
font_add_google("Open Sans", "Open Sans", regular.wt = 400, bold.wt = 800)
font_add_google("Oswald", "title", regular.wt = 400, bold.wt = 1000)
showtext_auto()

#Tidy
kidpandr <- kids %>%
  filter(variable == "parkrec") %>%
  inner_join(regions, by = c("state" = "State"))%>%
  arrange(-year, Region, -inf_adj_perchild) %>%
  group_by(year, Region) %>%
  mutate('2016rank' = rank(-inf_adj_perchild), percap = inf_adj_perchild*1000) %>%
  rename(state_code = "State Code")

sixteen <- kidpandr %>%
  filter(year == "2016") %>%
  mutate(state = fct_reorder(state, percap))



#Plot

pandrplot <- ggplot() +
  geom_line(data = kidpandr, aes(x = year, y = percap, group = state), color = "gray", size = 1.25) +
  geom_line(data = subset(kidpandr, state == "Indiana"), aes(x = year, y = percap), color = "red", size = 1.25) +
  theme_minimal()
pandrplot

pandrplot2 <- ggplot() + 
  geom_col(sixteen, mapping = aes(y=percap, x= state, fill = Region)) +
  coord_flip() +
  facet_wrap(~Region, scales = "free_y") +
  theme_minimal(base_size = 14, base_family = "Open Sans") +
  theme(plot.title = element_text(size=40, family = "title", face = "bold")) +
  labs(title = "Parks and Recreation", subtitle = "2016 state funding per child for Parks and Recreation. Indiana had the\n lowest Parks and Rec funding in the Midwest in 2016.", x = "", y = "Parks and Rec Funding per Child", caption = "Data: Urban instatute   Graphic: I. Bell") +
  scale_y_continuous(labels = scales::label_dollar())
pandrplot2 + theme(plot.subtitle = element_text(size = 16), 
    panel.grid.major = element_line(colour = "gray80"), 
    panel.grid.minor = element_line(linetype = "blank"), 
    axis.text = element_text(size = 10), 
    plot.title = element_text(family = "serif", 
        size = 43), panel.background = element_rect(fill = "aliceblue"), 
    plot.background = element_rect(fill = "whitesmoke"), 
    legend.position = "none") +
  theme(plot.margin=unit(c(0.5,2,0.5,0.5),"cm"))

ggsave("PandR.png", dpi = 300, height = 7, width = 10)
