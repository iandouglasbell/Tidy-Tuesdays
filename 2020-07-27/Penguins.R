

library(tidyverse)
library(ggalt)
library(ggtext)

###Data
penguins.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

penguins_raw.csv <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins_raw.csv')

###Tidy

penguins <- penguins_raw.csv %>%
  mutate(Species = case_when(Species == "Adelie Penguin (Pygoscelis adeliae)"~ "Adelie", Species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo", Species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap")) %>%
  rename("Delta15N" = 15, "Delta13C" = 16)


###Plot

Penguin_Plot <- ggplot(penguins, aes(Delta15N, Delta13C, color = Species)) + geom_point() + geom_encircle() + theme_minimal() + scale_color_manual(values = c("darkorange","darkorchid","cyan4")) + labs(title = "<b>Penguin Food Webs</b><br><span style = 'font-size:14pt'>The ratios of stable carbon and nitrogen isotopes in Antartic penguins can indicate food preferences. <span style = 'color:darkorchid;'>Chinstrap</span> and <span style = 'color:cyan4;'>Gentoo</span> penguins appear to feed on distinct ocean food sources, while <span style = 'color:darkorange;'>Adelie</span> penguins have overlap with both other species.</span>", caption = "Data: Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer\n Archipelago (Antarctica) penguin data. #TidyTuesday | @Ian_Bellio", x= expression(delta^{15}*" Nitrogen"), y= expression(delta^{13}*" Carbon")) + theme(
  plot.title.position = "plot",
  plot.title = element_textbox_simple(
    size = 13,
    lineheight = 1,
    padding = margin(5.5, 5.5, 5.5, 5.5),
    margin = margin(0, 0, 5.5, 0),
  ))
Penguin_Plot

ggsave("penguins.png", height= 6, width = 6, dpi = 300)
