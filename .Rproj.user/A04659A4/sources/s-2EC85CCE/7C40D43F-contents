#Tidy Tuesday Climbing Expeditions
# September 29 2020
# Ian Bell 

library(tidyverse)
library(scales)
library(showtext)


font_add_google("Muli", "muli")
font_add_google("Permanent Marker", "pmarker")
font_add_google("Roboto", "roboto")
font_add_google("Secular One", "so")
showtext_auto()

#### DATA
members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

#### TIDY
# Look at where climbers get AMS

AMS_ht <- members %>%
  filter(injury_type == "AMS") %>%
  filter(member_id != "LHOT08110-01") %>%
  mutate(injury_height_ft = injury_height_metres * 3.28084)

#### PLOT

AMS_ht_plot <- ggplot(data = AMS_ht, aes(oxygen_used, injury_height_ft, fill = oxygen_used)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 2) +
  geom_point(position = position_jitter(width = .15), size = 1) +
  geom_boxplot(aes(x = as.numeric(oxygen_used) + 1, y = injury_height_ft),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  coord_flip() +
  theme_minimal() +
  theme_minimal(base_size = 16, base_family = "roboto") +
  theme(plot.title = element_text(size=26, family = "so")) +
  theme(legend.position = "none") +
  scale_y_continuous(label=comma) +
  labs(title = "Acute Mountain Sickness in Himalayan Climbing", subtitle = "Acute Mountain Sickness (AMS) is an altitude-related illness caused by the reduced\n oxyen at altitude. With supplemental oxygen, AMS occured at a median altitude of\n 24,442 ft. Without, AMS occured at a median altitude of 20,997 ft.", x = "Supplemental Oxygen Used", y = "AMS Injury Height (ft)")

AMS_ht_plot

ggplot_build(AMS_ht_plot)$data
