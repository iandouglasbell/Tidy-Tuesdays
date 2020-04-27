#Measles Vaccinations
#Ian Bell Feb 25 2020

library(tidyverse)
library(sf)
library(ggtext)
library(patchwork)
library(scales)
library(gridExtra)

####Data
measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

####Tidy

#Ca Schools with MMR rates below 95%
CA_Vacc <- measles %>%
  filter(state == 'California') %>%
  filter(!is.na(lat)) %>%
  filter(overall != -1) %>%
  filter(mmr < 95)

#Convert into sf point object
CA_Schools <-  st_as_sf(CA_Vacc, coords = c("lng", "lat"), remove= FALSE, agr = "constant", crs = 4326) 

#Import shapefile of California Counties
CA_Counties <- st_read('CA_Counties/CA_Counties_TIGER2016.shp')

#For Table of Worst MRR rates for CA schools with at least 100 students
CA_Lowest <- measles %>%
  filter(state == 'California') %>%
  filter(overall != -1) %>%
  top_n(-15, mmr) %>%
  arrange(mmr) %>%
  mutate(mmr= mmr/100) %>%
  mutate(mmr = percent(mmr)) %>%
  rename(Name = name, City = city, "MMR Vaccination Rate" = mmr)

####Plot

# CA with schools <95%
CA_Plot2 <- ggplot() +
  geom_sf(data = CA_Counties, fill = 'white') +
  geom_sf(data = CA_Schools, color = 'red', size = 1) +
  theme_minimal() +
  coord_sf(datum = NA) +
  labs(
    title = "<span style = 'font-size:12pt'>1,566 schools had a MMR vaccination rate<span style = 'color:red;'> below 95%.") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = "cornsilk"
    ))

#CA with schools <90%
CA_Plot3 <- ggplot() +
  geom_sf(data = CA_Counties, fill = 'white') +
  geom_sf(data = subset(CA_Schools, mmr <90), color = 'purple', size = 1) +
  theme_minimal() +
  coord_sf(datum = NA) +
  labs(
    title = " <span style = 'font-size:12pt'> 508 schools had a MMR vaccination rate<span style = 'color:purple;'> below 90%. </span>") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = "cornsilk"
    ))

#Table of schools with lowest MMR vaccination rates
mytheme <- ttheme_minimal(
  core = list(fg_params=list(cex = 0.80)),
  colhead = list(fg_params=list(cex = 0.9)),
  rowhead = list(fg_params=list(cex = 0.9)))

Table <- tableGrob(CA_Lowest[1:10, c('Name', 'City' ,'MMR Vaccination Rate')], theme=mytheme)


#Combine plots and table
patchwork_plot <- (CA_Plot2 + CA_Plot3)/(Table)
patchwork_plot  + plot_annotation(
  title = 'California School Measles Vaccination Rates 2018-2019',
  subtitle = 'For measles, ~90-95% of a population needs immunity to prevent spread of the disease.\nThe table below shows the lowest MMR (Measles, Mumps, Rubella) vaccination rate schools.',
  caption = 'Data: Wall Street Journal | @Ian_Bellio | #TidyTuesday', theme = theme(plot.title = element_text(size = 18), plot.subtitle = element_text(size = 12))
)

#Save plot
ggsave("CA_Measles.png", dpi = 400, width = 7.1, height = 7, units = "in")
