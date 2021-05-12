# Broadband TidyTuesday
# May 12, 2021
# Ian Bell

library(tidyverse)
library(tigris)
library(sf)
library(plyr)
library(scales)
library(ggtext)

###Data
broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')
broadbandzip <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband_zip.csv')

trib <- native_areas()

zipcodes <- zctas(cb = TRUE)

###Tidy

#combine zip codes with indian country lands based on location
native_area_zips <- st_join(trib, zipcodes) %>%
  mutate(ZIP = as.numeric(GEOID10))

#join native_area_zips with broadbandzip

native_broadband <-  left_join(broadbandzip,native_area_zips, by = c("POSTAL CODE" = "ZIP")) %>%
  mutate(indian_country = case_when(is.na(NAME) ~ "Other", TRUE ~ "Tribal Lands" ))

native_broadband$indian_country <- factor(native_broadband$indian_country, levels = c("Tribal Lands", "Other"))

###Plot

nativebroadband_plot <- ggplot(data = native_broadband, aes(x = indian_country, y = `BROADBAND USAGE`, color = indian_country)) + 
  geom_point(position = position_jitter(width = .4), size = 0.2) +
  geom_boxplot(aes(x = indian_country , y = `BROADBAND USAGE`),outlier.shape = NA, alpha = 0, width = 0.3, colour = "black") +
  scale_y_continuous(label=percent) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme_minimal(base_size = 16, base_family = "roboto") +
  theme(plot.title = element_text(size=26, family = "times new roman")) +
  theme(legend.position = "none") +
  labs(title = "Broadband Internet Use in Indian Country", subtitle = "In the United States, zip codes that contain tribal lands have lower broadband internet\n usage when compared with other zip codes. Pricing and lack of broadband infrastructure\n in tribal communities can impact education, economic development, and quality of life.  ", x = "", y = "Percent Broadband Usage", caption = "Data: Microsoft, US Census Bureau | #TidyTuesday | @Ian_Bellio")

nativebroadband_plot