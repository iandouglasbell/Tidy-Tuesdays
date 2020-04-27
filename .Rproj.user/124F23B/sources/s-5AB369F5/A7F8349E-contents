#CArbon Footprints
#Tidy Tuesday Feb 18 2020
#Ian Bell

library(tidyverse)
library(waffle)
library(patchwork)
library(viridis)

#DATA
food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

other_sources <- tibble(source = c("Heating and Cooking", "Transportation", "Residential Electricity", "Air Travel"), carbon = c(5.85, 2.52, 6.68, 1.52)) #source: Carbon Footprint Calculations for an Average American https://externalassets.cooleffect.org/lib/content/wp-content/uploads/2018/04/05172819/CarbonFootprint_AverageAmerican.pdf 


#TIDY

top_countries <- food_consumption %>%
  group_by(country) %>%
  summarise(total = sum(co2_emmission))

top_6 <- food_consumption %>%
  filter(country == "Argentina" | country == "Australia" | country == "Albania" | country == "New Zealand" | country == "Iceland" | country == "USA")


top6plot <- ggplot(top_6, aes(x=reorder(country, co2_emmission), y=co2_emmission, fill=food_category, color = food_category))+
  geom_col() +
  coord_flip() +
  geom_text(data = data.frame(x = 2.29644221051345,
                              y = 2021.22245900281,
                              label = "New Zealand and \nIceland consume\n more mutton"),
            mapping = aes(x = x,
                          y = y,
                          label = label),
            angle = 0L,
            lineheight = 1L,
            hjust = 0.5,
            vjust = 0.5,
            colour = "black",
            family = "sans",
            fontface = "bold",
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_minimal() + 
  theme(text = element_text(size=16, family = "Verdana")) + 
  labs(title = "The Carbon Footprint of What We Eat", subtitle = 'Top 6 Nations with the largest food carbon footprint' , y= "Carbon Dioxide Emission (Kg CO2/person/year)", caption = 'Based on annual averages\nData by Spotify via spotifyr package\n#TidyTuesday   @Ian_Bellio') +
  scale_fill_viridis(discrete = TRUE, direction = -1) 



top6plot

#+ 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Faceted Waffle Bar Chart",
    subtitle = "{dplyr} storms data",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))


other <- other_sources %>%
  mutate(carbon = carbon * 1000) #convert from metric ton to kg

Meat_Capitols <- food_consumption %>%
  mutate(Meat= case_when(food_category == "Beef" | food_category == "Fish" | food_category == "Lamb & Goat" | food_category == "Milk - inc. cheese" | food_category == "Pork" | food_category == "Poultry"| food_category == "Eggs" ~ "Meat", TRUE ~ "Not Meat")) %>%
  filter(Meat == "Meat") %>%
  group_by(food_category) %>%
  top_n(5, consumption)


food <- food_consumption %>%
  filter(country == "USA") %>%
  mutate(Meat= case_when(food_category == "Beef" | food_category == "Fish" | food_category == "Lamb & Goat" | food_category == "Milk - inc. cheese" | food_category == "Pork" | food_category == "Poultry"| food_category == "Eggs" ~ "Meat", TRUE ~ "Not Meat")) %>%
  group_by(Meat) %>%
  summarise(carbon= sum(co2_emmission)) %>%
  mutate(source = "Food")

carbon <- full_join(food, other)

carbon_flight <- carbon %>%
  filter(source == "Air Travel")

#VISUALIZE

top_countries <- ggplot(Meat_Capitols, aes(x=reorder(country, consumption), y= consumption, fill = consumption)) + geom_col() + facet_wrap(~food_category, scales = "free") + coord_flip()
top_countries

Waffle = waffle(carbon_flight$carbon/10, rows =10 , keep = TRUE, xlab = NULL, title = 'Air Travel', size = 1, flip = FALSE, reverse = FALSE, equal = TRUE, pad = 0, use_glyph = TRUE, glyph_size = 20, legend_pos = "right")
Waffle

waffle2 = waffle(carbon_flight$carbon/500, rows =5 , keep = TRUE, xlab = NULL, title = '', size = 1, flip = FALSE, reverse = FALSE, equal = TRUE, pad = 0, use_glyph = FALSE, glyph_size = 20, legend_pos = "right")
waffle2

Waffle + waffle2


waffleplot <- waffle(other, rows = 5)
waffleplot

library(emojifont)  
library(dplyr)
library(ggwaffle)

waffle_data <- waffle_iron(other, aes_d(group = source))

w1 <- ggplot(waffle_data, aes(carbon, carbon, fill = group)) + 
  geom_waffle() + coord_equal()
w1



w2 <- ggplot(waffle_data, aes(x, y, fill = group)) + 
  geom_waffle()

w1 + w2
