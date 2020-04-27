#TidyTuesday Broadway Shows
#March 28 2020   Ian Bell

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

###Data
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

###Wrangle

#Summarise CPI by year, grosses by year
cpi_year <- cpi %>%
  mutate(year_month = as.Date(year_month)) %>%
  mutate(year = year(year_month)) %>%
  group_by(year) %>%
  summarise(mean_cpi = mean(cpi)) %>%
  mutate(conversion_fact = 266.7803/mean_cpi)

grosses_year <- grosses %>%
  mutate(week_ending = as.Date(week_ending)) %>%
  mutate(year = year(week_ending)) %>%
  group_by(year) %>%
  summarise(total_gross = sum(weekly_gross), total_seats = sum(seats_sold)) %>%
  inner_join(cpi_year, by = "year") %>%
  mutate(adjusted = total_gross*conversion_fact)

#Find average tacket price for each show by year for histogram
ticket_price <- grosses %>%
  mutate(week_ending = as.Date(week_ending)) %>%
  mutate(year = year(week_ending)) %>%
  inner_join(cpi_year, by = "year") %>%
  mutate(adjusted_avg = avg_ticket_price*conversion_fact) %>%
  group_by(year, show) %>%
  summarise(price = mean(adjusted))

#Number of Plays on Broadway each year
number_plays <- grosses %>%
  mutate(week_ending = as.Date(week_ending)) %>%
  mutate(year = year(week_ending)) %>%
  group_by(year) %>%
  summarise(number = n_distinct(show))

###Plot

#chart of grosses by year (all adjusted to 2020 price)

gross_plot <- ggplot(grosses_year, aes(x=year, y=adjusted)) +
  geom_col(fill="#390099") + 
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9)) + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFBD00", colour = "#FFBD00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed', colour = "black")) +
  labs(x= "", y = "Annual Gross\n(Adjusted to 2020 dollars)", title = "...and annual total gross revenue has\ntripled since 1985 to over 1.5B a year")
gross_plot

#chart of seats sold (attendance)

seats <- ggplot(grosses_year, aes(x=year, y=total_seats)) +
  geom_col(fill="#9E0059")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#FFBD00", colour = "#FFBD00"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed', colour = "black")) +
  labs(x= "", y = "Annual Broadway Attendance", title = "Attendance has more than doubled since 1985")+
  theme(plot.background = element_rect(fill = "#FFBD00", colour = "#FFBD00"))
seats

#charts of ticket prices
tckt_price <- ggplot(grosses_two, aes(x=year, y=price, group=year)) + 
  geom_boxplot(fill = "#FF5400") +
  scale_y_continuous(labels = dollar) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed', colour = "black")) +
  labs(x= "", y = "Average Seat Price per Play\n(Adjusted to 2020 dollars)", title = "Show average seat price has also risen,\nand now more ultra-expensive shows") +
  theme(plot.background = element_rect(fill = "#FFBD00", colour = "#FFBD00"))
tckt_price

#Chart of number of broadway plays
n_plays <- ggplot(number_plays, aes(x=year, y=number)) + 
  geom_point(size=3, color = "#FF0054") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.grid.major.y = element_line(size = 0.5, linetype = 'dashed', colour = "black")) +
  labs(x= "", y = "Number of Broadway Plays", title = "The number of Broadway plays has climbed") +
  theme(plot.background = element_rect(fill = "#FFBD00", colour = "#FFBD00"))
n_plays 

combined <- patchwork_plot <- (n_plays +tckt_price)/(seats + gross_plot)
combined + plot_annotation(
  title = 'The Broadway Play Explosion',
  subtitle = 'Broadway Trends Since 1985',
  caption = 'Prices adjusted using average annual Consumer Price Index\n Data: Playbill, Alex Cookson, U.S. Bureau of Labor Statistics | Analysis: @Ian_Bellio | #TidyTuesday', theme = theme(plot.title = element_text(size = 22), plot.subtitle = element_text(size = 18))
)

ggsave("Broadway.png", dpi = 300, width = 9.5, height = 8, units = "in")
