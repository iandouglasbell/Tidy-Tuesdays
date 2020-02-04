# NFL Data
# Tidy Tuesday Ian Bell 
# Feb 4 2020

#Libraries
library(tidyverse)
library(geofacet)
library(stringr)

#Pull in Data
attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

abbreviations <- readr::read_csv('https://gist.githubusercontent.com/cnizzardini/13d0a072adb35a0d5817/raw/4b555e084e8cec673dc587555008607fb06c6a60/nfl_teams.csv')

#Tidy

abbreviations <- abbreviations %>%
  mutate(team_name = word(Name, -1))# Pull in team three-letter abbreviations to mesh with geofacet grid, make each team unique (New York)

standings <- inner_join(standings, abbreviations, by = "team_name") %>%
  rename(code = Abbreviation) %>%
  mutate(code = replace(code, code == "STL" & team_name == "Rams" & team == "Los Angeles", "LAR")) %>%
  mutate(code = replace(code, code == "SD" & team_name == "Chargers" & team == "Los Angeles", "LAC")) #Join Standings data with Abbreviations data so geofacet can use abbreviations to facet out the data, modify Rams and Chargers abbreviations because of location change

#Custom Geofacet Grid
mygrid <- data.frame(
  name = c("Bills", "Lions", "Packers", "Vikings", "Patriots", "Seahawks", "Bears", "Bengals", "Browns", "Colts", "Giants", "Jets", "Broncos", "Chiefs", "Eagles", "Steelers", "49ers", "Rams", "Ravens", "Raiders", "Titans", "Redskins", "Falcons", "Panthers", "Cowboys", "Chargers", "Rams", "Saints", "Cardinals", "Texans", "Jaguars", "Chargers", "Buccaneers", "Dolphins"),
  code = c("BUF", "DET", "GB", "MIN", "NE", "SEA", "CHI", "CIN", "CLE", "IND", "NYG", "NYJ", "DEN", "KC", "PHI", "PIT", "SF", "STL", "BAL", "OAK", "TEN", "WAS", "ATL", "CAR", "DAL", "LAC", "LAR", "NO", "ARI", "HOU", "JAX", "SD", "TB", "MIA"),
  row = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7),
  col = c(8, 5, 4, 3, 9, 1, 4, 7, 6, 5, 9, 8, 2, 3, 8, 7, 1, 4, 7, 1, 5, 8, 6, 7, 3, 1, 2, 5, 2, 3, 8, 1, 7, 8),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid)

TeamWins <- ggplot(data = standings) +
  geom_col(mapping = aes(year, wins, fill = wins), width = 1) +
  geom_line(mapping = aes(year, wins)) +
  geom_point(filter(standings, sb_winner == "Won Superbowl"), mapping = aes(x=year, y= (wins + 5), color = "Superbowl \nChamps"), size=2.5, fill = "blue", shape = 25) +  
  facet_geo(~code, grid = mygrid, label = "code") +
  labs(title = "NFL Wins 2000 - 2020",
       caption = 'Data by Pro Football Reference\n#TidyTuesday • @Ian_Bellio',
       subtitle = 'Season win totals for each team in the National Football League for the past 20 years',
       x="",
       y="") +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_bw() +
  theme(plot.title = element_text(size=32, family = "Helvetica", face = "bold", color = "#f38181")) + 
  theme(text = element_text(size=12, family = "Helvetica", face = "bold")) +
  theme(plot.background = element_rect(fill = "#95e1d3")) +
  theme(strip.background = element_rect(color="#fce38a", fill="#FDF7C0", size=0.5, linetype="solid")) +
  scale_x_continuous(labels = c("'00","","'10","","'20")) +
  theme(legend.title = element_blank(), legend.background = element_rect(fill = "#95e1d3"), legend.key = element_rect(fill = "#95e1d3", color = NA),legend.position = c(0.45, 0.1), legend.direction = "horizontal") +
  theme(panel.background = element_rect(fill = "#eaffd0", colour = "#6D9EC1",size = 1, linetype = "solid")) +
  guides(fill = FALSE) +
  theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm")) 

TeamWins

ggsave("NFL_Wins.png", dpi = 300, width = 11, height = 8, units = "in")
