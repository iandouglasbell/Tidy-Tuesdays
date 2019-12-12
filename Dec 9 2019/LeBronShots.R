#Replicate NBA Shot Density Chart
#Tidy Tuesday December 10, 2019
#Ian Bell twitter @Ian_Bellio

library(readr)
library(tidyverse)
library(ggthemes)
library(BasketballAnalyzeR)

#Pull in raw shot data. Source: downloaded from NBAsavant.com based on ESPN Shot Tracker and stats.nba.com.

LeBron <- read_csv("LeBron.csv")

#All shots the LeBron Made, reproject coordinates to match BasketballAnalyzeR court coordinates
LeBronMade <- LeBron %>%
  mutate(x_new = x * 0.1, y_new = y * 0.1 - 42.25) %>%
  filter(y_new <= 0) %>%
  filter(shot_made_flag == 1)

#All shots the LeBron Missed, reproject coordinates to match BasketballAnalyzeR court coordinates
LeBronMisses <- LeBron %>%
  mutate(x_new = x * 0.1, y_new = y * 0.1 - 42.25) %>%
  filter(y_new <= 0) %>%
  filter(shot_made_flag == 0)

#Hexplot of made shots
MadePlot <- ggplot(LeBronMade, aes(x= x_new,y=y_new)) + geom_hex(binwidth = c(1.5,1.5)) + ylim(-47,0)
MadePlot

#Extract made hex plot values 
MadeHexValues <- layer_data(MadePlot, 1)

#Hexplot of missed shots 
MissedPlot <- ggplot() + geom_hex(data = LeBronMisses, aes(x= x_new,y=y_new, color="gray"), binwidth = c(1.5,1.5)) + ylim(-47,0)
MissedPlot

#Extract missed hex plot values 
MissedHexValues <- layer_data(MissedPlot,1)

#Combine extracted hex plot values to creat hex of shooting percent, binned percentages, and removed NAs (showing as factor?)
ShootingPercentHex <- left_join(MadeHexValues, MissedHexValues, by = c("x" = "x", "y" = "y")) %>%
  mutate(TotalShots = count.y + count.x) %>%
  mutate(ShootingPercent = count.x/TotalShots) %>%
  mutate(PercentBins = cut(ShootingPercent,breaks=c(-Inf, 0.2, 0.3, 0.4, 0.5, Inf), labels=c("<20%", "20-30%", "30-40%", "40-50%", ">50%"))) %>%
  filter(!is.na(PercentBins))
  
#Plot shooting percent using FiveThirtyEight theme
ShootingPercentPlot <-ggplot(data = ShootingPercentHex, aes(x,y)) + geom_hex(stat = "identity", na.rm = TRUE, aes(fill = PercentBins)) + ylim(-47,0) + theme_fivethirtyeight(base_size = 16, base_family = "sans") + labs(title = "LeBron James Sweet Spots", subtitle = 'All of his shots, 2010-17 regular seasons and playoffs' ,caption = 'Data from NBA.com and ESPN Shot Tracker \nCourt geom from BasketballAnalyzeR package \n#TidyTuesday • @Ian_Bellio') +  theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(), panel.grid.major = element_blank())  +  guides(alpha = FALSE, size = FALSE) + scale_fill_manual(values = c("#3269af","#47abca","#e5dc9c","#f47622","#e62d2f"))
ShootingPercentPlot

#Overlay BasketballAnalyzeR pre-made court
drawNBAcourt(ShootingPercentPlot, size= 0.8, col = "#666667")

#Save as image