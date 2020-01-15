#Tidy Tuesdays 
#Password
#Ian Bell Jan 14, 2020 •

library(tidyverse)
library(cowplot)

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

animals <- passwords %>%
  filter(category == "animal") %>%
  arrange(-rank) %>%
  mutate(password = factor(password, levels=password)) 

top15 <- top_n(animals, -15, rank)

plot <- ggplot(top15, aes( y= rank, x=password,  fill=rank)) + geom_bar(stat="identity") + coord_flip() + scale_fill_viridis_c(option = "viridis") + labs(title = 'What is the Most Popular Animal to Use in a Password?',  subtitle = 'Top 15 Animal-Related Passwords',caption = 'Data from Information is Beautiful \n #TidyTuesday • @Ian_Bellio', x= "Password", y= "Rank") + theme(legend.position = "none")  + theme(plot.title = element_text(size=24, family = "Source Sans Pro Semibold")) + theme(text = element_text(size=18, family = "Source Sans Pro")) 
plot

ggdraw(plot)+
  draw_image("emoji/dragon.png", x = 0.18, y = 0.84, width = 0.06, height = 0.07)+
  draw_image("emoji/monkey.png", x = 0.24, y = 0.80, width = 0.06, height = 0.06) +
  draw_image("emoji/dog.png", x = 0.39, y = 0.75, width = 0.07, height = 0.07) +
  draw_image("emoji/eagle.png", x = 0.43, y = 0.70, width = 0.07, height = 0.07) +
  draw_image("emoji/fire.png", x = 0.45, y = 0.65, width = 0.06, height = 0.06) +
  draw_image("emoji/bird.png", x = 0.495, y = 0.65, width = 0.06, height = 0.06) +
  draw_image("emoji/tiger.png", x = 0.45, y = 0.585, width = 0.07, height = 0.07) +
  draw_image("emoji/tiger.png", x = 0.50, y = 0.585, width = 0.07, height = 0.07) +
  draw_image("emoji/chicken.png", x = 0.52, y = 0.535, width = 0.06, height = 0.06) +
  draw_image("emoji/bulldog.png", x = 0.555, y = 0.49, width = 0.07, height = 0.07) +
  draw_image("emoji/spider.png", x = 0.60, y = 0.44, width = 0.06, height = 0.06) +
  draw_image("emoji/rabbit.png", x = 0.62, y = 0.39, width = 0.06, height = 0.06) +
  draw_image("emoji/tigerface.png", x = 0.65, y = 0.335, width = 0.05, height = 0.05) +
  draw_image("emoji/cat.png", x = 0.72, y = 0.28, width = 0.07, height = 0.07)   +
  draw_image("emoji/turtle.png", x = 0.77, y = 0.225, width = 0.06, height = 0.06)   +
  draw_image("emoji/crocodile.png", x = 0.85, y = 0.18, width = 0.06, height = 0.06)   +
  draw_image("emoji/leopard.png", x = 0.89, y = 0.12, width = 0.06, height = 0.06)                     
