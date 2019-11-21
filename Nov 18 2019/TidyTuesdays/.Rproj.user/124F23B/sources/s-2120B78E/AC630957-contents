# Tidy Tuesday 
# New Zealand Bird of the Year
#Ian Bell

#Goal: Find the New Zealand Bird of the Year and Visualize it 

library(tidyverse)
library(lubridate)

nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

#Make a new column of rank numeric

vote_rank <- "[[:digit:]]+"

nz_bird$rank <- str_extract(nz_bird$vote_rank, vote_rank)
nz_bird$rank <- as.numeric(nz_bird$rank)


#Find total number of first preference bird votes
#86 total species

#Create list of Birds and number of votes per rank

FPBV <- nz_bird%>%
  count(bird_breed, rank) %>%
  arrange(rank, desc(n))

#Count Number of total first place votes

First <- nz_bird %>%
  filter(rank == 1) %>%
  count() 

#43,460 total first place votes cast, Yellow-eyed Penguin was the top number of first-place votes with 5,757. Not an absolute majority, so we press on with the . 

#Assign a unique voter id (voter_id) for every voter (assuming everyone only voted once)
nz_bird <- nz_bird %>%
  group_by(rank) %>%
  mutate(voter_id = 1:n())

#Test code to figure out lowest vote-getting bird by round
test <- nz_bird %>%
  group_by(voter_id) %>%
  slice(which.min(rank)) %>%
  ungroup() %>%
  count(bird_breed) %>%
  arrange(desc(n)) %>%
  summarize(min = last(bird_breed))


#Function to determine bird getting lowest primary votes per round. Works, still need to figure out how to loop the function and record the results. 

vote_fun <- function(x, y, z, d){
  x <- group_by(.data = x, .dots = lazyeval::lazy(voter_id))
  d <- group_by(.data = x, .dots = lazyeval::lazy(voter_id)) %>%
    slice(which.min(rank)) %>%
    ungroup() 
  y <- d %>%
    count(bird_breed) %>%
    arrange(desc(n)) %>%
    summarize(last = last(bird_breed))
  z <- x %>%
    filter(bird_breed != y$last)
  e <- z %>%
    group_by(.dots = lazyeval::lazy(voter_id)) %>%
    slice(which.min(rank)) %>%
    ungroup() %>%
    count(bird_breed) %>%
    arrange(desc(n)) %>%
    mutate(birds$i)
  return(e)
}


#Plot of votes over time period, by unique voter

vote_time <- nz_bird %>%
  mutate(hour = str_pad(hour, width=2, side="left", pad="0")) %>%
  mutate(hour = str_pad(hour, width=4, side="right", pad="0")) %>%
  mutate(hour = as.POSIXct(hour, format = "%H%M")) %>%
  mutate(hour = format(hour, "%H:%M:%S")) %>%
  mutate(datetime = as.POSIXct(paste(date, hour), format="%Y-%m-%d %H:%M:%S")) %>%
  filter(rank == 1) %>%
  mutate(dow = wday(datetime, label = TRUE))

Voters_time <- ggplot(vote_time, aes(x=datetime, y=voter_id, color = dow)) +geom_point(size = 2) + scale_x_datetime(breaks = "1 day", date_labels = "%b %d") + labs(title = "New Zealand Bird of the Year Voting by Date", y = "Cumulative Votes", x = "Date") +  theme_bw(base_size = 10) + theme(legend.title = element_blank()) + theme(panel.grid.major.x = element_line(colour="gray", size = (0.75))) 
Voters_time

ggsave("Voters_time.png", dpi=400, width = 8, height = 4)
