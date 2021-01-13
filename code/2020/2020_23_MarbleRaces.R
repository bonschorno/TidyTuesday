#Marbles: Week 23

rm(list = ls())

library(tidyverse)
library(ggbump)
library(lubridate)
library(ggthemes)
library(wesanderson)
library(ggtext)

marbles <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

drop_cols <- c("source", "number_laps", "avg_time_lap", "host", "notes")

finaltop5 <- c("Savage Speeders", "O'rangers", "Hazers", "Snowballs", "Team Galactic")

marbles$pole <- substring(marbles$pole, 2)
marbles$date <- dmy(marbles$date)

marbles <- marbles %>% 
  select(-one_of(drop_cols)) %>% 
  drop_na(pole) %>% 
  mutate(pole = as.numeric(pole)) %>% 
  filter(team_name %in% finaltop5) %>% 
  mutate(rank = ifelse(team_name == "Savage Speeders",1,0))


ggplot(data = marbles, aes(x = date, y = pole, color = team_name)) +
  geom_segment(aes(x=as.Date("2020-02-15"),xend=as.Date("2020-04-04"),y=10,yend=10), color = "grey", linetype = "dotted", size = 0.1, alpha = 0.1) +
  geom_segment(aes(x=as.Date("2020-02-15"),xend=as.Date("2020-04-04"),y=5,yend=5), color = "grey", linetype = "dotted", size = 0.1, alpha = 0.1) +
  geom_segment(aes(x=as.Date("2020-02-15"),xend=as.Date("2020-04-04"),y=1,yend=1), color = "grey", linetype = "dotted", size = 0.1, alpha = 0.1) +
  geom_bump(size = 1.5, aes(alpha = rank)) +
  geom_point() +
  geom_text(data = marbles %>% filter(date == max(date)),
            aes(x = date + 1, label = team_name, alpha = rank), size = 5, hjust = 0) +
  scale_y_continuous(breaks = c(1,5,10), labels = c(1,5,10), trans = "reverse") +
  labs(title = "<b>Top Five of Marbula One Season 1</b>",
       subtitle = "<br>Consistent with their win of the season, the <span style = 'color:#F2AD00;'>Savage Speeders</span> finished the final race of the season in first place. The <span style = 'color:#FF0000;'>Hazers</span>, on the other hand, despite being the runners up in the overall standings, had to settle for second last place in the final.",
       caption = "Data: Jelle's Marble Runs | Graphic: @bonschorno") +
  ylab("") +
  scale_x_date(date_breaks = "2 week", date_labels = "%d %B",
               limit=c(as.Date("2020-02-15"),as.Date("2020-04-15"))) +
  scale_color_manual(values = wes_palette(n = 5, name = "Darjeeling1")) +
  scale_alpha(range = c(0.3, 0.8)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black", fill = "black"),
        plot.caption = element_markdown(colour = "grey", size = 10, family = "Roboto"),
        panel.grid = element_blank(),
        axis.text = element_text(color = "white", family = "Roboto"),
        plot.title = element_markdown(color = "grey", family = "Roboto", size = 25, hjust = 0.5),
        plot.subtitle = element_textbox_simple(
          hjust = 0.5,
          size = 13,
          lineheight = 1,
          padding = margin(5.5, 5.5, 5.5, 5.5),
          margin = margin(0, 0, 5.5, 0),
          fill = "black",
          color = "grey",
          family = "Roboto"
        ))
