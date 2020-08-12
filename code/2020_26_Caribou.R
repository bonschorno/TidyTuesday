#caribous

rm(list = ls())

library(mapcan)
library(tidyverse)
library(ggdark)
library(lubridate)
library(patchwork)
library(wesanderson)

individuals <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

NA_car133 <- locations %>% 
  filter(animal_id == "NA_car133") %>% 
  mutate(rank = dense_rank(event_id),
         xend = lead(longitude), 
         yend = lead(latitude))


ggplot(data = NA_car133, aes(x = longitude, y = latitude, color = season)) +
  geom_segment(aes(xend = xend, yend = yend), alpha = 0.1) +
  geom_point(alpha = 0.15) +
  labs(title = "NA_car133's journey",
       x = "",
       y = "") +
  dark_theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Roboto"))

#and now for all movements --> doesn't work too well

movements <- locations %>% 
  mutate(rank = dense_rank(event_id),
         xend = lead(longitude), 
         yend = lead(latitude))


ggplot(data = movements, aes(x = longitude, y = latitude)) +
  geom_segment(aes(xend = xend, yend = yend), alpha = 0.1) +
  facet_wrap(season ~ .) +
  geom_point(alpha = 0.15) +
  labs(title = "Travelling to the North country sid",
       x = "",
       y = "") +
  dark_theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Roboto"))


#what about tracing the journey?

journey <- NA_car133 %>% 
  mutate(time = ymd_hms(timestamp),
         week = week(time),
         year = year(time),
         distance = abs(longitude - xend))

years <- ggplot(data = journey, aes(x = longitude, y = latitude, color = season)) +
  geom_segment(aes(xend = xend, yend = yend), alpha = 0.1) +
  geom_point(alpha = 0.15) +
  scale_color_manual(values = c("#9972af","#c8b35a")) +
  scale_y_continuous(breaks = c(54.5, 55)) +
  scale_x_continuous(breaks = c(-120, -120.5, -121)) +
  labs(title = "Following Becky's journey",
       x = "",
       y = "",
       color = "Season") +
  dark_theme_minimal() +
  theme(text = element_text(family = "Roboto"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

distance <- ggplot(data = journey, aes(x = time, y = distance, fill = season)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#9972af","#c8b35a")) +
  labs(x = "",
       y = "",
       fill = "Season") +
  dark_theme_minimal() +
  theme(legend.position = "none", 
        text = element_text(family = "Roboto"),
        panel.grid = element_blank(),
        axis.text.y = element_blank())

years + distance + plot_layout(ncol = 1) & theme(plot.background = element_rect(colour = "black"))

#too much if I put both together. Let's stick to the first one

ann_text1 <- data.frame(longitude = -120.9, latitude = 54.9,
                       season = "Summer")
ann_text2 <- data.frame(longitude = -120.5, latitude = 54.3,
                        season = "Summer")

ggplot(data = journey, aes(x = longitude, y = latitude)) +
  geom_segment(aes(xend = xend, yend = yend), alpha = 0.1) +
  facet_wrap(season ~ .) +
  geom_point(alpha = 0.2) +
  scale_y_continuous(breaks = c(54.5, 55), labels = c("54° 30' N", "55° N")) +
  scale_x_continuous(breaks = c(-120, -120.5, -121), labels = c("120° S", "120° 30' S", "121° S")) +
  labs(title = "NA_car133's journey",
       subtitle = "The graph shows the migration pattern of a younger, female caribou over a period from 2010 to 2013.\nAn evident migration pattern emerges: While the herds cover large distances in summer, they remain\nin place in winter. It is also interesting to see how the summer destinations change. In 2011, for example,\nthe herd moved much higher to the north.\n",
       x = "",
       y = "",
       color = "Season") +
  geom_text(data = ann_text1,label = "Caribou migrate farther than\nany other known land animal...", size = 3.5) +
  geom_text(data = ann_text2,label = "...with two different herds\nin Alaska and Canada traveling\nup to 1,350 kilometers per year.\nSource:sciencenews.org", size = 3.5) +
  dark_theme_minimal() +
  theme(text = element_text(family = "Arial"),
        legend.position = "none",
        plot.title = element_text(size = 15, face = "bold")) 

#without annotations 

ggplot(data = journey, aes(x = longitude, y = latitude, color = as.factor(year))) +
  geom_segment(aes(xend = xend, yend = yend), alpha = 0.1) +
  facet_wrap(season ~ .) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values=wes_palette(name="Royal1")) +
  scale_y_continuous(breaks = c(54.5, 55), labels = c("54° 30' N", "55° N")) +
  scale_x_continuous(breaks = c(-120, -120.5, -121), labels = c("120° S", "120° 30' S", "121° S")) +
  labs(title = "Tracking Carlee (NA_car133)",
       subtitle = "The plot shows the migration pattern of Carlee, a young, female caribou, over the period of four years.\nA clear migration pattern emerges: While the herd covers large distances in summer, it remains\nmostly in place in winter. It is also interesting to see how the summer destinations change.\nFor example, the herd moved much higher to the north in 2011 than in any other year.\n",
       x = "",
       y = "",
       color = "Year",
       caption = "\nData: Movebank Data Repository | Graphic: @bonschorno") +
  dark_theme_minimal() +
  theme(text = element_text(family = "Roboto Light"),
        plot.subtitle = element_text(family = "Roboto Light"),
        legend.position = "bottom",
        plot.title = element_text(size = 20, face = "bold", family = "Roboto Bold")) 

ggsave("Caribou.png", width = 9, height = 6, dpi = 400)

#skyline plot

ggplot(data = journey, aes(x = time, y = distance, fill = season)) +
  geom_bar(stat = "identity") +
  scale_fill_grey() +
  labs(title = "Distance travelled",
       x = "",
       y = "",
       fill = "Season") +
  dark_theme_minimal() +
  theme(legend.position = "none", 
        text = element_text(family = "Chivo"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = -10, size = 25))

