rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(ggradar)
library(janitor)
library(lubridate)
library(wesanderson)
library(ggtext)

animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')

# first try ----------------------------------------------------------

animals <- c("Dogs", "Cats", "Horses")

catdogs <- c("Dogs", "Cats")

ninenine <- animal_outcomes %>% 
  select(-Total) %>% 
  filter(year == 1999,
         animal_type %in% animals) %>% 
  pivot_longer(-c(year:outcome), names_to = "state", values_to = "Values") %>% 
  arrange(year, state) %>% 
  group_by(state, animal_type) %>% 
  mutate(freq = round(Values/sum(Values), digits = 2)) %>% 
  select(-Values, -year) %>% 
  ungroup() %>% 
  pivot_wider(everything(), names_from = "outcome", values_from = "freq")

nsw99 <- ninenine %>% 
  filter(state == "NSW") %>% 
  select(-state)

ggradar(nsw99)


# second try ---------------------------------------------------------

test <- animal_outcomes %>% 
  select(-Total) %>% 
  filter(animal_type %in% catdogs) %>% 
  pivot_longer(-c(year:outcome), names_to = "state", values_to = "values") %>% 
  arrange(year, state)

ggplot(data = test, aes(x = year, y = values, group = state, color = state)) +
  geom_point() +
  geom_line() +
  facet_grid(outcome ~ animal_type)

# third try ---------------------------------------------------------

unique(animal_complaints$`Electoral Division`)

test2 <- animal_complaints %>% 
  clean_names() %>% 
  filter(animal_type == "dog",
         complaint_type == "Attack"|complaint_type == "Noise"|complaint_type == "Wandering") %>% 
  mutate(year = str_sub(date_received, start = -4)) %>% 
  group_by(year, suburb, complaint_type) %>% 
  tally() %>% 
  filter(suburb != "Unallocated")

top25 <- test2 %>% 
  group_by(suburb) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  slice(1:25) %>% 
  pull(suburb)

data <- test2 %>% 
  filter(suburb %in% top25)

data2020 <- data %>% 
  filter(year == 2020)

ggplot(data = data, aes(x = year, y = n, group = complaint_type, color = complaint_type)) +
  geom_point(size = 0.8, shape = 1) +
  geom_line(alpha = 0.5) +
  scale_x_discrete(breaks = c(2013, 2015, 2017, 2019), labels = c(2013, 2015, 2017, 2019)) +
  scale_color_manual(values = c("#0B775E", "#35274A" ,"#F2300F")) +
  labs(x = "", 
       y = "",
       title = "Doggos in Down Under",
       subtitle = "<br>The chart illustrates the 25 Australian suburbs that report the most complaints about dogs.<br><span style='color:#35274A'>**Noise**</span> is by far the most common cause. Unfortunately, no further information could be<br>found about the reasons why so many more complaints have been filed in Kelso (NSW)<br>and Kirwan (Queensland) due to <span style='color:#0B775E'>**attacks**</span> and <span style='color:#F2300F'>**wandering**</span> of dogs.<br>",
       color = "",
       caption = "\nSource: RSPCA | Graphic: @bonschorno") +
  facet_wrap(. ~ suburb, ncol = 5) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        text = element_text(family = "Titillium Web"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.x = element_text(angle = 45, size = 5),
        legend.position = "none",
        plot.background = element_rect(fill = "white"),
        plot.subtitle = element_markdown(),
        plot.margin = unit(c(0.5,1,0.5,1), "cm")) 

ggsave(filename = "doggos.png", height = 20, width = 20, units = "cm", dpi = 500)
