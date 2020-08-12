rm(list = ls())

library(tidyverse)
library(hrbrthemes)
library(waffle)
library(wesanderson)
library(patchwork)
library(ggpubr)

astro <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

# sex ---------------------------------------------------------------------

women <- astro %>% 
  select(sex) %>% 
  group_by(sex) %>% 
  summarise(freq = n()) %>% 
  mutate(freq_rel = round(freq/sum(freq)*100, digits = 0),
         sex = ifelse(sex == "female", "Female", "Male"))

spacepalette <- c("#57BDA2", "#2493A2")

(plot_women <- ggplot(data = women, aes(fill = sex, values = freq_rel)) +
  geom_waffle(n_rows = 5, size = 1, colour = "#0A0E28") +
  coord_equal() +
  labs(title = "The gender gap",
       fill = "") +
  scale_fill_manual(values =spacepalette) +
  theme_ipsum_tw(grid="") +
  theme_enhance_waffle() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        text = element_text(color = "white", face = "bold"),
        legend.position = "bottom")
) 


# nations -----------------------------------------------------------------

nations <- astro %>% 
  select(nationality) %>% 
  mutate(nationality = case_when(nationality == "U.S." ~ "US",
                                 TRUE ~ "Other")) %>% 
  group_by(nationality) %>% 
  summarise(freq = n()) %>% 
  mutate(freq_rel = round(freq/sum(freq)*100, digits = 0))

(plot_nations <- ggplot(data = nations, aes(fill = nationality, values = freq_rel)) +
  geom_waffle(n_rows = 5, size = 1, colour = "#0A0E28") +
  coord_equal() +
  labs(title = "US predominance",
       fill = "") +
  scale_fill_manual(values = spacepalette) +
  theme_ipsum_tw(grid="") +
  theme_enhance_waffle() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        text = element_text(color = "white", face = "bold"),
        legend.position = "bottom")
)


# civilians ---------------------------------------------------------------

civilians <- astro %>% 
  select(military_civilian) %>% 
  mutate(military_civilian = ifelse(military_civilian == "civilian", "Civilian", "Military")) %>% 
  group_by(military_civilian) %>% 
  summarise(freq = n()) %>% 
  mutate(freq_rel = round(freq/sum(freq)*100, digits = 0))

(plot_civilians <- ggplot(data = civilians, aes(fill = military_civilian, values = freq_rel)) +
    geom_waffle(n_rows = 5, size = 1, colour = "#0A0E28") +
    coord_equal() +
    labs(title = "Civilian minority",
         fill = "") +
    scale_fill_manual(values = spacepalette) +
    theme_ipsum_tw(grid="") +
    theme_enhance_waffle() +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          text = element_text(color = "white", face = "bold"),
          legend.position = "bottom")
) 


# age ---------------------------------------------------------------------

age <- astro %>% 
  select(year_of_birth, year_of_mission) %>% 
  mutate(age = year_of_mission-year_of_birth,
         old = ifelse(age < 35, "Under 35", "Over 35")) %>% 
  group_by(old) %>% 
  summarise(freq = n()) %>% 
  mutate(freq_rel = round(freq/sum(freq)*100, digits = 0))

(plot_age <- ggplot(data = age, aes(fill = old, values = freq_rel)) +
    geom_waffle(n_rows = 5, size = 1, colour = "#0A0E28") +
    coord_equal() +
    labs(title = "Few youngsters",
         fill = "") +
    scale_fill_manual(values = spacepalette) +
    theme_ipsum_tw(grid="") +
    theme_enhance_waffle() +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          text = element_text(color = "white", face = "bold"),
          legend.position = "bottom")
) 


# eva ---------------------------------------------------------------------

eva <- astro %>% 
  group_by(mission_title) %>% 
  summarise(n = sum(field21)) %>% 
  mutate(activities = ifelse(n == 0, "None", "At least 1")) %>% 
  group_by(activities) %>% 
  summarise(freq = n()) %>% 
  mutate(freq_rel = round(freq/sum(freq)*100, digits = 0))

(plot_eva <- ggplot(data = eva, aes(fill = activities, values = freq_rel)) +
    geom_waffle(n_rows = 5, size = 1, colour = "#0A0E28") +
    coord_equal() +
    labs(title = "Frequent extravehicular activities",
         fill = "") +
    scale_fill_manual(values = spacepalette) +
    theme_ipsum_tw(grid="") +
    theme_enhance_waffle() +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          text = element_text(color = "white", face = "bold"),
          legend.position = "bottom")
) 


# text --------------------------------------------------------------------

text_astro <- paste(
  "Each square represents 1% of the relevant",
  "population. Of all missions listed, only 11%",
  "of the crew members were women. 5% of the",
  "crew members were under 35 years old when",
  "they were in space and two thirds of the ",
  "crew members were Americans.", sep = "\n"
)


text_astro_plot <- text_grob(text_astro, color = "white", family = "Titillium Web Light", size = 9, lineheight = 1.5)

#assembling

plot_women + 
  plot_nations + 
  plot_civilians +
  plot_age +
  plot_eva +
  text_astro_plot +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Astronauts in waffles",
                  caption = "\nSource: Astronaut Database | Graphic: @bonschorno",
                  theme = theme(plot.title = element_text(color = "white",
                                                          size = 25,
                                                          hjust = 0.5,
                                                          family = "Titillium Web",
                                                          face = "bold"),
                                plot.caption = element_text(colour = "white", size = 7, hjust = 1.1))) & 
  theme(plot.background = element_rect(colour = "#0A0E28", fill = "#0A0E28"),
                                plot.margin = margin(0.5, 1, 0.5, 1, "cm"))

ggsave(filename = "astronauts.png", height = 20, width = 20, units = "cm", dpi = 400)

