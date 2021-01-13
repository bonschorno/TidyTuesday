#European Energy

rm(list = ls())

library(tidyverse)
library(maps)
library(patchwork)
options(scipen = 999)
library(ggforce)
library(ggtext)

energy_types <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')

#preparing data
energy_types <- energy_types %>% 
  mutate(country_name = case_when(country_name == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
                                  country_name == "North Macedonia" ~ "Macedonia", 
                                  TRUE ~ country_name)) %>% 
  drop_na(country_name)

#country vector
countries <- energy_types %>% 
  distinct(country_name) %>% 
  pull()

#how many energy types?
unique(energy_map$type)

#energy vector
energies <- c("Conventional thermal", "Nuclear", "Hydro")

#eu map
eu_map <- map_data("world", region = countries) %>% 
  filter(lat < 75)

#country centroids
centroids <- eu_map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))


#merging data
energy_map <- energy_types %>% 
  left_join(centroids, by = c("country_name" = "region")) %>% 
  rename(year2016 = "2016",
         year2017 = "2017",
         year2018 = "2018")

solar_italy <- "After Germany, Italy ranks second in Europe as a producer of solar power."
solar_sweden <- "In 2016, Sweden produced only 143 Gwh of solar energy."

(solar <- ggplot(subset(energy_map, type == "Solar")) +
  annotate("text", x = 31, y = 56.1, label = "'16", size = 3, family = "Titillium Web") +
  annotate("text", x = 32, y = 53.9, label = "'17", size = 3, family = "Titillium Web") +
  annotate("text", x = 35, y = 56.1, label = "'18", size = 3, family = "Titillium Web") +
  annotate("point", x = 32, y = 55, alpha = 0.2, size = 15, color = "grey") +
  annotate("point", x = 33, y = 55, alpha = 0.2, size = 15, color = "grey") +
  annotate("point", x = 34, y = 55, alpha = 0.8, size = 15, color = "grey") +
  annotate("segment", x = 25.5, xend = 32, y = 43, yend = 53.5, colour = "black", linetype = "dotted") +
  geom_point(aes(x = long, y = lat, size = year2016), color = "#FFC647", alpha = 0.2) +
  geom_point(aes(x = long+0.5, y = lat, size = year2017), color = "#FFC647", alpha = 0.4) +
  geom_point(aes(x = long+1, y = lat, size = year2018), color = "#FFC647", alpha = 0.8) +
  geom_mark_circle(aes(x = long+1, y = lat, filter = country_name == "Italy", label = "O sole mio", 
                         description = solar_italy), label.family = "Titillium Web", label.buffer = unit(1, 'mm'), con.type = "none") +
    geom_mark_circle(aes(x = long+1, y = lat, filter = country_name == "Sweden", label = "Ain't no sunshine", 
                         description = solar_sweden), label.family = "Titillium Web", label.buffer = unit(5, 'mm'), con.type = "none") +
  scale_size(range = c(0,20)) +
  labs(x = "",  
       y = "", 
       size = "Gigawatt hours",
       title = "<span style='color:#FFC647'>Solar</span>") +
  theme_minimal() +
    theme(legend.position = "left",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linetype = "dotted"),
          text = element_text(family = "Titillium Web"),
          axis.text = element_blank(),
          plot.title = element_markdown(size = 30, family = "Titillium Web", face = "bold", hjust = 0.5))
)

turkey_hydro <- "Turkey sources most of its energy from hydropower."
norway_hydro <- "#1 in terms of hydropower"
germany_hydro <- "A closer look reveals that from 2016-2018, hydroelectric energy has decreased by 20%."

(hydro <- ggplot(subset(energy_map, type == "Hydro")) +
    geom_point(aes(x = long, y = lat, size = year2016), color = "#005b96", alpha = 0.2) +
    geom_point(aes(x = long+0.5, y = lat, size = year2017), color = "#005b96", alpha = 0.4) +
    geom_point(aes(x = long+1, y = lat, size = year2018), color = "#005b96", alpha = 0.8) +
    annotate("point", x = 16.42866, y = 66.24992, size = 3, color = "black") +
    geom_mark_circle(aes(x = long+1, y = lat, filter = country_name == "Turkey", label = "Warm waters", 
                         description = turkey_hydro), label.family = "Titillium Web", label.buffer = unit(2, 'mm'), con.type = "none") +
    geom_mark_circle(aes(x = long+1, y = lat, filter = country_name == "Norway", label = "Norway", 
                         description = norway_hydro), label.family = "Titillium Web", label.buffer = unit(45, 'mm'), con.type = "none") +
    annotate("segment", x = 14, xend = 16.42866, y = 59.5, yend = 66.24992, color = "black") +
    geom_mark_circle(aes(x = long+1, y = lat, filter = country_name == "Germany", label = "Germany", 
                        description = germany_hydro), label.family = "Titillium Web", con.type = "none") +
  scale_size(range = c(0,20)) +
  guides(size = guide_legend(reverse = TRUE)) +
  labs(x = "",  
       y = "", 
       size = "Gigawatt hours", 
       title = "<span style='color:#005b96'>Hydro</span>") +
  theme_minimal() +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dotted"),
        text = element_text(family = "Titillium Web"),
        axis.text = element_blank(),
        plot.title = element_markdown(size = 30, family = "Titillium Web", face = "bold", hjust = 0.5))
)

solar + hydro + plot_layout(ncol = 2) +
  plot_annotation(title = "Mapping European Energy<br>",
                  theme = theme(plot.title = element_markdown(size = 30, face = "bold", hjust = 0.5),
                                text = element_text(family = "Titillium Web"),
                                plot.caption = element_text(hjust = 0)))

ggsave(filename = "energy.png", width = 40, height = 30, units = "cm", dpi = 500)
