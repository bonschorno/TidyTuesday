#Week 35: Crop yields

rm(list = ls())

library(tidyverse)
library(janitor)
library(hrbrthemes)

fertilizer <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')

data <- fertilizer %>% 
  clean_names() %>% 
  filter(year == 1961 | year == 2018) %>% 
  get_dupes(entity) %>% 
  drop_na(code) %>% 
  select(1,3,4, yield = cereal_yield_tonnes_per_hectare) %>% 
  mutate()

set.seed(141)
random <- data %>% 
  distinct(entity) %>% 
  pull() %>% 
  sample(size = 25)

data_plot <- data %>% 
  filter(entity %in% random)

change <- data_plot %>% 
  pivot_wider(names_from = "year", values_from = "yield") %>% 
  rename(year_2018 = "2018",
         year_1961 = "1961") %>% 
  mutate(change_per = (year_2018-year_1961)/year_1961*100,
         change_abs = round(year_2018-year_1961, digits = 1)) %>% 
  select(entity, change_per, change_abs)

data_plot2 <- inner_join(data_plot, change) 

ggplot(data = data_plot2, aes(x = year, y = yield)) +
  geom_segment(data = data_plot, aes(x = year, xend = year, y = 0, yend = yield), color = "grey", alpha = 0.1) +
  geom_segment(data = data_plot, aes(x = 1961, xend = 2018, y = 0, yend = 0), color = "grey", alpha = 0.1) +
  #geom_text(data = subset(data_plot2, year == 1961), color = "white", size = 2.5, vjust = -2, hjust = 0.3, family = "Nunito")  +
  geom_text(data = subset(data_plot2, year == 2018 & entity != "Norway"), aes(label = paste0("+", change_abs)), color = "white", size = 3, vjust = -1, hjust = 0.8, family = "Nunito") +
  geom_text(data = subset(data_plot2, year == 2018 & entity == "Norway"), aes(label = change_abs), color = "white", size = 3, vjust = -1, hjust = 0.8, family = "Nunito") + 
  ylim(0, 10) +
  geom_line(data = data_plot2, aes(color = change_per), size = 1.5) +
  geom_point(data = data_plot2, aes(color = change_per), size = 2) +
  scale_color_gradient(low = "#66B5F6", high = "#BFE299") +
  facet_wrap(~ entity, ncol = 5, strip.position = "bottom") +
  scale_x_continuous(breaks = c(1961, 2018), labels = c(1961, 2018)) +
  labs(title = "Cereal yield",
       subtitle = "These slope charts show the development of cereal yield in tonnes per hectare\nfrom 1961 to 2018 for 25 randomly selected countries. The number above the\npoint shows the absolute difference between then and now, while the color of\nthe slope reflects the percentage difference.\n",
       color = "Change (in %)",
       x = "",
       y = "",
       caption = "\nSource: Our World in Data | Graphic: @bonschorno") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Nunito", colour = "white"),
        strip.text = element_text(color = "white"),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "black"),
        legend.position = "bottom",
        plot.title = element_text(size = 50, face = "bold", family = "IBM Plex Sans", color = "white"),
        plot.subtitle = element_text(size = 13),
        legend.title = element_text(vjust = 0.8),
        plot.caption = element_text(hjust = 0.5),
        panel.spacing = unit(1, "cm"),
        plot.margin = unit(c(1,1,1,1), "cm"))

ggsave("cropyields.png", height = 30, width = 22, units = "cm", dpi = 500)
