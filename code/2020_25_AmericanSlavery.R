rm(list = ls())

library(tidyverse)
library(waffle)
library(hrbrthemes)
library(ggtext)
options(scipen = 999)

census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')

regions <- census %>% 
  filter(region != "USA Total") %>% 
  mutate(black_slaves = black-black_free,
         years = case_when(
           year <= 1800 ~ "1790-1800",
           year == 1810 | year == 1820 ~ "1810-1820",
           year == 1830| year == 1840  ~ "1830-1840",
           year == 1850| year == 1860  ~ "1850-1860",
           year == 1870 ~ "1870"
         )) %>% 
  arrange(region) %>% 
  group_by(years, region) %>% 
  summarise(white = mean(white),
            black_free = mean(black_free),
            black_slaves = mean(black_slaves),
            total = mean(total)) %>% 
  mutate(white_perc = round((white/total)*100, digits = 0),
         black_free_perc = round((black_free/total)*100, digits = 0),
         black_slaves_perc = round((black_slaves/total)*100, digits = 0)) %>% 
  select(years, contains("perc"), region) %>% 
  pivot_longer(-c(years, region), names_to = "vars", values_to = "vals") %>% 
  mutate(vars = factor(vars)) %>% 
  filter(region != "West") %>% 
  mutate(vals = ifelse(years == "1790-1800" & region == "Northeast" & vars == "black_free_perc", 1, vals))

cols <- c("white_perc" = "#ededed", "black_free_perc" = "black", "black_slaves_perc" = "#707070")
new_labels <- c("1790-1800" = "1790-1800\n", "1810-1820" = "", "1830-1840" = "1830-1840\n", "1850-1860" = "", "1870" = "1870\n")

ggplot(regions, aes(fill = vars, values = vals)) +
  geom_waffle(color = "white", size = 0.7, n_rows = 10, flip = TRUE) +
  facet_grid(region~years, labeller = labeller(years = new_labels), switch = "y") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_manual(values = cols)+
  coord_equal() +
  labs(
    title = "American Slavery",
    subtitle = "Each square marks about 1% of the population in the respective census region.<br><b><span style ='color:#707070;'>Grey</span></b> represents the free black population, <b>black</b> the population in slavery. Specifically,<br>from 1790 to 1800, one third of the total population in the South were enslaved black people.<br><br>",
    x = "text",
    y = "text",
    caption = "\nData: US Census Archives | Graphic: @bonschorno"
  ) +
  theme_void(base_family = "Arial") +
  theme(panel.grid = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.ticks = element_blank(),
        legend.position = "none",
        strip.text.y.left = element_text(angle = 0),
        strip.text.y = element_text(size = 9),
        strip.text.x = element_text(size = 9),
        plot.subtitle = element_markdown(size = 11),
        plot.title = element_text(size = 20, face = "bold")) +
  guides(fill = guide_legend(reverse = TRUE))

