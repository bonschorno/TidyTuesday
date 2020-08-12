rm(list = ls())

library(tidyverse)

firsts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

#firsts

set.seed(2020)

eighteen <- firsts %>% 
  filter(year < 1800) %>% 
  sample_n(1)

nineteen <- firsts %>% 
  filter(year >= 1800 & year < 1900) %>% 
  sample_n(1)

twenty <- firsts %>% 
  filter(year >= 1900 & year < 2000) %>% 
  sample_n(1)

twentyone <- firsts %>% 
  filter(year >= 2000 & year < 2100) %>% 
  sample_n(1)

accomplishment_vct <- c("First African Episcopal Church established",
                    "First African-American woman to earn a B.A.",
                    "First African-American four-star general",
                    "First African-American to be inducted into the NASCAR Hall of Fame")


achievements <- firsts %>% 
  group_by(year) %>% 
  add_tally() %>% 
  mutate(accomplishment_dmy = ifelse(accomplishment %in% accomplishment_vct, 1,0))

arrows <- tibble(x1 = c(1960, 1940, 1846, 1755),
                 x2 = c(1969, 2009, 1863, 1792),
                 y1 = c(12.7, 7.2, 5, 4), 
                 y2 = c(12.1, 6.3, 2.13, 1.2))

ggplot(data = achievements, aes(x = year, y = n, color = category, size = ifelse(accomplishment_dmy == 1,5,1))) +
  geom_jitter(alpha = 0.5) +
  coord_flip() +
  labs(color = "",
       x = "",
       y = "",
       title = "A timeline of achievements",
       subtitle = "Four out of 479 African-Americans breaking the color barrier across eight disciplines and four centuries",
       caption = "Data: Wikipedia | Graphic: @bonschorno") +
  ylim(0,15) +
  theme_minimal() +
  scale_colour_grey() +
  annotate("text", x = 1935, y = 7, size = 3.5, color = "black", lineheight = 1,
           label = "Wendell Scott,", fontface = "bold") +
  annotate("text", x = 1920, y = 7, size = 3, color = "black", lineheight = 1,
    label = "the first African-American\nto be inducted into the\nNASCAR Hall of Fame.") +
  annotate("text", x = 1955, y = 13, size = 3.5, color = "black", lineheight = 1,
           label = "Daniel James Jr.,", fontface = "bold") +
  annotate("text", x = 1944, y = 13, size = 3, color = "black", lineheight = 1,
           label = "the first African-American\nfour-star general.") +
  annotate("text", x = 1840, y = 5, size = 3.5, color = "black", lineheight = 1,
           label = "Mary Jane Patterson,", fontface = "bold") +
  annotate("text", x = 1830, y = 5, size = 3, color = "black", lineheight = 1,
           label = "the first African-American\nwoman to earn a B.A.") +
  annotate("text", x = 1750, y = 4, size = 3.5, color = "black", lineheight = 1,
           label = "Absalom Jones,", fontface = "bold") +
  annotate("text", x = 1740, y = 4, size = 3, color = "black", lineheight = 1,
           label = "founded the first\nAfrican Episcopal Church.") +
  geom_curve(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = 0.3) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 13, family = "Arial"),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.margin=unit(c(1,1,1,1),"cm"),
        plot.title = element_text(size = 20, face = "bold"),
        plot.caption = element_text(family = "Arial", color = "grey"))
