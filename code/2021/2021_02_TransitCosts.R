# week 2: Transit Cost

library(tidyverse)
library(ggtext)
library(ggbeeswarm)

# getting the data----

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

transit <- data %>% 
  filter(country == "CN", 
         start_year %in% seq(1997, 2024,1)) 

countries <- data %>% 
  group_by(country) %>% 
  count(sort = T) %>% 
  filter(n > 10,
         country != "CN") %>% 
  pull(country)

colors <- c("#0A122A", "#698F3F")

data %>% 
  filter(country %in% countries,
         length < 110) %>% 
  mutate(duration = as.numeric(end_year) - as.numeric(start_year)) %>% 
  group_by(country) %>% 
  mutate(max_length = max(length, na.rm = T)) %>% 
  add_count(country) %>% 
  ggplot(aes(x = fct_reorder(country, max_length), size = as.numeric(real_cost)/1000, 
             y = length, color = as.factor(rr), shape = as.factor(rr))) +
  geom_quasirandom(alpha = 0.7, dodge.width = 0.5) +
  #geom_point(alpha = 0.7, shape = 15) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c(19, 15)) +
  scale_y_continuous(breaks = seq(0,60,20), labels = c("Length of rail network:", "20 km", "40 km", "60 km")) +
  scale_x_discrete(labels = rev(c("Spain", "India", "Turkey", "Japan", "Taiwan", "France", "Italy", "USA", "Germany"))) +
  labs(title = "Transit Costs Project",
       subtitle = "The database created by the Transit Costs Project spans more than 50 countries and totals<br>more than 11,000 km of urban rail built since the late 1990s. Their goal is to figure out how to<br>deliver more high-capacity transit projects for a fraction of the cost in various countries.<br><br> Listed are those countries with more than ten projects in the database (except from China).<br>Also, the length of the networks was limited to less than 100km. Regional rails are shown in<br><b style='color:#698F3F;'>green</b>, all remaining urban rail projects in <b style='color:#0A122A;'>black</b>.",
       caption = "\nSource: Transit Costs Project | Graphic: @bonschorno",
    color = "",
       x = "",
       y = "",
       size = "Real cost (billion $)") +
  coord_flip() +
  guides(shape=FALSE,
         color = FALSE) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#E8E8E8", color = "#E8E8E8"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom",
        plot.title = element_text(family = "Patua One", size = 30),
        plot.subtitle = element_markdown(family = "Lora Regular", size = 10, lineheight = 1.2),
        plot.caption = element_text(family = "Lora Italic", size = 8, hjust = 0.5),
        text = element_text(family = "Lora SemiBold"),
        plot.margin = unit(c(0.5,0.5,0.5,0), "cm"))

ggsave("2021_02_TransitCosts.png", dpi = 500, height = 25, width = 20, units = "cm")
