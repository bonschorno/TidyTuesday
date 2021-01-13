rm(list = ls())

library(tidytuesdayR)
library(tidyverse)
library(ggrepel)
library(hrbrthemes)
library(patchwork)
library(wesanderson)
library(magrittr)
library(ggalt)
library(ggpubr)

load("coffee.RData")

coffee$country_of_origin <- recode(coffee$country_of_origin, "Tanzania, United Republic Of" = "Tanzania")
coffee$country_of_origin <- recode(coffee$country_of_origin, "United States (Hawaii)" = "USA")
coffee$country_of_origin <- recode(coffee$country_of_origin, "United States (Puerto Rico)" = "USA")
coffee$country_of_origin <- recode(coffee$country_of_origin, "United States" = "USA")
coffee$country_of_origin <- recode(coffee$country_of_origin, "Cote d?Ivoire" = "Cote d'Ivoire")

southamerica <- c("Guatemala", "Brazil", "Peru", "Costa Rica", "Mexico", "Honduras",
                  "Nicaragua", "Colombia", "Panama", "El Salvador", "Ecuador")

africa <- c("Ethiopia", "Uganda", "Tanzania", "Kenya", "Burundi", "Rwanda", "Malawi", "Zambia", "Cote d'Ivoire")


coffee %<>% mutate(continent = case_when(country_of_origin %in% southamerica ~ "South America",
                                         country_of_origin %in% africa ~ "Africa", 
                                         TRUE ~ "Asia"))


bubbleplot <- coffee %>% 
  group_by(country_of_origin, continent) %>% 
  summarise(freq = n(),
            rating = mean(total_cup_points, na.rm = T),
            altitude = mean(altitude_mean_meters, na.rm = T)) %>% 
  drop_na() %>% 
  filter(altitude < 2000 & altitude > 500) %>% 
  ungroup() %>% 
  mutate(rank = dense_rank(desc(freq)))

(plot_bubbleplot <- ggplot(data = bubbleplot, aes(x = rating, y = altitude, size = freq, color = continent)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(data = subset(bubbleplot, rank < 8), 
                  aes(label = country_of_origin), 
                  direction     = "both",
                  show.legend = F, 
                  min.segment.length = 0) +
  labs(x = "\nOverall rating",
       y = "Average altitude (in m)\n", 
       size = "Number of bags", 
       color = "", 
       title = "Origins of tasted coffees") +
  scale_color_manual(values = wes_palette("Royal1")) +
    scale_size(guide = "none") +
  theme_ipsum_tw() +
  theme(axis.text = element_text(color = "white"),
        text = element_text(color = "white"),
        legend.position = "top")
)
  
dumbbell <- coffee %>% 
  group_by(species) %>% 
  summarise(across(aroma:cupper_points, ~ mean(.x, na.rm = TRUE))) %>% 
  pivot_longer(-species, names_to = "characteristics", values_to = "values") %>% 
  pivot_wider(names_from = "species", values_from = "values") %>% 
  mutate(diff = Robusta-Arabica, 
         absdiff = abs(diff)) %>% 
  filter(absdiff > 0.1)

Royal1 = c("#899DA4", "#C93312", "#FAEFD1", "#DC863B")

(plot_dumbbell <- ggplot() +
  geom_segment(data=dumbbell, aes(y=characteristics, yend=characteristics, x=7, xend=10), color="white", size=0.1, linetype = "dotted", alpha = 0.8) +
  geom_dumbbell(data=dumbbell, aes(y=characteristics, x=Arabica, xend=Robusta, alpha = 0.8),
                size=0.5, color="white", size_x=3, size_xend = 3, colour_x = "#C93312", colour_xend = "#FAEFD1") +
  geom_text(data=filter(dumbbell, characteristics=="sweetness"),
            aes(x=Arabica, y=characteristics, label="Arabica"),
            color="#C93312", size=3, vjust=-1.5, fontface="bold", family="Roboto") +
  geom_text(data=filter(dumbbell, characteristics=="sweetness"),
            aes(x=Robusta, y=characteristics, label="Robusta"),
            color="#FAEFD1", size=3, vjust=-1.5, fontface="bold", family="Roboto") +
  geom_text(data=filter(dumbbell, characteristics == "sweetness"), aes(x=Arabica, y=characteristics, label=round(Arabica, digits = 2)),
            color="#C93312", size=2.75, vjust=2.5, family="Roboto") +
  geom_text(data=filter(dumbbell, characteristics == "sweetness"), aes(x=Robusta, y=characteristics, label=round(Robusta, digits = 2)),
            color="#FAEFD1", size=2.75, vjust=2.5, family="Roboto") +
  scale_y_discrete(breaks = c("sweetness", "flavor", "cupper_points", "aroma", "aftertaste", "acidity"), labels = c("Sweetness", "Flavor", "Cupper Points", "Aroma", 
                                                                                                                   "Aftertaste", "Acidity")) +
  labs(x = "\nGrade",
       y = "", 
       title = "Robusta vs Arabica") +
  theme_ipsum_tw() +
  theme(axis.text = element_text(color = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_blank(),
        text = element_text(color = "white"),
        legend.position = "none")
)

moisture <- coffee %>% 
  select(moisture, continent) %>% 
  filter(moisture < 0.28)

(plot_moisture <- ggplot(data = moisture, aes(x = moisture, fill = continent)) +
  geom_histogram(bins = 80, alpha = 0.8) +
  scale_fill_manual(values = wes_palette("Royal1")) +
  scale_x_continuous(breaks = c(0,0.1,0.15, 0.2), labels = c("0%", "10%", "15%", "20%")) +
  labs(x = "Moisture",
       y = "Count\n", 
       fill = "",
       title = "Moisture content") +
  theme_ipsum_tw() +
  theme(legend.position = "bottom",
        axis.text = element_text(color = "white"),
        text = element_text(color = "white"))
)

text_bubble <- paste(
  "The larger the bubbles are, the more coffees have been tested by",
  "the Coffee Quality Institute. The top 7 countries of origin are",
  "labelled with names. However, the frequencies do not correspond",
  "to actual export volumes. The three largest coffee exporters in",
  "2019 were Brazil (~ 2.5 million tonnes), Vietnam", 
  "(~ 1.6 million tonnes) and Colombia (~ 800k tonnes).",
  "",
  "Source: worldatlas.com", sep = "\n")

text_bubble_plot <- text_grob(text_bubble, color = "white", family = "Titillium Web Light", size = 14, hjust = 0.5, lineheight = 1.7)

text_dumbbell <- paste(
  "Despite the very similar ratings, there are important differences",
  "between the two most common coffee species. The most obvious",
  "of these is the sweet taste of Arabica beans as the plot clearly",
  'shows. "[Arabica beans] often also have hints of fruits or berries.',
  'Robusta, on the other hand, has a stronger, harsher and more',
  'bitter taste, with grainy or rubbery overtones."',
  "",
  "Source: perkcoffee.co", sep = "\n"
)

text_dumbbell_plot <- text_grob(text_dumbbell, color = "white", family = "Titillium Web Light", size = 14, lineheight = 1.7)

text_moisture <- paste(
  '"There is no official standard for ideal moisture content in green',
  "coffee, although the ICO recommends 11% as a good target. However",
  "it’s commonly accepted that 10-12% is a reasonable range. Anything",
  "less than 10% is likely to result in loss of cup quality, while",
  "humidity at higher levels begins to create a risk of mold growth.'",
  "It thus seems that the many zero correspond to NA rather than",
  "actually zero.",
  "",
  "Source: perfectdailygrind.com", sep = "\n"
)

text_moisture_plot <- text_grob(text_moisture, color = "white", family = "Titillium Web Light", size = 14, lineheight = 1.7)


plot_bubbleplot + text_bubble_plot +
  text_dumbbell_plot + plot_dumbbell +
  plot_moisture + text_moisture_plot +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Coffee | Kafo | Buna | Kopi", 
                  caption = "Source: Coffee Quality Database by James LeDoux | Graphic: @bonschorno",
                  theme = theme(plot.title = element_text(size = 40, family = "Titillium Web Bold", face = "bold", hjust = 0.5, color = "white"),
                                plot.caption = element_text(color = "white"))) & theme(
    plot.background = element_rect(colour = "#00353f", fill = "#00353f"))

ggsave("coffee.png", width = 297, height = 420, units = "mm", dpi = 400)  

