# Tidy Tuesday (Week 4): Kenya Census

library(tidyverse) # datawrangling
library(rKenyaCensus)
library(sf)
library(rmapshaper)
options(scipen = 999)

# data wrangling ----------------------------------------------------------

data("DataCatalogue")

shapefiles <- KenyaCounties_SHP

shapes <- st_as_sf(shapefiles)

crops <- V4_T2.22 %>% 
  filter(AdminArea == "County")

phones <- phones %>% 
  filter(AdminArea == "County")

crops_sf <- shapes %>% 
  left_join(crops) %>% 
  left_join(phones)

kenya_sf_simple <- ms_simplify(crops_sf, keep_shapes = TRUE, keep = 0.01)

# plot --------------------------------------------------------------------

burg <- c("#fbe6c5","#70284a")
peach <- c("#fde0c5","#eb4a40")
purp <- c("#f3e0f7", "#63589f")
purpor <- c("#f9ddda", "#573b88")
magenta <- c("#f3cbd3", "#6c2167")
mint <- c("#e4f1e1","#0d585f")
teal <- c("#b0f2bc", "#257d98")

ggplot(kenya_sf_simple, aes(fill = MPO_Female_Perc, color = MPO_Female_Perc)) +
  geom_sf() +
  scale_fill_continuous(high = purpor[1], low = purpor[2], labels = c("20%", "", "40%", "", "60%", "")) +
  scale_color_continuous(high = purpor[1], low = purpor[2], labels = c("20%", "", "40%", "", "60%", "")) +
  labs(title = "Female mobile phone users",
       subtitle = "---------------------------",
       color = "",
       fill = "",
       caption = "\nSource: @Shel_Kariuki | Graphic: @bonschorno") +
  theme(text = element_text(family = "Arvo", color = "black"),
        axis.text = element_text(size = 7),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dotted", color = "darkgrey", size = 0.2),
        plot.title = element_text(size = 30, hjust = 0.5),
        plot.subtitle = element_text(vjust = 5.5, hjust = 0),
        plot.caption = element_text(vjust = 300, hjust = 0.99),
        plot.background = element_rect(fill = "#e5e5e3", color = "#e5e5e3"),
        legend.position = "bottom",
        legend.title = element_text(size = 11),
        legend.text = element_text(color = "black", hjust = 0.3, size = 7),
        plot.margin = unit(c(1,1,0,1), "cm"))


ggsave("plots/2021/2021_04_KenyaCensus.png", dpi = 400, width = 17, height = 24, units = "cm")
        