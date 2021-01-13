# Tidy Tuesday (Week 3): Art Collections

library(tidyverse) # datawrangling
library(treemapify) # treemaps

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')

# data wrangling
data <- artwork %>% 
  drop_na(year, acquisitionYear)

# counting n works per artist
n_works <- data %>% 
  group_by(artist) %>% 
  count(sort = T)

# selecting top 15
top15 <- n_works %>% 
  ungroup() %>% 
  mutate(n_char = paste0("(", n, ")"),
         artist_n = paste(artist, n_char, sep = "\n")) %>% 
  slice(2:16) 

# plot
ggplot(top15, aes(area = n, fill = artist, label = artist_n)) +
  geom_treemap(radius = grid::unit(0.15, "cm"), color = "white", start = "topleft") +
  geom_treemap_text(colour = "white", place = "topleft",
                    reflow = T, family = "Playfair Display", start = "topleft",
                    padding.x = grid::unit(3, "mm"),
                    padding.y = grid::unit(3, "mm"),) +
  labs(title = "Art Collections - Tate Modern",
       subtitle = "<br>Tate Modern is one of the largest museums of modern and contemporary art in the world. As with the UK's other national galleries<br>and museums, there is <b>no admission charge<b> for access to the collection displays, which take up the majority of the gallery space [...]<br>(Source: Wikipedia)<br><br>Listed, with the exception of Joseph Mallord William Turner, are the artists with the most works in the gallery. The larger the<br>number of works, the larger the respective area. They include famous names like Andy Warhol, landscape painters like William<br>Daniell or performance artist like Joseph Beuys.<br>", 
       caption = "\nSource: Tate Art Museum | Graphic: @bonschorno") +
  theme(legend.position = "none") +
  #scale_fill_manual(values = milkmaid) +
  scale_fill_grey(start = 0.1, end = 0.5) +
  theme(panel.background = element_rect(fill = "lightgrey"),
        plot.background = element_rect(fill = "lightgrey"),
        plot.title = element_text(family = "Playfair Display", face = "bold", size = 43, hjust = 0.5),
        plot.subtitle = ggtext::element_markdown(family = "Playfair Display", size = 10, lineheight = 1.2),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.caption = element_text(family = "Playfair Display"))

ggsave("plots/2021/2021_03_ArtCollections.png", dpi = 400, width = 23, height = 27, units = "cm")
