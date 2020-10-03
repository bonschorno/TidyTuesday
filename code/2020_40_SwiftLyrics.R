# week 40: Beyoncé and Taylor Swift Lyrics

library(tidyverse)
library(quanteda)

# getting the data----

taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')

# taylor swift ----

swift <- taylor_swift_lyrics %>% 
  mutate_all(.funs=tolower) %>% 
  group_by(Album) %>% 
  summarise(Lyrics = paste(Lyrics, collapse = "")) 
  
swift_corpus <- corpus(swift, docid_field = "Album", text_field = "Lyrics")

summary(swift_corpus)

swift_tokens <- tokens(swift_corpus,
                         what = "word",
                         remove_punct = T,
                         remove_separators = T)

swift_tokens <- tokens_remove(swift_tokens, 
                                pattern = stopwords("en"))


swift_dfm <- dfm(swift_tokens)

ndoc(swift_dfm)
nfeat(swift_dfm)

topfeatures(swift_dfm, n = 50)

textplot_xray(
  kwic(swift_corpus, "love"),
  kwic(swift_corpus, "home"),
  kwic(swift_corpus, "sky"))  +
  aes(color = keyword) + scale_color_manual(values = c("#C60F7B", "#FF715B", "#F5D547")) +
  labs(x = "\n\nRelative Token Index",
       y = "",
       caption = "\n\nSource: @Rosie_Baillie_ | Graphic: @bonschorno",
       title = "Tokenizing Taylor's Texts",
       subtitle = "The following plot shows the lexical dispersion of the three words 'love', 'home', and 'sky' in all songs of Taylor Swift's albums.\nLexical dispersion is a measure of how frequently a word appears across the parts of a corpus. At first glance, you can see that\nthe word 'love' is used very often throughout all albums, while 'sky' appears very rarely. The plot also shows how the words are\ndistributed across the different songs of an album. Regrettably, the lyrics are not sorted chronologically for all albums.\nOtherwise, you could read the position of the respective token (word) out of the graphic. That is, the further left the token is\ndisplayed, the earlier the word appears in the text and vice versa.\n\n") +
  theme(text = element_text(family = "IBM Plex Sans", color = "white"),
        plot.background = element_rect(fill = "#0E1116"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "#0E1116"),
        strip.text = element_text(color = "white"),
        strip.text.x = element_text(size = 15, vjust = 2, face = "bold"),
        strip.text.y = element_text(size = 10),
        plot.title = element_text(size = 30, family = "IBM Plex Sans", face = "bold"),
        axis.text = element_text(color = "white"),
        axis.text.x = element_text(vjust = -4, size = 7),
        axis.ticks = element_blank(),
        panel.spacing = unit(2, "lines"),
        plot.subtitle = element_text(family = "IBM Plex Sans", face = "italic"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.caption = element_text(size = 7))
  
ggsave("swift_lyrics.png", height = 20, width = 40, units = "cm", dpi = 500)
