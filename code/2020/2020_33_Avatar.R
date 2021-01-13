#avatar

library(tidyverse)
library(tidytext)
library(ggrepel)
library(ggtext)

avatar <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')
scene_description <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/scene_description.csv')

data(stop_words)

airbenders <- c("Aang", "Katara", "Sokka", "Zuko", "Iroh", "Azula")

avatar_slim <- avatar %>% 
  select(book, character, character_words) %>% 
  filter(character != "Scene Description",
         character %in% airbenders)

avatar_tokens <- avatar_slim %>% 
  unnest_tokens(output = word, input = character_words)

avatar_tokes_clean <- avatar_tokens %>% 
  anti_join(stop_words)

words_character <- avatar_tokes_clean %>% 
  group_by(character) %>% 
  count(word, sort = TRUE) 

words <- words_character %>% 
  group_by(character) %>% 
  mutate(rank = dense_rank(desc(n))) %>% 
  filter(rank < 10)

ggplot(data = subset(words, character == "Aang"), aes(x = word, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(character ~ .)


# sentiment analysis ------------------------------------------------------

avatar_slim <- avatar %>% 
  select(book, chapter, chapter_num, character_words)

avatar_tokens <- avatar_slim %>% 
  unnest_tokens(output = word, input = character_words)

avatar_tokes_clean <- avatar_tokens %>% 
  anti_join(stop_words)

nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

avatar_sentiment <- avatar_tokes_clean %>% 
  group_by(book, chapter, chapter_num) %>% 
  count(word, sort = T) %>% 
  inner_join(bing) %>% 
  arrange(book, chapter_num)

aggregated_sentiment <- avatar_sentiment %>% 
  group_by(book, chapter, chapter_num, sentiment) %>% 
  summarise(total = sum(n)) %>% 
  mutate(total = ifelse(sentiment == "positive", total, total*-1))

ggplot(aggregated_sentiment, aes(x = chapter_num, y = total, fill = sentiment)) +
  geom_segment(aes(x = chapter_num, xend = chapter_num, y = 0, yend = total, color = sentiment)) +
  geom_point(aes(color = sentiment)) +
  facet_wrap(. ~ book) +
  labs(x = "\nChapter",
       y = "Number of words\n") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank())

aggregated_sentiment_long <- avatar_sentiment %>% 
  group_by(book, chapter, chapter_num, sentiment) %>% 
  summarise(total = sum(n)) %>% 
  pivot_wider(names_from = "sentiment", values_from = "total") %>% 
  mutate(positivity = positive - negative,
         positivity_color = as.factor(ifelse(positivity < 0, 1, 0)),
         chapter = case_when(chapter == "Sozin's Comet, Part 2: The Old Masters" ~ "\nThe Old Masters",
                             chapter == "The Waterbending Master" ~ "\nThe Waterbending Master",
                             TRUE ~ chapter))

  ggplot(aggregated_sentiment_long, aes(x = chapter_num, y = positivity, fill = positivity_color, label = chapter)) +
  geom_segment(data = subset(aggregated_sentiment_long, positivity > 2 | positivity < 0), aes(x = chapter_num, xend = chapter_num, y = 0, yend = positivity, color = positivity_color),
               arrow = arrow(length = unit(0.1, "inches"), type = "closed"), size = 1.5, linejoin = "mitre") +
  scale_color_manual(values = c("#EEB05A", "#87AFD1")) +
  scale_x_continuous(breaks = c(1, 10, 20), labels = c(1, 10, 20)) +
  geom_text_repel(data = subset(aggregated_sentiment_long, positivity > 15), aes(color = positivity_color), 
                  angle = 90, size = 3, point.padding = 0.2, family = "Herculanum") +
  facet_wrap(. ~ book) +
  labs(title = "Avatar - The Last Airbender",
       subtitle = "<br>'In a war-torn world of elemental magic, a young boy reawakens to undertake a dangerous mystic quest to fulfill<br>his destiny as the Avatar, and bring peace to the world.' - Kenneth Chisholm on IMDb Reviews. <br><br>The plot shows for each chapter of the three books whether more <span style='color:#87AFD1'>positive</span> or more <span style='color:#EEB05A '>negative</span> words were mentioned.<br>The analysis was performed with the sentiment lexicon provided by Bing Liu and collaborators.<br><br>",
       caption = "\nSource: appa | Graphic: @bonschorno",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        strip.text = element_text(face = "bold", size = 15, family = "Herculanum"),
        axis.title.y = element_text(family = "Herculanum"),
        text = element_text(family = "Herculanum"),
        plot.title = element_text(hjust = 0.5, size = 25),
        plot.subtitle = element_markdown())

ggsave("positivity.png", height = 20, width = 30, units = "cm", dpi = 500)  
