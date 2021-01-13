
rm(list = ls())

library(tidyverse)
library(wesanderson)
library(patchwork)
library(ggthemes)
library(ggtext)


cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')


classics <- boston_cocktails %>% 
  filter(category == "Cocktail Classics") %>%
  mutate(ingredient = ifelse(ingredient == "Juice of a Lemon", "Lemon Juice", ingredient)) %>% 
  group_by(ingredient) %>% 
  add_tally() %>% 
  distinct(ingredient, .keep_all = T) %>% 
  filter(n > 6) %>% 
  select(ingredient, n) %>% 
  ungroup() %>% 
  mutate(id=row_number())

# Set a number of 'empty bar'
empty_bar <- 10

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(classics))
colnames(to_add) <- colnames(classics)
classics <- rbind(classics, to_add)
classics$id <- seq(1, nrow(classics))

# Get the name and the y position of each label
label_data <- classics
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot

categories <- c("Whiskies", "Brandy", "Vodka", "Tequila")

drinks <- boston_cocktails %>% 
  filter(category %in% categories,
         ingredient_number > 1) %>%
  group_by(category, ingredient) %>% 
  add_tally() %>% 
  distinct(ingredient, .keep_all = T) %>% 
  filter(n > 4) %>% 
  ungroup() %>% 
  mutate(category = as.factor(category)) %>% 
  select(category, ingredient, n)


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
to_add <- data.frame( matrix(NA, empty_bar*nlevels(drinks$category), ncol(drinks)) )
colnames(to_add) <- colnames(drinks)
to_add$category <- rep(levels(drinks$category), each=empty_bar)
drinks <- rbind(drinks, to_add)
drinks <- drinks %>% arrange(category)
drinks$id <- seq(1, nrow(drinks))

# Get the name and the y position of each label
label_data <- drinks
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot

plot2 <- ggplot(drinks, aes(x=as.factor(id), y=n, fill=category)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-50,120) +
  labs(title = "<b>Essential ingredients</b>",
  subtitle = "<br><br>
    <span style = 'font-size:10pt'>Vermouth seems pair well with <span style = 'color:#FF0000;'>Brandy</span>. For cocktails with <span style = 'color:#00A08A;'>Tequila</span>, citrus fruits are essential. Also for <span style = 'color:#F2AD00;'>Vodka</span>-based cocktails, lemons and limes should not be missing. Finally, bitter aromas seem to be popular for cocktails with <span style = 'color:#F98400;'>Whisky</span>.</span>",
  caption = "Data:  Mr. Boston Bartender's Guide | Graphic: @bonschorno") +
  theme_minimal() +
  scale_fill_manual(values=wes_palette(name="Darjeeling1")) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=n+10, label=ingredient, hjust=hjust), 
            color="white", fontface="bold", alpha = 0.8, size=2.8, angle= label_data$angle, inherit.aes = FALSE, family = "Roboto") +
  theme(
    plot.title = element_markdown(colour = "grey", hjust = 0.5, size = 20, family = "Roboto"),
    plot.caption = element_markdown(colour = "grey", size = 10, family = "Roboto"),
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(colour = "black", fill = "black"),
    plot.margin = unit(rep(1,4), "cm"),
    plot.subtitle = element_textbox_simple(
      hjust = 0.5,
      size = 15,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      fill = "black",
      color = "grey",
      family = "Roboto"
    ))

plot2

ggsave("cocktails.png", width = 20, height = 20, units = "cm", dpi = 500)
