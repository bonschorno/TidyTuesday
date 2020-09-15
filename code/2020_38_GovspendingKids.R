#Week 38: Gov Spending on Kids

library(tidykids)
library(tidyverse)
library(gt)
library(purrr)

# datawrangling

data <- tidykids %>% 
  select(state, variable, year, inf_adj_perchild)

relevantyears <- c(1997, 2007, 2016)

table <- data %>% 
  filter(year %in% relevantyears,
         variable == "PK12ed") %>% 
  mutate(inf_adj_perchild = round(inf_adj_perchild, digits = 2)) %>% 
  pivot_wider(names_from = year, values_from = inf_adj_perchild) %>% 
  rename(State = state) %>% 
  select(-variable)

# sparklines for every state

pked <- data %>% 
  filter(variable == "PK12ed") %>% 
  rename(State = state)

plot_group <- function(df){
  plot_object <- 
    ggplot(data = df, aes(x = year, y = inf_adj_perchild, group = 1)) +
    geom_line(color = "#7C7287", size = 5) +
    geom_point(data = subset(df, year == 2016), color = "#7C7287", size = 20) +
    theme_void()
  return(plot_object)
}

tibble_plot <- pked %>%
  group_by(State) %>% 
  nest() %>% 
  mutate(plot = map(data, plot_group)) %>%
  select(-data)

# creating the table

table %>% 
  mutate(ggplot = NA) %>% 
  gt() %>% 
  tab_header(title = html("<b>Public spending on elementary and<br>secondary education in the US</b>"),
             subtitle = md("Amount of USD spent in $1000s per child (inflation-adjusted)<br><br>")) %>%
  tab_source_note(source_note = md("**Data**: Urban Institute | **Table**: @bonschorno")) %>% 
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(vars(State))) %>% 
  data_color(columns = vars(`2016`),
             colors = scales::col_numeric(c("#f6f7d4", "#d2f6c5", "#99f3bd", "#28df99"), domain = NULL)) %>% 
  cols_align(align = "center",
             columns = 2:4) %>% 
  opt_table_font(font = list(c("IBM Plex Sans"))) %>% 
  tab_options(heading.title.font.size = 30,
              heading.subtitle.font.size = 15,
              heading.align = "left",
              table.border.top.color = "white",
              heading.border.bottom.color = "white",
              table.border.bottom.color = "white",
              column_labels.border.bottom.color = "grey",
              column_labels.border.bottom.width= px(1)) %>% 
  cols_label(ggplot = "Trend") %>% 
  text_transform(locations = cells_body(columns = vars(ggplot)),
                 fn = function(x) {map(tibble_plot$plot, ggplot_image, height = px(20), aspect_ratio = 5)}) %>% 
  gtsave("govspendingkids.png", expand = 10)





  
