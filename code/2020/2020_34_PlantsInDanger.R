#Week 34: Plants in Danger

library(tidyverse)
library(ggalluvial)
library(hrbrthemes)
library(ggsci)

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')

#data wrangling
plants_clean <- plants %>% 
  filter(group == "Flowering Plant",
         threat_NA == 0) %>% 
  select(year_last_seen, contains("threat")) %>% 
  pivot_longer(-c(year_last_seen), names_to = "threat", values_to = "number")

#preparing data for plotting 
plant_alluvial <- plants_clean %>% 
  group_by(year_last_seen, threat) %>% 
  summarise(total = sum(number)) %>% 
  group_by(year_last_seen) %>% 
  mutate(slot_total = sum(total),
         total_per = total/slot_total) %>% 
  group_by(threat) %>% 
  mutate(group = group_indices()) %>% 
  drop_na(year_last_seen) %>% 
  filter(threat != "threat_NA") %>% 
  mutate(threat = str_remove(threat, "threat_")) %>% 
  mutate(threat = case_when(threat == "AA" ~ "Agriculture & Aquaculture",
                   threat == "BRU" ~ "Biological Resource Use",
                   threat == "CC" ~ "Climate Change",
                   threat == "EPM" ~ "Energy Production",
                   threat == "GE" ~ "Geological Events",
                   threat == "HID" ~ "Human Intrusions",
                   threat == "ISGD" ~ "Invasive Species", 
                   threat == "NSM" ~ "Natural System Modifications",
                   threat == "P" ~ "Pollution",
                   threat == "RCD" ~ "Commercial Development",
                   threat == "TS" ~ "Transportation Corridor")) %>% 
  filter(threat != "Geological Events",
         threat != "Pollution")

#reordering factors
fct_relevel(plant_alluvial$year_last_seen)
plant_alluvial$year_last_seen <-  fct_relevel(plant_alluvial$year_last_seen, "Before 1900")

#plot
ggplot(plant_alluvial,
       aes(x = year_last_seen, stratum = threat, alluvium = group,
           y = total,
           fill = threat, label = threat)) +
  geom_flow(alpha = 1, width = 0, curve_type = "linear") +
  scale_fill_uchicago() +
  facet_wrap(threat ~ .) +
  labs(title = "Plants in Danger\n",
    x = "",
       y = "Number of threatened species\n",
       fill = "",
    caption = "\n\nSource: IUCN Red list of Threatened Species | Graphic: @bonschorno") +
  theme_ft_rc(base_family = "IBM Plex Sans Medium") +
  theme(legend.position = "none",
        panel.grid.major.x  = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(hjust = 0.5, face = "bold", color = "white", size = 9),
        axis.text.x = element_text(size = 7, angle = 45, vjust = -.05),
        axis.text.y = element_text(size = 6),
        plot.title = element_text(size = 30, hjust = 0.5))

ggsave("ExtinctPlants.png", height = 20, width = 20, units = "cm", dpi = 500)
