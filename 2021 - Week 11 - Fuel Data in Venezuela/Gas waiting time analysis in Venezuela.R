library(tidyverse)
library(scales)
library(readr)
library(lubridate)


library(showtext)
showtext::showtext_auto()
font_add_google(name = "karla", family = "sans-serif")

gas <- read_csv("gas-wait-time-analysis2.csv")

weekly_wait_time <- gas %>% 
  mutate(date_surveyed = as.Date(date_surveyed),
         week = floor_date(date_surveyed, unit = "week")) %>% 
  group_by(week, state) %>% 
  summarise(avg_wait_time = mean(hour_wait)) %>% 
  ungroup() %>% 
  filter(avg_wait_time < 1000)


background <- "#f6e0ca"
text_color <- "#2b3051"

theme_c4v <- theme(
  
  plot.background = element_rect(fill = background),
  panel.background = element_rect(fill = background),
  
  
  panel.border = element_blank(),
  
  strip.background = element_blank(),
  strip.text = element_text(family = "karla", size = 10, color = text_color),
  
  legend.background = element_blank()
  

)


weekly_wait_time %>% 
  ggplot(aes(week, avg_wait_time, color = state)) +
  geom_line(show.legend = F) +
  facet_wrap(~state, 4) +
  scale_colour_manual(values = c("#2b3051", "#2b3051", "#2b3051", "#2b3051")) +
  theme_c4v +
  labs(y = "Tiempo de Espera",
       x = NULL)


