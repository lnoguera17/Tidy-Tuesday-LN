library(tidyverse)
library(scales)
library(readr)
library(lubridate)


library(showtext)
showtext::showtext_auto()
font_add_google(name = "karla", family = "karla")


gas <- read_csv("gas-wait-time-analysis2.csv")

weekly_wait_time <- gas %>% 
  mutate(date_surveyed = as.Date(date_surveyed),
         month = floor_date(date_surveyed, unit = "month")) %>% 
  group_by(month, state) %>% 
  summarise(avg_wait_time = mean(hour_wait),
            count = n()) %>% 
  ungroup() %>% 
  filter(avg_wait_time < 1000) 


background <- "#f6e0ca"
text_color <- "#2b3051"
font_type <- "karla"

theme_c4v <- theme(
  
  plot.background = element_rect(fill = background),
  panel.background = element_rect(fill = background),
  plot.title = element_text(size = 20, colour = text_color, hjust = 0.5, margin = margin(10, 0, 10,0), face = "bold"),
  
  panel.border = element_blank(),
  
  strip.background = element_blank(),
  strip.text = element_text(family = font_type, size = 16, color = "black"),
  
  legend.background = element_blank(),
  
  axis.text = element_text(family = font_type ,size = 14, colour = text_color),
  axis.title.y  = element_text(family = font_type ,size = 14, colour = text_color),
  
  plot.margin = margin(0,0, 0, 0.5, unit = 'cm')

)


wait_time_gas <- weekly_wait_time %>% 
  ggplot(aes(month, avg_wait_time)) +
  geom_area(show.legend = F, alpha = 0.7) +
  geom_line(color = text_color, size = 0.7) +
  facet_wrap(~state, 4) +
  geom_text(aes(label = round(avg_wait_time, 1)), vjust =-0.3, show.legend = F) +
  scale_y_continuous(breaks = seq(0,30,10)) +
  scale_x_date(date_labels = "%b %y") +
  theme_c4v +
  labs(y = "Tiempo de Espera (Horas)",
       x = NULL,
       title = "Promedio de Horas Mensual para Reponer Gasolina en Venezuela")

wait_time_gas

ggsave(wait_time_gas,
       filename = "Tiempo de Espera Gasolina.png",
       width = 10,
       height = 14,
       device = "png")

