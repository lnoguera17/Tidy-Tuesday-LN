library(tidytuesdayR)
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
showtext::showtext_auto()
font_add_google(name = "Roboto", family = "sans-serif")


# data
georgia_pop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')
georgia_pop




font <- "sans-serif"

theme_dubois <- theme(plot.background = element_rect(fill = "#ebd4b9"),
                      panel.background = element_rect(fill = "#ebd4b9"),
                      legend.background = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_line(colour = 'pink'),
                      plot.title = element_markdown(face = 'bold', size = 20, hjust = 0.5, lineheight = 0.2, color = "black", margin = margin(5,10,20,10), family = font),
                      plot.margin = margin(10,40,5,40),
                      legend.position = 'bottom',
                      legend.title = element_blank(),
                      legend.text = element_text(margin = margin(0,5,0,5, unit = 'pt')),
                      legend.key = element_blank(),
                      axis.title = element_text(family = font, size = 8),
                      panel.border = element_rect(colour = "black", fill=NA, size=0.2),
                      axis.text = element_text(size = 16, family = font))


dubois.challenge <- georgia_pop %>% 
  pivot_longer(Colored:White, names_to = "Race", values_to = "Population") %>% 
  arrange(desc(Year)) %>%
  ggplot(aes(Year, Population, linetype = Race)) +
  geom_line(aes(linetype = Race)) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_reverse(breaks = seq(0,100,5)) +
  coord_flip() +
  labs(title = "COMPARATIVE INCREASE OF WHITE AND COLORED <br> POPULATION OF GEORGIA.",
       y = 'PERCENTS',
       x = NULL) +
  theme_dubois

dubois.challenge

ggsave(dubois.challenge, filename = "dubois.challenge.png",
       width = 10,
       height = 15)




