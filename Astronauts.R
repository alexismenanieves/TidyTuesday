# 1.Load libraries
library(tidyverse)
library(ggtext)
library(glue)
library(extrafont)

# 2.Get data
url <- glue("https://raw.githubusercontent.com/rfordatascience/",
"tidytuesday/master/data/2020/2020-07-14/astronauts.csv")
astronauts <- read_csv(url)

# 3.Adecuate data
astronauts <- astronauts %>% 
  mutate(astro_age = year_of_mission - year_of_birth)

theme_astro <- function(){
  theme_minimal() %+replace%
    theme(
      plot.background = element_rect(fill = "#1d1330", color = "#1d1330"),
      panel.grid = element_line(color = "#1d1330"),
      panel.background = element_rect(fill = "#1d1330", color = "#1d1330"),
      text = element_text(family = "Metropolis", color = "white"),
      plot.title = element_textbox_simple(size = 20),
      plot.subtitle = element_textbox_simple(hjust = 0, size = 11, lineheight = 1),
      axis.text = element_text(color = "white", size = 10),
      axis.title = element_text(color = "white", size = 10)
    )
}

texts <- tibble(
  age = c(77,18),
  year = c(2010,1975),
  text = c("The older man who went to space was **John Herschel Glenn Jr.** aged 77 in 1999",
           "The **two youngest astronauts** were Gherman Titov and Valentina Tereshkova, both aged 26."),
  vjust = c(-0.5,-0.5)
)

astronauts %>% 
  ggplot() + 
  geom_point(aes(astro_age, year_of_mission, color = sex, 
                 size = hours_mission, alpha = total_number_of_missions),
             show.legend = FALSE) + 
  theme_astro() +
  scale_color_manual(values = c(male = "#e1f7fa", female = "#ffa72b")) +
  labs(
    title = "<b>Ages through time and space<b>",
    subtitle = glue("<br>Astronauts stay longer and are older than before.\n",
                    "Each dot is an astronaut on a mission. The larger the dot, ",
                    "the more hours the mission took, ranging from 0 to over 10,000 ",
                    "(14 months).<br>"),
    y = "",
    x = "Age of the astronauts at mission",
    caption = "\n#TidyTuesday | Graphic: Manu  "
  ) +
  xlim(c(10,85)) +
  geom_textbox(data = texts,
               aes(age,year, label = text),
               color = "white",
               family = "Metropolis",
               box.color = "#1d1330",
               fill = "#1d1330",
               maxwidth = unit(8,"lines"),
               size = 3.8,
               hjust = .5,
               show.legend = FALSE) +
  annotate("curve", x = 77, xend = 77, y = 2005, yend = 1999.5, curvature = 0,
           size = .75, arrow = arrow(length = unit(2, "mm")), color = "#938ca1") +
  annotate("curve", x = 18, xend = 25, y = 1966, yend = 1962,
           size = .75, arrow = arrow(length = unit(2,"mm")), color = "#ffa72b")
  
