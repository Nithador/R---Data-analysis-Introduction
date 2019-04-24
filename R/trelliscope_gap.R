library(tidyverse)
library(trelliscopejs)

tbl_gap <- as_tibble(gapminder::gapminder)

plot_gap <- function(tbl_data){
  ggplot(tbl_data, aes(x = gdpPercap,
                       y = lifeExp,
                       color = continent,
                       size = pop)) +
    geom_point()
}

tbl_viz <- tbl_gap %>%
  group_by(year) %>%
  nest() %>%
  mutate(panel = map_plot(data, plot_gap))

if(!dir.exists("trelliscope")){dir.create("trelliscope")}

trelliscope(tbl_viz,
            name = "Gapminder",
            path = "trelliscope/")
