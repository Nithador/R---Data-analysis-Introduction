library(gapminder)
library(tidyverse)

tbl_gap <- as_tibble(gapminder)
tbl_gap

tbl_gap %>%
  pull(year) %>%
  unique

# Draw a plot of year vs gdpPerCap colored by the continent. Use geom "smooth".0
tbl_gap %>%
  qplot(data = ., x = year, y = gdpPercap, color = continent, geom = "smooth")

# Plot histogram of lifeExp in 2007, set fill to continent.
tbl_gap %>%
  filter(year == 2007) %>%
  qplot(data = ., x = lifeExp, geom = "histogram", fill = continent)

# Compute max and mean gdpPercap by continent
tbl_gap %>%
  filter(year < 1970) %>%
  group_by(continent) %>%
  summarize(max_gdp = max(gdpPercap),
            mean_gdp = mean(gdpPercap),
            min_gdp = min(gdpPercap))
  
# Compute the top 3 countries for each continent by mean gdpPercap in 21st century
tbl_gap %>%
  filter(year > 2000) %>%
  group_by(continent, country) %>%
  summarize(mean_gdp = mean(gdpPercap)) %>%
  arrange(desc(mean_gdp)) %>%
  top_n(3)

# Find out which countries had the biggest absolute changes in life expectancy
tbl_gap %>%
  group_by(country) %>%
  summarize(lifeExp_change = sum(abs(diff(lifeExp)))) %>%
  arrange(desc(lifeExp_change))

# Draw a line plot of year vs lifeExp for the first three countries. Color lines by country.
tbl_gap %>%
  filter(country %in% c("Rwanda", "Cambodia", "China")) %>%
  qplot(data = ., x = year, y = lifeExp, color = country, geom = "line")


audit <- read_csv(url("http://rattle.togaware.com/audit.csv"))
audit

