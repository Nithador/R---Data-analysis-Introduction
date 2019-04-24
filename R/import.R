library(tidyverse)

# Weather data load
# starts <- cumsum(c(22, rep(c(5, 1, 1, 1), 31)))
# starts <- starts[-length(starts)]
# ends <- c(starts[-1], NA)
# 
# start = as.integer(c(1, 11, 15, 17, starts))
# end = as.integer(c(11, 15, 17, 21, ends))
# 
# names <- str_c(c("value", "mflag", "qflag", "sflag"), rep(1:31, each = 4), sep = "_")
# name = c("element", "id", "month", "year", names)
# 
# cols <- list(begin = start,
#              end = end,
#              skip = as.integer(0),
#              col_names = name)
# 
# weather_raw <- read_fwf("weather.txt", cols)
# helper <- problems(weather_raw)
# weather_raw <- weather_raw %>%
#   slice(-unique(helper$row))
# 
# weather_colnames <- colnames(weather_raw)
# weather_colnames <- str_subset(weather_colnames, "^(?:(?!flag).)*$")
# 
# weather_raw <- weather_raw %>%
#   select(weather_colnames)
# 
# save(weather_raw, file = "weather.RData")

# Course part

load("weather.RData")

any(is.na(weather_raw))

weather_raw %>%
  gather(key = day,
         value = value,
         -element, -id, -month, -year) %>%
  mutate(day = str_remove(day, "value_"),
         value = value / 10) %>%
  unite(col = date,
        year, month, day,
        sep = "-") %>%
  spread(key = element,
         value = value) %>%
  select(id,
         date,
         prcp = PRCP,
         tmin = TMIN,
         tmax = TMAX)

# Billboard data preprocessing

# raw <- read.csv("billboard.csv")
# 
# raw <- raw[, c("year", "artist.inverted", "track", "time", "date.entered", "x1st.week", 
#                "x2nd.week", "x3rd.week", "x4th.week", "x5th.week", "x6th.week", "x7th.week", 
#                "x8th.week", "x9th.week", "x10th.week", "x11th.week", "x12th.week", "x13th.week", 
#                "x14th.week", "x15th.week", "x16th.week", "x17th.week", "x18th.week", "x19th.week", 
#                "x20th.week", "x21st.week", "x22nd.week", "x23rd.week", "x24th.week", "x25th.week", 
#                "x26th.week", "x27th.week", "x28th.week", "x29th.week", "x30th.week", "x31st.week", 
#                "x32nd.week", "x33rd.week", "x34th.week", "x35th.week", "x36th.week", "x37th.week", 
#                "x38th.week", "x39th.week", "x40th.week", "x41st.week", "x42nd.week", "x43rd.week", 
#                "x44th.week", "x45th.week", "x46th.week", "x47th.week", "x48th.week", "x49th.week", 
#                "x50th.week", "x51st.week", "x52nd.week", "x53rd.week", "x54th.week", "x55th.week", 
#                "x56th.week", "x57th.week", "x58th.week", "x59th.week", "x60th.week", "x61st.week", 
#                "x62nd.week", "x63rd.week", "x64th.week", "x65th.week", "x66th.week", "x67th.week", 
#                "x68th.week", "x69th.week", "x70th.week", "x71st.week", "x72nd.week", "x73rd.week", 
#                "x74th.week", "x75th.week", "x76th.week")]
# 
# names(raw)[2] <- "artist"
# 
# raw$artist <- iconv(raw$artist, "MAC", "ASCII//translit")
# raw$track <- str_replace(raw$track, " \\(.*?\\)", "")
# names(raw)[-(1:5)] <- str_c("wk", 1:76)
# raw <- arrange(raw, year, artist, track)
# 
# write_csv(raw, path = "billboard2.csv")

# Billboard course

library(lubridate)

billboard_raw <- read_csv("billboard2.csv")

billboard_clean <- billboard_raw %>%
  gather(key = week,
         value = rank,
         -1:-5) %>%
  mutate(week = as.integer(str_remove(week, "wk")),
         date = date.entered + weeks(week - 1)) %>%
  arrange(year, artist, track, time, date.entered)


# TB table
tb_raw <- read_csv("tb.csv")

tb_clean <- tb_raw %>%
  select(-new_sp) %>% # drop column new_sp
  rename(country = 1) %>% # rename the first column
  filter(year == 2000) %>% # select only rows with year equal to 2000
  gather(key = group,
         value = cases,
         -country,
         -year) %>% # gather all the columns except country and year
  mutate(group = str_replace(group, "new_sp_", replacement = "")) %>%
  separate(group,
           into = c("gender", "age"),
           sep = 1) %>%
  drop_na %>%
  arrange(country, year, cases)

# PEW data

library("foreign")

# Data from http://pewforum.org/Datasets/Dataset-Download.aspx

pew_raw <- read.spss("pew.sav")
pew_raw <- as_tibble(pew_raw)

pew_clean <- pew_raw %>%
  select(q16, reltrad, income) %>%
  mutate(reltrad = str_replace(as.character(reltrad), "Churches", "")) %>%
  mutate(reltrad = case_when(
    str_detect(q16, "Atheist") ~ "Atheist",
    str_detect(q16, "Agnostic") ~ "Agnostic",
    TRUE ~ as.character(reltrad)
  ))

save(tb_clean, pew_clean, billboard_clean, file = "cleaning_results.RData")