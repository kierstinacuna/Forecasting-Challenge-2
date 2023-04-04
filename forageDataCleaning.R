library(dplyr)
library(tidyr)
library(lubridate)

setwd("~/Documents/GitHub/CourseResources/EcologicalForecasting/challenge1/")

weather <- read.csv("Weather data.csv") %>%
  mutate(date = ymd(paste(as.character(Date), "-01", sep = ""))) %>%
  rename(month = Date,
         pptIn = ppt..inches.,
         tmin = tmin..degrees.F.,
         tmean = tmean..degrees.F.)

forage <- read.csv("Forage Data.csv") %>%
  mutate(date = mdy(paste("05-01-", as.character(year), sep = "")),
         month = paste(as.character(year), "-05", sep = ""))

modelIn <- weather %>%
  left_join(forage, by = "month") %>%
  select(-date.x, -date.y) %>%
  mutate(date = ymd(paste(month, "-01", sep = "")),
         month = month(date),
         year = year(date),
         winterYear = ifelse(month == 1, year-1, year),
         winterMonth = ifelse(month == 1, 12, month-1)) %>%
  select(pptIn, tmin, tmean, year, winterMonth, winterYear) %>%
  mutate(year = winterYear + 1) %>%
  left_join(forage, by = "year") %>%
  select(-date, -month) %>%
  mutate(percentile = cume_dist(lbs_per_acre)) %>%
  pivot_longer(cols = c(colnames(.[1:3])),
               names_to = "variable",
               values_to = "data") %>%
  mutate(label = paste(variable, winterMonth, sep = "_")) %>%
  select(-variable, -winterMonth) %>%
  pivot_wider(names_from = "label", values_from = "data")

write.csv(modelIn, "month columns.csv")
