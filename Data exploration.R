library(tidyverse)
library(lubridate)

setwd("C:/Users/kiers/OneDrive/Documents/GitHub/Forecasting-Challenge-2")

forage <- read.csv("Forage Data.csv")
weather <- read.csv("Weather data.csv")

str(forage)
str(weather)

# Read in dates as dates, change column names
test <- weather %>% 
  mutate(Date = ym(Date),
         Year = year(Date),
         Month = month(Date)) %>% 
  rename(ppt = ppt..inches.,
         tmin = tmin..degrees.F.,
         tmean = tmean..degrees.F.) %>% 
  relocate(Year, .after = Date) %>% 
  relocate(Month, .after = Year)
