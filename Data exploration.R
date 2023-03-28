library(tidyverse)
library(lubridate)

setwd("C:/Users/kiers/OneDrive/Documents/GitHub/Forecasting-Challenge-2")

forage <- read.csv("Forage Data.csv")
weather <- read.csv("Weather data.csv")

str(forage)
str(weather)

# Read in dates as dates, change column names
weather <- weather %>% 
  mutate(Date = ym(Date),
         Year = year(Date),
         Month = month(Date)) %>% 
  rename(ppt = ppt..inches.,
         tmin = tmin..degrees.F.,
         tmean = tmean..degrees.F.) %>% 
  relocate(Year, .after = Date) %>% 
  relocate(Month, .after = Year)

forage <- forage %>% 
  rename(Year = year,
         Forage = lbs_per_acre)

# Merge data together
data <- merge(weather, forage, by="Year", all.x = T)

data <- data %>% 
  arrange(Year, Month) %>% 
  relocate(Date, .before = Year)

write.csv(data, "Full data.csv")

# Make histograms, explore relationships
panel.hist <- function(x, ...)
{
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

pairs(data[,2:7], diag.panel = panel.hist)
