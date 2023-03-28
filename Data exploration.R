library(tidyverse)
library(lubridate)
library(forecast)

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


# Make some lag plots
tmin_ts <- ts(data$tmin, start = c(1935, 1), end = c(2018, 1), frequency = 12)
tmean_ts <- ts(data$tmean, start = c(1935, 1), end = c(2018, 1), frequency = 12)
ppt_ts <- ts(data$ppt, start = c(1935, 1), end = c(2018, 1), frequency = 12)

lag.plot(tmin_ts, lags = 12, do.lines = F)
lag.plot(tmean_ts, lags = 12, do.lines = F)
lag.plot(ppt_ts, lags = 12, do.lines = F)

tsdisplay(tmin_ts)
tsdisplay(tmean_ts)
tsdisplay(ppt_ts)
