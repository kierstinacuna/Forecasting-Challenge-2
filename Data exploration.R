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

# Precip accumulation from the previous winter and fall is probably important for 
#   spring growth, unclear how far into the fall precip is important though
# Make two additional variables, one representing previous winter precip, and one 
#   representing previous winter and fall precip
# For each years previous winter precip, I want the current year's January precip,
#   and the previous year's December preicp

# Corina made some great aggregated variables! Jan temps are previous year Jan temps though

combined <- read.csv("aggregated variables.csv")


# Summer temps and precip might also impact forage because it would impact seed set
#   We would expect more viable seeds in cooler, wetter summers

summerppt <- data %>% 
  filter(Month %in% c(6, 7, 8, 9)) %>% 
  group_by(Year) %>% 
  summarise(ppt_summer = sum(ppt))

summertmean <- data %>% 