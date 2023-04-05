# Get set up ----

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


# Make some lag plots ----
tmin_ts <- ts(data$tmin, start = c(1935, 1), end = c(2018, 1), frequency = 12)
tmean_ts <- ts(data$tmean, start = c(1935, 1), end = c(2018, 1), frequency = 12)
ppt_ts <- ts(data$ppt, start = c(1935, 1), end = c(2018, 1), frequency = 12)

lag.plot(tmin_ts, lags = 12, do.lines = F)
lag.plot(tmean_ts, lags = 12, do.lines = F)
lag.plot(ppt_ts, lags = 12, do.lines = F)

tsdisplay(tmin_ts)
tsdisplay(tmean_ts)
tsdisplay(ppt_ts)

# Make more vars ----
# Precip accumulation from the previous winter and fall is probably important for 
#   spring growth, unclear how far into the fall precip is important though
# Make two additional variables, one representing previous winter precip, and one 
#   representing previous winter and fall precip
# For each years previous winter precip, I want the current year's January precip,
#   and the previous year's December preicp

# Corina made some great aggregated variables! Jan temps are previous year Jan temps though

# combined <- read.csv("aggregated variables.csv")
# combined <- combined %>% 
#   rename(Year = year) %>% 
#   select(!X)
# 
# 
# # Summer temps and precip might also impact forage because it would impact seed set
# #   We would expect more viable seeds in cooler, wetter summers
# 
# summervars <- data %>% 
#   filter(Month %in% c(6, 7, 8, 9)) %>% 
#   group_by(Year) %>% 
#   summarise(ppt_summer = sum(ppt),
#             tmean_summer = mean(tmean),
#             tmin_summer = min(tmin))
# 
# augustvars <- data %>% 
#   filter(Month == 8) %>% 
#   group_by(Year) %>% 
#   summarise(tmean_aug = mean(tmean),
#             tmin_aug = min(tmin))
# 
# combined <- merge(combined, summervars, by="Year", all.x = T)
# combined <- merge(combined, augustvars, by="Year", all.x = T)
# 
# write.csv(combined, "aggregated variables.csv")

# I didn't aggregate summer data correctly (needed to be year before) so here's a dataframe
#   that Seth made:

monthcols <- read.csv("month columns.csv")

# Add aggregated vars to this new dataframe
combined <- monthcols %>% 
  mutate(ppt_winter = pptIn_12 + pptIn_11 + pptIn_10 + pptIn_9,
         tmin_winter = pmin(tmin_12, tmin_11, tmin_10, tmin_9),
         tmean_winter = (tmin_12 + tmin_11 + tmin_10 + tmin_9)/4,
         tmin_jan = tmin_12,
         tmean_jan = tmean_12,
         ppt_summer = pptIn_5 + pptIn_6 + pptIn_7 + pptIn_8,
         tmin_summer = pmin(pptIn_5, pptIn_6, pptIn_7, pptIn_8),
         tmean_summer = (pptIn_5 + pptIn_6 + pptIn_7 + pptIn_8)/4,
         tmin_aug = tmin_7,
         tmean_aug = tmean_7) %>% 
  select(c(year, winterYear, lbs_per_acre, percentile, ppt_winter, tmin_winter, tmean_winter,
           tmin_jan, tmean_jan, ppt_summer, tmin_summer, tmean_summer, tmin_aug, tmean_aug)) %>% 
  filter(year != 1935) %>% 
  filter(year < 2011)

write.csv(combined, "aggregated variables.csv")

# Make some models ----
model1 <- lm(lbs_per_acre ~ ppt_summer + tmean_summer, data = combined)
model2 <- lm(lbs_per_acre ~ ppt_summer + tmin_summer, data = combined) 
model3 <- lm(lbs_per_acre ~ ppt_summer + tmean_aug, data = combined)
model4 <- lm(lbs_per_acre ~ ppt_summer + tmin_aug, data = combined)

AIC(model1, model2, model3, model4)
# model 2 has lowest AIC

# Try adding winter ppt in
model5 <- lm(lbs_per_acre ~ ppt_summer + tmin_summer + ppt_winter, data = combined)
model6 <- lm(lbs_per_acre ~ tmin_summer + ppt_winter, data = combined)
model7 <- lm(lbs_per_acre ~ ppt_summer + ppt_winter, data = combined)

AIC(model2, model5, model6, model7)
# Models 6 and 7 have the lowest AIC

par(mfrow = c(2,2))
plot(model6)
plot(model7)
# Model6 residuals look really bad, model7 looks slightly less bad, so I'll go with that one!

# Make some forecasts ----
predictdata <- monthcols %>% 
  mutate(ppt_winter = pptIn_12 + pptIn_11 + pptIn_10 + pptIn_9,
         tmin_winter = pmin(tmin_12, tmin_11, tmin_10, tmin_9),
         tmean_winter = (tmin_12 + tmin_11 + tmin_10 + tmin_9)/4,
         tmin_jan = tmin_12,
         tmean_jan = tmean_12,
         ppt_summer = pptIn_5 + pptIn_6 + pptIn_7 + pptIn_8,
         tmin_summer = pmin(pptIn_5, pptIn_6, pptIn_7, pptIn_8),
         tmean_summer = (pptIn_5 + pptIn_6 + pptIn_7 + pptIn_8)/4,
         tmin_aug = tmin_7,
         tmean_aug = tmean_7) %>% 
  select(c(year, winterYear, lbs_per_acre, percentile, ppt_winter, tmin_winter, tmean_winter,
           tmin_jan, tmean_jan, ppt_summer, tmin_summer, tmean_summer, tmin_aug, tmean_aug))

