library(Metrics) #for rmse 
library(rstan)
library(shinystan)
library(dplyr)
#library(lubridate) #for managing date data 
library(tidyr)

setwd("~/Documents/Ecological Forcasting EECB 701A/Competition 2")
forage.csv <- read.csv("~/Documents/Ecological Forcasting EECB 701A/Competition 2/forage_data.csv")
weather.csv <- read.csv("~/Documents/Ecological Forcasting EECB 701A/Competition 2/weather_data.csv")


#### Data Formatting ####

weather <- weather.csv %>% separate(Date, c("Year","month")) #separates date into year and month columns
weather$Year <- as.numeric(weather$Year)
weather$month <- as.numeric(weather$month)
weather$date <- paste(weather$month,1,weather$Year,sep="/")
weather$date <- as.Date(weather$date,"%m/%d/%y") #create a real date class column 
forage <- forage.csv

#combined <- merge(forage.csv,weather, by="year") #merges x & y df's using year as reference
#works but loses 1935 weather data because forage data starts in 1936


#### Data Exploration ####

plot(forage.csv$year, forage.csv$lbs_per_acre) #forage quantities over times
#plot(forage.csv[40:50,]$year, forage.csv[40:50,]$lbs_per_acre) #forage quantities over times
lines(forage.csv$year, forage.csv$lbs_per_acre)
#is there a long-term cycle/pattern? 

hist(forage.csv$lbs_per_acre, breaks = 100) #appears to be a somewhat normal distribution
#maybe a little right-skewed 

### Create and format covariates ###

# aggregating months Oct:Jan as winter precip
weather$season <- ifelse(weather$month > 9 | weather$month < 2, "winter", "") # | represents "or" 
winter_ppt <- filter(weather, season == "winter")
winter_ppt <- winter_ppt[2:length(winter_ppt$month),]
winter_ppt$year <- ifelse(winter_ppt$month < 2, (winter_ppt$Year - 1), winter_ppt$Year)
winter_ppt <- winter_ppt[,c(2:6,8)]

#sum winter precip by year
winter_ppt_sums <- winter_ppt %>% group_by(year) %>%
  summarise(ppt_sum=sum(ppt..inches.)) %>%
  as.data.frame()

#get min temp for winter
winter_temp_min <- winter_ppt %>% group_by(year) %>%
  summarise(temp_min=min(tmin..degrees.F.)) %>%
  as.data.frame()

#get avg temp for winter
winter_temp_avg <- winter_ppt %>% group_by(year) %>%
  summarise(temp_avg=mean(tmean..degrees.F.)) %>%
  as.data.frame()

#last observed min temp (january)
jan_temp_min <- filter(winter_ppt, month == 1)
jan_temp_min <- jan_temp_min[,c(3,6)]

#last observed avg temp (january)
jan_temp_avg <- filter(winter_ppt, month == 1)
jan_temp_avg <- jan_temp_avg[,c(4,6)]


#add to forage df
all_data <- merge(forage, winter_ppt_sums, by="year")
all_data <- merge (all_data, winter_temp_min, by="year")
all_data <- merge(all_data, winter_temp_avg, by="year")
all_data <- merge(all_data, jan_temp_min, by="year")
all_data <- merge(all_data, jan_temp_avg, by="year")

#re-name columns
names(all_data)[3] <- "ppt_winter"
names(all_data)[4] <- "tmin_winter"
names(all_data)[5] <- "tmean_winter"
names(all_data)[6] <- "tmin_jan"
names(all_data)[7] <- "tmean_jan"

#withhold 20% of data to train the model 
n<-length(all_data$lbs_per_acre) 
datafit<-all_data[1:(n-15),] #This is our "observed" data; remove last 15 obs 
observed15 <- all_data$lbs_per_acre[61:75] #last 15 forage observations 

nfit<-length(datafit$lbs_per_acre)

#### Re-explore data ####

#correlation table 
cor_data <- datafit
cor_results <- cor(cor_data)
write.csv(cor_results, 
          file="/Users/williamslab/Documents/Ecological Forcasting EECB 701A/Competition 2/cor_test.csv")
#no surprising results, temperature data is correlated with each other 

#look at lagged effect of precip on forage for 1,2,3 years 

#### Model Selection ####

#using dredge 
global_model <- glm(lbs_per_acre[-1]~ppt_winter[(-nfit)]+tmin_winter[(-nfit)]+
                      tmean_winter[(-nfit)]+tmin_jan[(-nfit)]+tmean_jan[(-nfit)], data=datafit) 
  
options(na.action = "na.fail")

aic_table <- MuMIn::dredge(global.model = global_model)

write.csv(aic_table, 
          file="/Users/williamslab/Documents/Ecological Forcasting EECB 701A/Competition 2/aic_table.csv")
#works despite the warning 

#### Forecasting ####

#Model 1: forage ~ previous winter ppt total 
model1 <- glm(lbs_per_acre[-1]~ppt_winter[(-nfit)], data=datafit) 

#train model to predict future weather
y <- rep(NA, 15) #empty vector for forloop
y[1] <- datafit$lbs_per_acre[nfit] #use last observed soil moisture value as the initial condition
y.ppt_winter <- all_data$ppt_winter[(n-15):(n-1)]

b0 <- model1$coefficients[1]
b1 <- model1$coefficients[2]

linear_model_1 <- function(b0,b1,y.ppt_winter){  #creating a forecasting function 
  future_forage <- b0+b1*y.ppt_winter
}

for(i in 2: 16){    
  y[i] <- linear_model_1(b0=b0, b1=b1, y.ppt_winter=y.ppt_winter[i-1])
}

plot(observed15, type="l", col="green", xlab="Days into the Future", ylab="Predicted Forage",
     main=expression("Model1"))
lines(y, type="b")
#Model 1 is okay, but peaks aren't as extreme as they should be. Maybe try AR model as well

#test full forecast 
y.full <- rep(NA, 75) #empty vector for forloop
y.full[1] <- datafit$lbs_per_acre[1]#use last observed soil moisture value as the initial condition
y.ppt_winter.full <- all_data$ppt_winter

for(i in 2:76){    
  y.full[i] <- linear_model_1(b0=b0, b1=b1, y.ppt_winter=y.ppt_winter.full[i-1])
}

plot(y.full, type="l", col="black", xlab="Days into the Future", ylab="Forage",
     main="Model 1: forage ~ winter ppt total",
     cex.main=0.9)  #predicted values
lines(all_data$lbs_per_acre, type="b", col="steelblue") #observed 
legend("topleft",c("Predicted", "Observed"), lwd=c(2,2), col=c("black","steelblue"), 
       y.intersp=1.5,cex=0.7, lty=c(1,NA), pch=c(NA,1), merge=FALSE)

rmse(data$X11C, y.7a)

# have not incorporated autoregressive model yet 
# ensemble forecasting
# regularization 
# incorporating lagged effects 




