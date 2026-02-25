## ENVST 325 Activity 4
## Author: Jacqueline Reynaga
## Date Created: 2-24-26
## Date Last Modified: 2-25-26

install.packages(c("dplyr", "lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

# in-class prompts --------------------------------------------------------
## read in data
### change #N/As from strings to actual NA values
weather <- read.csv("/cloud/project/activity04/campus_weather.csv", na.strings = "#N/A")
metaData <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv")

## deal with dates/times
### UTC - universal time (gold standard)
### example: central time zone is UTC - 6 or UTC - 5 (daylight savings)
### make sure time zones match between data -> present/interpret data in local time
weather$dateF <- mdy_hm(weather$Date)
weather$dateET <- mdy_hm(weather$Date, tz = "America/New_York")
### check 4 failed to parse warning
weather %>% 
  filter(is.na(dateET))
### times that daylight savings jumps

## functions
### set up a time interval
weather$dateF[2] %--% weather$dateF[3]
int_length(weather$dateF[2] %--% weather$dateF[3]) # default value = seconds

test <- weather$dateF[1:10]
test
test[-1] # remove first observation

### x = date vector
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
}

timeCheck900(weather$dateF)

## for loops
### combining csvs of all the same type of data
soilFiles <- list.files("/cloud/project/activity04/soil/")
soilList <- list()

for(i in 1:length(soilFiles)) {
  soilList[[i]] <- read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
}
str(soilList)

soilData <- do.call('rbind', soilList)

## prompt 1
###calculate moving average
airMA <- numeric()

for(i in 8:(length(weather$AirTemp))) {
  airMA[i] <- mean(weather$AirTemp[(i - 7):i])
}
weather$airMA <- airMA

## prompt 2
### solar radiation measurements issues on the sensor in May and June of 2021
weather$doy <- yday(weather$dateF)
weather$month <- month(weather$dateF)
weather$year <- year(weather$dateF)

checkSensor <- weather %>% 
  filter(weather$month == 5 |
           weather$month == 6)
ggplot(checkSensor, aes(x = dateET, y = SolRad)) +
  geom_line()


# homework prompts --------------------------------------------------------
## prompt 1
### precipitation data issues

#### check precipitation visualization
ggplot(weather[weather$doy > 121 & weather$doy < 274 ,], 
       aes(x = dateF, y = Precip)) +
  geom_col(color = 'steelblue') +
  labs(x = "Date", y = "Precipitation (mm)") +
  theme_classic() # data unreliable may-june (no large or trace events)
#### filter out issues
precipIssues <- weather %>% 
  filter(AirTemp >= 0 |
           XLevel < abs(2) |
           YLevel < abs(2) |
           month == 5 |
           month == 6)
#### see how many data points were removed
nrow(weather) - nrow(precipIssues)

## prompt 2
### setting up a flag when battery drops below 8.5 V
#### BatVolt col is in mV so convert 8.5 V to mV
weather$batteryFlag <- ifelse(weather$BatVolt < 8500, 1, 0)

## prompt 3
### function that checks for unrealistic ranges in air temp and solar radiation
ggplot(weather, aes(x = dateF, y = SolRad)) +
  geom_line()
ggplot(weather, aes(x = dateF, y = AirTemp)) +
  geom_line()

solRadAndAirTempCheck <- function(solar_data_vector, air_temp_vector){
  
  sol_intervals <- solar_data_vector[-length(solar_data_vector)] %--% solar_data_vector[-1]
  sol_interval_times <- int_length(sol_intervals)
  sol_intervals[sol_interval_times != air_temp_vector]
}

## prompt 4
### winter air temp plot from Jan-March 2021 and look for snow accumulation
winterAir <- weather %>% 
  filter(between(month, 1, 3) &
           year == 2021)
ggplot(winterAir, aes(x = dateF, y = AirTemp)) +
  geom_line(color = 'slategrey') +
  labs(x = "Date", y = "Air Temperature (°C)") +
  theme_classic()

## prompt 5
### total precipitation March-April 2021
#### air temp measured in C so need to turn 35 F to C
minTempC <- (35 - 32) / (9/5)
weather$dmy <- make_date(year = weather$year, 
                         month = weather$month, 
                         day = mday(weather$dateF))
#### add up daily precipitation and get minimum daily air temp
totalPrecip <- weather %>% 
  filter(between(month, 3, 4) &
           year == 2021) %>% 
  group_by(doy, dmy) %>% 
  summarise(dailyPrecip = sum(Precip),
            minAirTemp = min(AirTemp))

for(i in 1:nrow(totalPrecip)) {
  totalPrecip$dailyPrecip[i] <- ifelse(i == 1, 
                                       ifelse(totalPrecip$minAirTemp[i] < minTempC, 
                                              NA, totalPrecip$dailyPrecip[i]),
                                       ifelse(totalPrecip$minAirTemp[i] < minTempC | 
                                                totalPrecip$minAirTemp[(i-1)] < minTempC,
                                              NA, totalPrecip$dailyPrecip[i]))
}

sum(totalPrecip$dailyPrecip, na.rm = TRUE)
sum(!is.na(totalPrecip$dailyPrecip))
nrow(totalPrecip)

## prompt 6
### alter time function to include user specified time interval
timeCheck <- function(date_vector, time_interval){
  intervals <- date_vector[-length(date_vector)] %--% date_vector[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != time_interval]
}
#### soil data read in earlier
soilData$dateF <- ymd_hm(soilData$Timestamp)
timeCheck(soilData$dateF, 3600)
