## ENVST 325 Activity 4
## Author: Jacqueline Reynaga
## Date Created: 2-24-26
## Date Last Modified: 2-24-26

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
#### solar radiation measurements issues on the sensor in May and June of 2021
checkSensor <- weather %>% 
  filter(month(weather$dateF) == 5 |
           month(weather$dateF) == 6)
ggplot(checkSensor, aes(x = dateET, y = SolRad)) +
  geom_line()


# homework prompts --------------------------------------------------------


