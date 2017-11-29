# LRB shiny App -- Trial 1
# includes WQ, Flow, Weather
# Visualization, Summary stats, downloads




# preambel =================================================================
library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)

wd <- getwd()

# support scripts ===

# load source script for summary table ===
source(file="E:/UFZ/10 Programs/R- Project/R_Work_def/Support.R")

# initialise count function
countValues <- function(x){
  as.integer(length(!is.na(x)[!is.na(x)==TRUE]))
}


# import data ==============================================================

# WQ data
lRaw.dat <- read.csv(file=paste0(wd,"/Data_In/WQ_Data.csv"), header = TRUE, sep = ";", dec = ".")
lRaw.dat$DateTime <- as.POSIXct(strptime(lRaw.dat$DateTime, format = "%Y-%m-%d %R"))

# Flow data
lflow_hourly.df <- read.csv(file=paste0(wd,"/Data_In/lFLOW_Hourly.csv"), header = TRUE, sep = ";", dec = ".")
lflow_daily.df <- read.csv(file=paste0(wd,"/Data_In/lFLOW_Daily.csv"), header = TRUE, sep = ";", dec = ".")
lflow_monthly.df <- read.csv(file=paste0(wd,"/Data_In/lFLOW_Monthly.csv"), header = TRUE, sep = ";", dec = ".")
# tranform Rdate to POSct
lflow_hourly.df$RDate <- as.POSIXct(lflow_hourly.df$RDate, format="%Y-%m-%d %R")
lflow_daily.df$RDate <- as.POSIXct(lflow_daily.df$RDate, format="%Y-%m-%d")
lflow_monthly.df$RDate <- as.POSIXct(lflow_monthly.df$RDate, format="%Y-%m-%d")

# weather data
weather_raw.df <- read.csv(file=paste0(wd,"/Data_In/Weather_10min.csv"), header = TRUE, sep = ";", dec = ".")
lweather_daily.df <- read.csv(file=paste0(wd,"/Data_In/lWeather_Daily.csv"), header = TRUE, sep = ";", dec = ".")
lweather_monthly.df <- read.csv(file=paste0(wd,"/Data_In/lWeather_Monthly.csv"), header = TRUE, sep = ";", dec = ".")

weather_raw.df$RDate_Xlt <- as.POSIXct(weather_raw.df$RDate_Xlt, format="%Y-%m-%d %R")
lweather_daily.df$Date <- as.POSIXct(lweather_daily.df$Date, format="%Y-%m-%d")
lweather_monthly.df$Date <- as.POSIXct(lweather_monthly.df$Date, format="%Y-%m-%d")
