# LRB shiny App -- Trial 1
# includes WQ, Flow, Weather
# Visualization, Summary stats, downloads


# Note:
# This code imports the input data from a local directory /Data_In/ that has to be
# present in the working directory.
# 
# All the code under wd/Code/ besides Support.R, is not used in this version.
# It would have ot be used when unprepared raw data from data source files will be used.


# preambel =================================================================
library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)

wd <- getwd()

data_dir <- "E:/ownCloud/LRB - DataProcess"


# support scripts ===

# load source script for summary table ===
source(file=paste0(wd,"/Code/Support.R"))

# initialise count function
countValues <- function(x){
  as.integer(length(!is.na(x)[!is.na(x)==TRUE]))
}


# import data from local wd/Data_In/...     ====================================================

# set data dir
if (dir.exists("Data_In")==TRUE) {
  data_dir <- "/home/shinyadmin/owncloud/LRB_DataProcess"

  
  #=== load data from ownCloud ====================================
  load(file = paste0(data_dir, "/03 R_formats/LRB_WQ.rda"))
  load(file = paste0(data_dir, "/03 R_formats/LRB_Weather.rda"))
  load(file = paste0(data_dir, "/03 R_formats/LRB_Flow.rda"))
  
  
  

  # load samplepoint info
  SPoint.dat <- read.csv(paste0(data_dir,"/02 Management_CSV/LRB_SamplePoint.csv"), header=TRUE, sep=";")
  
  # load system information 
  systems_df <- read.csv(paste0(data_dir,"/02 Management_CSV/LRB_Systems.csv"), header=TRUE, sep=";")
  
  
  
  
  
  #=== preprocess wq data =========================================
  
  # add log_Ecoli
  Raw.dat$log_Ecoli <- log10(Raw.dat$Ecoli)
  
  # order df
  Raw.dat <- Raw.dat[order(Raw.dat$SamplePoint, Raw.dat$DateTime),]
  
  # add sample point informatoin
  Raw.dat <- left_join(Raw.dat, SPoint.dat, by="SamplePoint")
  #rm(SPoint.dat)
  
  # tranform to longformat
  lRaw.dat <- Raw.dat %>% 
                        gather(Parameter, value,
                               T_lab:log_Ecoli
                        ) %>%
                            select(
                              DateTime,SystemType, FlowDirection, System, SampleType, SamplePoint, dist_axial, Parameter, value
                            )
  # remove empty entries and duplicates
  lRaw.dat <- na.omit(lRaw.dat)
  lRaw.dat <- distinct(lRaw.dat)
  rm(Raw.dat)
  
  
  
  
  
  #=== preprocess flow data =======================================
  
  lflow_hourly.df <- left_join(lflow_hourly.df, select(systems_df, c(System, FlowDirection)),
                                by=c("System"="System")
                      )
  
  lflow_hourly.df <- na.omit(lflow_hourly.df)
  lflow_hourly.df <- distinct(lflow_hourly.df)
  
  
  
  
  lflow_daily.df <- left_join(lflow_daily.df, select(systems_df, c(System, FlowDirection)),
                              by=c("System"="System")
                     )
  lflow_daily.df <- na.omit(lflow_daily.df)
  lflow_daily.df <- distinct(lflow_daily.df)
  
  
  
  
  lflow_monthly.df <- left_join(lflow_monthly.df, select(systems_df, c(System, FlowDirection)),
                                by=c("System"="System")
                      )
  
  lflow_monthly.df <- na.omit(lflow_monthly.df)
  lflow_monthly.df <- distinct(lflow_monthly.df)
  
  
  
  #=== preprocess weather data
  
  
  rm(weather_daily.df, weather_monthly.df)
  
  lweather_daily.df$Date <- as.POSIXct(lweather_daily.df$Date, format="%Y-%m-%d")
  
  weather_raw.df$RDate_Xlt <- as.POSIXct(weather_raw.df$RDate_Xlt)
  
  
  
  
  
  
  
  
  
  rm(SPoint.dat)
  
  
} else {
    data_dir <- "/Data_Example/"
  

# WQ data
lRaw.dat <- read.csv(file=paste0(wd,data_dir, "WQ_Data.csv"), header = TRUE, sep = ";", dec = ".")
lRaw.dat$DateTime <- as.POSIXct(strptime(lRaw.dat$DateTime, format = "%Y-%m-%d %R"))

# Flow data
lflow_hourly.df <- read.csv(file=paste0(wd, data_dir, "lFlow_Hourly.csv"), header = TRUE, sep = ";", dec = ".")
lflow_daily.df <- read.csv(file=paste0(wd, data_dir, "lFlow_Daily.csv"), header = TRUE, sep = ";", dec = ".")
lflow_monthly.df <- read.csv(file=paste0(wd, data_dir, "lFlow_Monthly.csv"), header = TRUE, sep = ";", dec = ".")
# tranform Rdate to POSct
lflow_hourly.df$RDate <- as.POSIXct(lflow_hourly.df$RDate, format="%Y-%m-%d %R")
lflow_daily.df$RDate <- as.POSIXct(lflow_daily.df$RDate, format="%Y-%m-%d")
lflow_monthly.df$RDate <- as.POSIXct(lflow_monthly.df$RDate, format="%Y-%m-%d")

# weather data
weather_raw.df <- read.csv(file=paste0(wd,data_dir, "Weather_10min.csv"), header = TRUE, sep = ";", dec = ".")
lweather_daily.df <- read.csv(file=paste0(wd, data_dir, "lWeather_Daily.csv"), header = TRUE, sep = ";", dec = ".")
lweather_monthly.df <- read.csv(file=paste0(wd, data_dir, "lWeather_Monthly.csv"), header = TRUE, sep = ";", dec = ".")

weather_raw.df$RDate_Xlt <- as.POSIXct(weather_raw.df$RDate_Xlt, format="%Y-%m-%d %R")
lweather_daily.df$Date <- as.POSIXct(lweather_daily.df$Date, format="%Y-%m-%d")
lweather_monthly.df$Date <- as.POSIXct(lweather_monthly.df$Date, format="%Y-%m-%d")

}