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


# WQ Data ===================================================================

## Import data
source("E:/UFZ/06 LRB_Data/01 R_Dir/Code/LRB_ImportData_classic WQ_Conc.R")

## Pre-processing ===

# load preprocessing script
source("E:/UFZ/06 LRB_Data/01 R_Dir/Code/LRB_Preprocessing_classic WQ_Conc.R")
rm(BDL.dat)

# add log_Ecoli
Raw.dat$log_Ecoli <- log10(Raw.dat$Ecoli)

# remove complete NA rows
Raw.dat <- Raw.dat[-which((is.na(Raw.dat$T_lab)) & (is.na(Raw.dat$T_field)) & (is.na(Raw.dat$EC)) & (is.na(Raw.dat$DO)) & (is.na(Raw.dat$ORP)) & (is.na(Raw.dat$pH)) & (is.na(Raw.dat$CBOD)) & (is.na(Raw.dat$TOC)) & (is.na(Raw.dat$TN)) & (is.na(Raw.dat$DOC)) & (is.na(Raw.dat$DN)) & (is.na(Raw.dat$IC)) & (is.na(Raw.dat$DIC)) & (is.na(Raw.dat$NH4N)) & (is.na(Raw.dat$NO3N)) & (is.na(Raw.dat$NO2N)) & (is.na(Raw.dat$TSS)) & (is.na(Raw.dat$Ecoli)) & (is.na(Raw.dat$log_Ecoli))),]


### Convert WQ data to long format ===

# seperate in data for field parameters, carbon, nitrogen, others
# set variable names for factors
factors.par <- c("DateTime",  "SystemType", "FlowDirection","System", "SampleType","SamplePoint")

# set variables for Parameters groups
parameters <- colnames(Raw.dat)[-c(1:9)]

# convert data
lRaw.dat <- melt(data=Raw.dat, id.vars=factors.par, 
                 measure.vars= parameters, 
                 variable.name="Parameter")

# remove unnecessary stuff
rm(SPoint.dat, DL,factors.par, m,parameters)


# Flow Data ================================================================

# load source script for summary table
source(file="E:/UFZ/10 Programs/R- Project/R_Work_def/Support.R")

source(file = paste0(wd,"/Code/R_Flow.R"))

# remove unnecessary data
rm(df, flow_daily.df, flow_hourly.df, flow_monthly.df, help.df, weather_daily.df, 
   i,j,k)

# convert all Xlt time formats to Ct
lflow_daily.df$RDate <- as.POSIXct(lflow_daily.df$RDate)
lflow_monthly.df$RDate <- as.POSIXct(lflow_monthly.df$RDate)



# Weather Data ==============================================================

source(file =paste0(wd, "/Code/R_Weather.R"))
rm(weather_daily.df, weather_monthly.df)
weather_raw.df$RDate_Xlt <- as.POSIXct(weather_raw.df$RDate_Xlt)
