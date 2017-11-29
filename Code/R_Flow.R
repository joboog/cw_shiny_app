#===================================================================================
#===================================================================================
#================= LRB Flow data processing =====================================
#===================================================================================
#===================================================================================




#======== Tasks ====================================================================

# 1. import data
# 2. summarize data (daily, monthly)
# 3. create plots
# 4. export tdata
# 5. create plots/ summary for defined period





#===================================================================================
#=== Management ====================================================================

### set working directory
#setwd("E:/UFZ/06 LRB_Data/01 R_Dir")

source_dir <- "E:/UFZ/06 LRB_Data/01 R_Dir"
#wd <-"E:/UFZ/06 LRB_Data/01 R_Dir"
# automatically set by project file

### load packages
library(ggplot2)
library(reshape2)
#library(zoo)
#library(FRB)
#library(RColorBrewer)
#library(xtable)

#===================================================================================





#===================================================================================

#=== 1. import corrected flow hourly data ======================

# hourly data
#flow_hourly.df <- read.csv("E:/UFZ/06 LRB_Data/Flow/R_Work_Flow/Data_In/Flow.csv", header=TRUE, sep=";", dec=".")
source(paste0(wd,"/Code/R_FLOW_Correct.R"))
#flow_hourly.df <- read.csv(paste0(wd,"/Data_Out/Flow_Hourly_corrected.csv"), header=TRUE, sep=";", dec=".")

# add POSXlt format
flow_hourly.df$RDate_Xlt <-  as.POSIXlt(flow_hourly.df$RDate)

#wd <- "E:/UFZ/04 Exercises/Exercies_R/Shiny/LRB_Flow"

#=================================================================


#===================================================================================





#=== 2. sumarize data ================================================================



#=== daily flow data ==============================================
# outsource this as a functi0n!

#create a date vector (no time)
flow_hourly.df$Date = strftime(flow_hourly.df$RDate_Xlt, "%d.%m.%Y")

# summary df by subset using plyr package
library(plyr)
i <- which(colnames(flow_hourly.df)=="RDate")
j <- which(colnames(flow_hourly.df)=="RDate_Xlt")
flow_daily.df <- ddply(flow_hourly.df[,-c(i,j)], .(Date),colwise(sum))
rm(i,j)

# create RDate to order df
flow_daily.df$RDate <- strptime(as.character(flow_daily.df$Date), format="%d. %m. %Y")
flow_daily.df <- flow_daily.df[order(flow_daily.df$RDate),]

#row.names(flow_daily.df) <- 1:length(flow_daily.df)
#===================================================================





#=== monthly flow data =============================================


#=== control for precipitation > 4mm/d ===
# read weather data
weather_daily.df <- read.csv(paste0(source_dir,"/Data_Out/Weather_Daily.csv"), header = TRUE, sep = ";")


# set flow values for days with precipitation > 4 mm to NA
i <- which(colnames(weather_daily.df)=="Date")
j <- which(colnames(weather_daily.df)=="P")

df <- merge(flow_daily.df, weather_daily.df[,c(i,j)], by="Date", all.x=TRUE)
df[which(df$P>4),c(2:31)] <- NA  #rep(NA, times=(j-i))


#=== control for zero entries (probably days were no dosing was applied)
# change zero to NA
df[df == 0] <- NA

# now summarize to monthly values =============

#create a year-month date vector (short date)
df$Date_short = strftime(df$RDate, "%Y/%m")
i <- which(colnames(df)=="Date")
j <- which(colnames(df)=="RDate")
k <- which(colnames(df)=="P")
flow_monthly.df <- ddply(df[,-c(i,j,k )], .(Date_short),colwise(mean, na.rm = TRUE))

#===================================================================



# get help file with systems and samplepoints
help.df <- read.csv(paste0(source_dir,"/Data_Management/LRB_FLOW_Help.csv"), header = TRUE, sep = ";")



#=== hourly data =============================================

# convert to long format
lflow_hourly.df <- melt(data=flow_hourly.df, id.vars=c("RDate"), 
                       measure.vars=c(colnames(flow_daily.df)[c(2:31)]), 
                       variable.name="Position")

# add RDate
#lflow_daily.df$RDate <- strptime(lflow_daily.df$Date, format = "%d.%m.%Y")

# merge lflow_monthly.df with help.df
lflow_hourly.df <- merge(lflow_hourly.df, help.df, by.x="Position", by.y = "Flow_Sys")




#=== daily data =============================================

# convert to long format
lflow_daily.df <- melt(data=flow_daily.df, id.vars=c("Date"), 
                         measure.vars=c(colnames(flow_daily.df)[-c(1,32)]), 
                         variable.name="Position")

# add RDate
lflow_daily.df$RDate <- strptime(lflow_daily.df$Date, format = "%d.%m.%Y")

# merge lflow_monthly.df with help.df
lflow_daily.df <- merge(lflow_daily.df, help.df, by.x="Position", by.y = "Flow_Sys")



#=== monthly data =============================================

lflow_monthly.df <- melt(data=flow_monthly.df, id.vars=c("Date_short"), 
                          measure.vars=c(colnames(flow_monthly.df)[-1]), 
                          variable.name="Position")

lflow_monthly.df$Date_short <- paste0(lflow_monthly.df$Date_short, "/01")
lflow_monthly.df$RDate <- strptime(lflow_monthly.df$Date_short, format = "%Y/%m/%d")

# merge lflow_monthly.df with help.df
lflow_monthly.df <- merge(lflow_monthly.df, help.df, by.x="Position", by.y = "Flow_Sys")
