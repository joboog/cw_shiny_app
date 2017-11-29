#===================================================================================
#===================================================================================
#================= LRB Flow data processing =====================================
#================= Correct Raw hourly data  =====================================
#================= gets called automatically in R_Flow.R !!! =======================


#======== Tasks ====================================================================

# correct raw flow data from 2017 on due to incorrect tip volumes
# tip volumes in process control system are accurate to the first digit only
# tip volume calibration are recored in the file LRB_TipVolumes with an accuracy of two digits


# !!! read in several files
# ideally from data base




### set working directory
#wd <- getwd()
wd <- "E:/UFZ/06 LRB_Data/01 R_Dir/"


#=== 1. Import Raw data and tip volumes from 2017 on =====

# import raw data ==============

RAW.df=ldply(list.files(path=paste0(wd,"/Data_In/LRB_FLOW_RAW_2017/"),pattern="csv",full.names=TRUE),function(filename) {
  dum=read.table(filename, sep=";", dec=",", header=TRUE)
  dum$filename=filename
  return(dum)
})

#RAW.df <- read.csv(paste0(wd,"/Data_In/Flow_Hourly_2017_01.csv"), header=TRUE, sep=";", dec=",")
RAW.df$DateTime <- paste(RAW.df$Date, RAW.df$Time, sep = " ")

# convert time format
RAW.df$RDate_Xlt <-  strptime(as.character(RAW.df$DateTime), format="%d. %m. %Y %R")
RAW.df$RDate <- as.POSIXct(RAW.df$RDate_Xlt)
RAW.df$Date <- as.character(RAW.df$RDate, format = "%m.%Y")


# remove all columns of outflow volume and tipping volume
RAW.df <- select(RAW.df, -contains("_Out_.V."), -contains("_Out_.L."))

# rename inflow columns
colnames(RAW.df) <- gsub("_.L.", "", colnames(RAW.df))
colnames(RAW.df) <- gsub("W.A.", "WA", colnames(RAW.df))
colnames(RAW.df) <- gsub("W.B.", "WB", colnames(RAW.df))

# import tip volumes from ownCloud ============

library(readxl)
TIP.df <- read_excel("E:/ownCloud/LRB - O&M/log book_o & m.xls", 
                     sheet = "OutflowMeasureCalibration_All", na = "empty")

# create a character date vector 
#colnames(TIP.df)[1] <- "RDate"
TIP.df$Date <- as.character(TIP.df$Date, format = "%m.%Y")

#=== 2. Update Flow

# merge RAW.df and TIP.df

RAW.df <- merge(RAW.df, TIP.df, by="Date", all.x=TRUE)



# update outlfow volume =============================
# OUTFLOW_new = NUMBER_OF_TIPS * updated TIP_VOLUME

RAW.df$VA_OUT <- RAW.df$VA_Out_.n. * RAW.df$VA

RAW.df$VAp_OUT <- RAW.df$VAp_Out_.n. * RAW.df$VAp
.
RAW.df$VM1_OUT <- RAW.df$VM1_Out_.n. * RAW.df$VM1

RAW.df$VM2_OUT <- RAW.df$VM2_Out_.n. * RAW.df$VM2

RAW.df$VS1_OUT <- RAW.df$VS1_Out_.n. * RAW.df$VS1

RAW.df$VS1p_OUT <- RAW.df$VS1p_Out_.n. * RAW.df$VS1p

RAW.df$WB_OUT <- RAW.df$WB_Out_.n. * RAW.df$WB

RAW.df$WA_OUT <- RAW.df$WA_Out_.n. * RAW.df$WA

RAW.df$HAp_OUT <- RAW.df$HAp_Out_.n. * RAW.df$HAp

RAW.df$HA_OUT <- RAW.df$HA_Out_.n. * RAW.df$HA

RAW.df$HMc_OUT <- RAW.df$HMc_Out_.n. * RAW.df$HMc

RAW.df$HM_OUT <- RAW.df$HM_Out_.n. * RAW.df$HM

RAW.df$H50p_OUT <- RAW.df$H50p_Out_.n. * RAW.df$H50p

RAW.df$H50_OUT <- RAW.df$H50_Out_.n. * RAW.df$H50

RAW.df$R_OUT <- RAW.df$R_Out_.n. * RAW.df$R
#====================================================


# remove unnecessary columns
RAW.df <- select(RAW.df, -contains("_Out_.n."), -contains("filename"), -contains("RDate_Xlt"))

# select only inflow and outflow and RDate
RAW.df <- select(RAW.df, contains("RDate"), contains("_In"), contains("OUT"))
colnames(RAW.df) <- gsub("In", "IN", colnames(RAW.df))


#=== 3. Export corrected hourly data =======================

flow_hourly.df <- RAW.df

# surpress when using sweave
write.table(flow_hourly.df, file = paste0(wd,"/Data_Out/Flow_hourly_corrected.csv"), sep = ";", dec = ".", na=""  ,row.names = FALSE)

rm(RAW.df)
rm(TIP.df)

