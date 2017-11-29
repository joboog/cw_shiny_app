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
source_dir <-"E:/UFZ/06 LRB_Data/01 R_Dir"


#=== 1. Import Raw data and tip volumes from 2017 on =====

# import raw data ==============

RAW.df=ldply(list.files(path=paste0(source_dir,"/Data_In/LRB_FLOW_RAW/"),pattern="csv",full.names=TRUE),function(filename) {
  dum=read.table(filename, sep=";", dec=",", header=TRUE)
  dum$filename=filename
  return(dum)
})

#RAW.df <- read.csv(paste0(source_dir,"/Data_In/Flow_Hourly_2017_01.csv"), header=TRUE, sep=";", dec=",")
RAW.df$DateTime <- paste(RAW.df$Date, RAW.df$Time, sep = " ")

# convert time format
RAW.df$RDate_Xlt <-  strptime(as.character(RAW.df$DateTime), format="%d. %m. %Y %R")
RAW.df$RDate <- as.POSIXct(RAW.df$RDate_Xlt)
RAW.df$Date <- as.character(RAW.df$RDate, format = "%m.%y")





# import tip volumes from ownCloud ============

library(readxl)
TIP.df <- read_excel("E:/ownCloud/LRB - O&M/log book_o & m.xls", 
                     sheet = "OutflowMeasureCalibration_2017", na = "empty")

# create a character date vector 
#colnames(TIP.df)[1] <- "RDate"
TIP.df$Date <- as.character(TIP.df$Date, format = "%m.%y")






#=== 2. Update Flow

# merge RAW.df and TIP.df

RAW.df <- merge(RAW.df, TIP.df, by="Date", all.x=TRUE)



# update outlfow volume =============================
# OUTFLOW_new = NUMBER_OF_TIPS * updated TIP_VOLUME
RAW.df$VA_IN <- RAW.df$VA_In_.L.
RAW.df$VA_OUT <- RAW.df$VA_Out_.n. * RAW.df$VA

RAW.df$VAp_IN <- RAW.df$VAp_In_.L.
RAW.df$VAp_OUT <- RAW.df$VAp_Out_.n. * RAW.df$VAp

RAW.df$VM1_IN <- RAW.df$VM1_In_.L.
RAW.df$VM1_OUT <- RAW.df$VM1_Out_.n. * RAW.df$VAp

RAW.df$VM2_IN <- RAW.df$VM2_In_.L.
RAW.df$VM2_OUT <- RAW.df$VM2_Out_.n. * RAW.df$VAp

RAW.df$VS1_IN <- RAW.df$VS1_In_.L.
RAW.df$VS1_OUT <- RAW.df$VS1_Out_.n. * RAW.df$VAp

RAW.df$VS1p_IN <- RAW.df$VS1p_In_.L.
RAW.df$VS1p_OUT <- RAW.df$VS1p_Out_.n. * RAW.df$VAp

RAW.df$W.B._IN <- RAW.df$W.B._In_.L.
RAW.df$W.B._OUT <- RAW.df$W.B._Out_.n. * RAW.df$VAp

RAW.df$W.A._IN <- RAW.df$W.A._In_.L.
RAW.df$W.A._OUT <- RAW.df$W.A._Out_.n. * RAW.df$VAp

RAW.df$HAp_IN <- RAW.df$HAp_In_.L.
RAW.df$HAp_OUT <- RAW.df$HAp_Out_.n. * RAW.df$VAp

RAW.df$HA_IN <- RAW.df$HA_In_.L.
RAW.df$HA_OUT <- RAW.df$HA_Out_.n. * RAW.df$VAp

RAW.df$HMc_IN <- RAW.df$HMc_In_.L.
RAW.df$HMc_OUT <- RAW.df$HMc_Out_.n. * RAW.df$VAp

RAW.df$HM_IN <- RAW.df$HM_In_.L.
RAW.df$HM_OUT <- RAW.df$HM_Out_.n. * RAW.df$VAp

RAW.df$H50p_IN <- RAW.df$H50p_In_.L.
RAW.df$H50p_OUT <- RAW.df$H50p_Out_.n. * RAW.df$VAp

RAW.df$H50_IN <- RAW.df$H50_In_.L.
RAW.df$H50_OUT <- RAW.df$H50_Out_.n. * RAW.df$VAp

RAW.df$R_IN <- RAW.df$R_In_.L.
RAW.df$R_OUT <- RAW.df$R_Out_.n. * RAW.df$VAp
#====================================================




#=== 3. Export corrected hourly data =======================

i <- which(colnames(RAW.df)=="RDate")
j <- which(colnames(RAW.df)=="VA_IN")
k <- which(colnames(RAW.df)=="R_OUT")

flow_hourly.df <- RAW.df[,c(i, j:k)]

# surpress when using sweave
write.table(flow_hourly.df, file = paste0(source_dir,"/Data_Out/Flow_hourly_corrected.csv"), sep = ";", dec = ".", na=""  ,row.names = FALSE)

rm(RAW.df)
rm(TIP.df)
rm(i)
rm(j)
rm(k)

