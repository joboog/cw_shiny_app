#==============================================================================
#== LRB Data Management -- Data Import for classic LRB WQ =====================
#==============================================================================

### Johannes Boog


#==============================================================================
#== 1. import data ============================================================


### Notes:
# Prior to script execution make sure to
# preprocessed data in excel
# look for BDL, OMR, NMR, <0.23
# especially or CBOD, Turbidity, NO3N, NO2N, Ecoli
# unhibit setwd command


#===




# load raw data  
#Raw.dat <- read.csv(file.choose(), header = TRUE, sep=";")
Raw.dat <- read.csv("E:/UFZ/06 LRB_Data/01 R_Dir/Data_In/LRB_RawData2.csv", header = TRUE, sep=";")


# load sample point information data
# add pore water sampling point coordinates, system type, flow direction etc
SPoint.dat <- read.csv("E:/UFZ/06 LRB_Data/01 R_Dir/Data_Management/LRB_SamplePoint.csv", header=TRUE, sep=";")
Raw.dat <- merge(Raw.dat, SPoint.dat, by="SamplePoint")
# still have to change the script for internal samples!!!


#rename columns
colnames(Raw.dat) <- c("SamplePoint","Experiment","Date", "Time",
                       "T_lab","EC","DO","ORP","pH","T_field",
                       "CBOD","TOC", "TN", "IC", "DOC", "DN","DIC", "NH4N","NO3N","NO2N", "Ecoli", "TSS",
                       "System", "frac_dist", "SystemType", "SampleType", "FlowDirection") 


#View(Raw.dat) 







#==============================================================================
#== 1b. Convert time format and order data =====================================

# combine Date and Time to  datetime
Raw.dat$DateTime <- paste(Raw.dat$Date, Raw.dat$Time, sep = " ") 

# convert datetime to R specific datetime vector
Raw.dat$DateTime <-  strptime(Raw.dat$DateTime, format="%d. %m. %Y %R")
class(Raw.dat$RDate)

# convert into POSIXct
# this format stores the date is continuous number,
# thus it can be used for calculations later on 
Raw.dat$DateTime <- as.POSIXct(Raw.dat$DateTime)

# change time format of RStudio
Sys.setlocale(category = "LC_ALL", locale = "english")

#==============================================================================

# reorder columns
Raw.dat <- Raw.dat[,c(  "SystemType","System", "FlowDirection","SampleType", "SamplePoint", "frac_dist","Date", "Time", "DateTime",
                        "T_lab","T_field","EC","ORP","pH", "DO",
                        "CBOD","TOC", "DOC", "IC", "DIC","TN", "DN", "NH4N","NO3N","NO2N",
                        "TSS","Ecoli")]

# reorder factor levels of SystemType
Raw.dat$SystemType <- factor(Raw.dat$SystemType, levels = c("Pretreatment", "SoilTreatment", "Intensified", "Conventional" ))
Raw.dat$System <- factor(Raw.dat$System, levels = c("SEP", "WA","WB","H50p", "H50", "VSp","HAp", "HA", "HMc", "HM",  "VAp", "VA", "VM1", "VM2","R"))


# save levels of SamplePoint from listing in data frame
# according to the subsequent unique appearance
# of SamplePoint names  
# reorder SamplePoint levels 
SamplePoint.levels <- unique(Raw.dat$SamplePoint)

# order rows according to RDateTime, SamplePoint and SampleType
Raw.dat <- Raw.dat[order(Raw.dat$SystemType, Raw.dat$System, Raw.dat$SamplePoint, Raw.dat$Date),]
