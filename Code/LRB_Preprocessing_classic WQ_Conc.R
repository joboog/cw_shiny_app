#==============================================================================
#== LRB Data Management  ======================================================
#==============================================================================

### Johannes Boog



# === Data Preprocessing -- for raw wq data (concentrations+field meas) =======


# =============================================================================

# === 2a. Handle BDL, OMR =====================================================
#==============================================================================

### convert factor column to character
#Raw.dat$CBOD <- as.character(Raw.dat$CBOD)

# load detection limits
BDL.dat <- read.csv("E:/UFZ/06 LRB_Data/01 R_Dir/Data_Management/LRB_DetectionLimits.csv", header = TRUE, sep=";")

# get column numbers to reclassify
m <- which(colnames(Raw.dat)=="T_lab")
n <- which(colnames(Raw.dat)=="Ecoli")

# substitue with zero (add BDL values) and convert tu numeric
# make sure no other string is in the columns than "BDL" and "OMR"
for (i in m:n){
  if (class(Raw.dat[,i]) =="factor" | class(Raw.dat[,i]) =="character" ){
    
    # reclassify
    Raw.dat[,i] <- as.character(Raw.dat[,i])
    
    # get DetectionLimit from BDL file
    DL <- BDL.dat[which(colnames(BDL.dat)==colnames(Raw.dat[i]))] 
    
    # add detection limit to WQ data
    Raw.dat[,i][Raw.dat[,i]=="BDL"] <- DL[1,1]
    
    # remove OMR
    Raw.dat[,i][Raw.dat[,i]=="OMR"] <- NA
    
    # convert tu numeric
    Raw.dat[,i] <- as.numeric(Raw.dat[,i])
  }
}


#==============================================================================