# LRB weather data

# preambel

source_dir <- "E:/UFZ/06 LRB_Data/01 R_Dir"

# import data ==========================================================

# import raw data from yearly 10min files

weather_raw.df=ldply(list.files(path=paste0(source_dir,"/Data_In/LRB_WEATHER_10min/"),pattern="csv",full.names=TRUE),function(filename) {
  dum=read.table(filename, sep=";", dec=".", header=TRUE)
  dum$filename=filename
  return(dum)
})

# import raw data from monthly files (from 2017 on)
weather_raw.df2=ldply(list.files(path=paste0(source_dir,"/Data_In/LRB_WEATHER_RAW/"),pattern="csv",full.names=TRUE),function(filename) {
  dum=read.table(filename, sep=";", dec=",", header=TRUE)
  dum$filename=filename
  return(dum)
})

# change to character class
weather_raw.df$Datum.Zeit <- as.character(weather_raw.df$Datum.Zeit)
weather_raw.df2$Datum.Zeit <- as.character(weather_raw.df2$Datum.Zeit)

# combine two data frames
weather_raw.df <- rbind(weather_raw.df, weather_raw.df2)
rm(weather_raw.df2)

weather_raw.df <- weather_raw.df[,1:11]

colnames(weather_raw.df) <- c("DateTime", "T_air_mean", "humidity_rel", "wind_speed_mean", "wind_speed_max", "wind_direction",
                              "radiation", "pressure_air", "P", "ET_haude", "ET_penman")

weather_raw.df$T_air_min <- weather_raw.df$T_air_mean
weather_raw.df$T_air_max <- weather_raw.df$T_air_mean



#=== daily weather data ================================================

# create a date vector (no time)
weather_raw.df$RDate_Xlt <- strptime(weather_raw.df$DateTime, format="%d. %m. %Y %R")
weather_raw.df$Date = strftime(weather_raw.df$RDate_Xlt, "%d.%m.%Y")


# summary df by subset using plyr package
library(plyr)
i <- which(colnames(weather_raw.df)=="DateTime")
j <- which(colnames(weather_raw.df)=="RDate_Xlt")


# now subset, ! some values will be summarized by mean, others by sum, min ,max
weather_daily.mean <- ddply(weather_raw.df[,c(15, 2:4, 6:8 )], .(Date),colwise(mean))
weather_daily.sum <- ddply(weather_raw.df[,c(15, 9:11)], .(Date),colwise(sum))
weather_daily.max <- ddply(weather_raw.df[,c(15, 5,13)], .(Date),colwise(max))
weather_daily.min<- ddply(weather_raw.df[,c(15, 12)], .(Date),colwise(min))
rm(i,j)


# now merge all df
weather_daily.df <- merge(weather_daily.mean, weather_daily.sum, by="Date")
weather_daily.df <- merge(weather_daily.df, weather_daily.max, by="Date")
weather_daily.df <- merge(weather_daily.df, weather_daily.min, by="Date")
rm(weather_daily.max, weather_daily.mean, weather_daily.min, weather_daily.sum)



# create RDate to order df
weather_daily.df$RDate <- strptime(as.character(weather_daily.df$Date), format="%d. %m. %Y")


# order rows
weather_daily.df <- weather_daily.df[order(weather_daily.df$RDate),]


# order columns
weather_daily.df <- weather_daily.df[,c("RDate", "Date","T_air_mean", "T_air_min", "T_air_max",
                                        "humidity_rel", "wind_speed_mean", "wind_speed_max", "wind_direction",
                                        "radiation", "pressure_air", "P", "ET_haude", "ET_penman" )]

# remove outliers
weather_daily.df$wind_direction[which(weather_daily.df$wind_direction==max(na.omit(weather_daily.df$wind_direction)))] <- NA
weather_daily.df$pressure_air[which(weather_daily.df$pressure_air==max(na.omit(weather_daily.df$pressure_air)))] <- NA
weather_daily.df$pressure_air[which(weather_daily.df$pressure_air==max(na.omit(weather_daily.df$pressure_air)))] <- NA
weather_daily.df$P[which(weather_daily.df$P==max(na.omit(weather_daily.df$P)))] <- NA
weather_daily.df$ET_haude[which(weather_daily.df$ET_haude==max(na.omit(weather_daily.df$ET_haude)))] <- NA

# transform to long format
lweather_daily.df <- melt(data=weather_daily.df, id.vars=c("Date"), 
                          measure.vars=c(colnames(weather_daily.df)[-c(1,2,15)]), 
                          variable.name="Parameter")

lweather_daily.df$Date <- as.POSIXct(strptime(lweather_daily.df$Date, format = "%d. %m. %Y"))


#=== monthly data ======================================================
# outsource this as a functin!

#create a year-month date vector (short date)
weather_daily.df$Date_short = strftime(weather_daily.df$RDate, "%Y/%m")

#===summarize data to vector ==
T_air_mean <- aggregate(weather_daily.df$T_air_mean ~ weather_daily.df$Date_short, FUN = mean)
T_air_min <- aggregate(weather_daily.df$T_air_min ~ weather_daily.df$Date_short, FUN = min)[,2]
T_air_max <- aggregate(weather_daily.df$T_air_max ~ weather_daily.df$Date_short, FUN = max)[,2]
humidity_rel <- aggregate(weather_daily.df$humidity_rel ~ weather_daily.df$Date_short, FUN = mean)[,2]
wind_speed_mean <- aggregate(weather_daily.df$wind_speed_mean ~ weather_daily.df$Date_short, FUN = mean)[,2]
wind_speed_max <- aggregate(weather_daily.df$wind_speed_max ~ weather_daily.df$Date_short, FUN = max)[,2]
wind_direction <- aggregate(weather_daily.df$wind_direction ~ weather_daily.df$Date_short, FUN = mean)[,2]
radiation <- aggregate(weather_daily.df$radiation ~ weather_daily.df$Date_short, FUN = mean)[,2]
pressure_air <- aggregate(weather_daily.df$pressure_air ~ weather_daily.df$Date_short, FUN = mean)[,2]
P <- aggregate(weather_daily.df$P ~ weather_daily.df$Date_short, FUN = sum)[,2]
ET_haude <- aggregate(weather_daily.df$ET_haude ~ weather_daily.df$Date_short, FUN = sum)[,2]
ET_penman <- aggregate(weather_daily.df$ET_penman ~ weather_daily.df$Date_short, FUN = sum)[,2]

weather_monthly.df <- data.frame(T_air_mean, T_air_min, T_air_max,humidity_rel,
                                 wind_speed_mean, wind_speed_max, wind_direction,
                                 radiation,
                                 pressure_air,
                                 P, ET_haude, ET_penman)

colnames(weather_monthly.df)[1] <- "Date"
colnames(weather_monthly.df)[2] <- "T_air_mean"

rm(T_air_mean, T_air_min, T_air_max,humidity_rel,
   wind_speed_mean, wind_speed_max, wind_direction,
   radiation,
   pressure_air,
   P, ET_haude, ET_penman)


# transform to long format
lweather_monthly.df <- melt(data=weather_monthly.df, id.vars=c("Date"), 
                            measure.vars=c(colnames(weather_monthly.df)[-c(1,14:15)]), 
                            variable.name="Parameter")

lweather_monthly.df$Date <-  as.POSIXct(paste0(as.character(lweather_monthly.df[,1]),"/01"), format = "%Y/%m/%d")
