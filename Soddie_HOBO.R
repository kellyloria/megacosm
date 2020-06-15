## ---------------------------
## QA'QC for 2019 Soddie "Megacosm" Pilot: 
##  HOBO Logger Data
##
## Author: Kelly A. Loria
## Date Created: 2020-03-19
## Email: kelly.loria@colorado.edu
##
## ---------------------------
## Load packages:
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(zoo)

## ---------------------------
# File path setup:
if (dir.exists('/Users/kellyloria/Documents/Niwot\ LTER\ 2017-2019/Mesocosm/MegacomsPilot/')){
  inputDir<- '/Users/kellyloria/Documents/Niwot\ LTER\ 2017-2019/Mesocosm/MegacomsPilot/'
  outputDir<- '/Users/kellyloria/Desktop/' 
}

## ---------------------------
# Individual tanks: Soddie 1
MRS_1He <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_1H_biege_20483163Q.csv"), header=T)
names(MRS_1He)

# 2. Fix timestamp
MRS_1He$timestamp <- as.POSIXct(MRS_1He$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_1He$timestamp)

# 3. Restrict for date range of deployment:
MRS_1He <- subset(MRS_1He,timestamp >= as.POSIXct('0018-10-22 08:00:00') & 
                  timestamp <= as.POSIXct('0019-07-03 00:00:00'))
range(MRS_1He$timestamp)

qplot(timestamp, TempC, data = MRS_1He, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

MRS_1Se <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_1S_biege_20483162_Q.csv"), header=T)
names(MRS_1Se)
MRS_1Se$timestamp <- as.POSIXct(MRS_1Se$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_1He$timestamp)


MRS_2Se <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_2S_black_20483164_Q.csv"), header=T)
names(MRS_2Se)
MRS_2Se$timestamp <- as.POSIXct(MRS_2Se$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_2Se$timestamp)

MRS_2He <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_2H_black_20483165_Q.csv"), header=T)
names(MRS_2He)
# 2. Fix timestamp
MRS_2He$timestamp <- as.POSIXct(MRS_2He$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_2He$timestamp)

MRS_3He <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_3H_biege_large_20483173_Q.csv"), header=T)
names(MRS_3He)
# 2. Fix timestamp
MRS_3He$timestamp <- as.POSIXct(MRS_3He$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_3He$timestamp)

MRS_3Se <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_3S_biege_large_20483172_Q.csv"), header=T)
names(MRS_3Se)
MRS_3Se$timestamp <- as.POSIXct(MRS_3Se$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_3Se$timestamp)

MRS_4He <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_4H_black_20483169_Q.csv"), header=T)
names(MRS_4He)
# 2. Fix timestamp
MRS_4He$timestamp <- as.POSIXct(MRS_4He$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_4He$timestamp)

MRS_4Se <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_4S_black_20483168_Q.csv"), header=T)
names(MRS_4Se)
MRS_4Se$timestamp <- as.POSIXct(MRS_4Se$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_4Se$timestamp)

MRS_5He <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_5H_biege_20483167_Q.csv"), header=T)
names(MRS_5He)
# 2. Fix timestamp
MRS_5He$timestamp <- as.POSIXct(MRS_5He$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_5He$timestamp)

MRS_5Se <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_5S_beige_20483166_Q.csv"), header=T)
names(MRS_5Se)
MRS_5Se$timestamp <- as.POSIXct(MRS_5Se$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_5Se$timestamp)

MRS_6He <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_6H_black_large_20483171_Q.csv"), header=T)
names(MRS_6He)
# 2. Fix timestamp
MRS_6He$timestamp <- as.POSIXct(MRS_6He$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_6He$timestamp)

MRS_6Se <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_6S_black_large_20483170_Q.csv"), header=T)
names(MRS_6Se)
# 2. Fix timestamp
MRS_6Se$timestamp <- as.POSIXct(MRS_6Se$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_6Se$timestamp)

MRS_7He <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_7H_biege_20483175_Q.csv"), header=T)
names(MRS_7He)
# 2. Fix timestamp
MRS_7He$timestamp <- as.POSIXct(MRS_7He$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_7He$timestamp)

MRS_7Se <- read.csv(paste0(inputDir, "2019_Soddie/Soddie_HOBO_2019/MRS_7S_biege_20483174_Q.csv"), header=T)
names(MRS_7Se)
# 2. Fix timestamp
MRS_7Se$timestamp <- as.POSIXct(MRS_7Se$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_7Se$timestamp)

MRS_8He <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_8H_black_20483177_Q.csv"), header=T)
names(MRS_8He)
# 2. Fix timestamp
MRS_8He$timestamp <- as.POSIXct(MRS_8He$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_8He$timestamp)

MRS_8Se <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/MRS_8S_black_20483176_Q.csv"), header=T)
names(MRS_8Se)
# 2. Fix timestamp
MRS_8Se$timestamp <- as.POSIXct(MRS_8Se$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_8Se$timestamp)


## ---------------------------
# Individual tanks: Soddie 1
MRS_1Hl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_1H_20261879.csv"), header=T)
names(MRS_1Hl)

# 2. Fix timestamp
MRS_1Hl$timestamp <- as.POSIXct(MRS_1Hl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_1Hl$timestamp)

# 3. Restrict for date range of deployment:
MRS_1Hl <- subset(MRS_1Hl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_1Hl$timestamp)

qplot(timestamp, TempC, data = MRS_1Hl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

MRS_1Sl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_1S_20261880.csv"), header=T)
names(MRS_1Sl)

# 2. Fix timestamp
MRS_1Sl$timestamp <- as.POSIXct(MRS_1Sl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_1Sl$timestamp)

# 3. Restrict for date range of deployment:
MRS_1Sl <- subset(MRS_1Sl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_1Sl$timestamp)

qplot(timestamp, TempC, data = MRS_1Sl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

## ---------------------------
# Individual tanks: Soddie 2
MRS_2Hl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_2H_20261881.csv"), header=T)
names(MRS_2Hl)

# 2. Fix timestamp
MRS_2Hl$timestamp <- as.POSIXct(MRS_2Hl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_2Hl$timestamp)

# 3. Restrict for date range of deployment:
MRS_2Hl <- subset(MRS_2Hl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_2Hl$timestamp)

qplot(timestamp, TempC, data = MRS_2Hl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

MRS_2Sl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_2S_20261882.csv"), header=T)
names(MRS_2Sl)

# 2. Fix timestamp
MRS_2Sl$timestamp <- as.POSIXct(MRS_2Sl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_2Sl$timestamp)

# 3. Restrict for date range of deployment:
MRS_2Sl <- subset(MRS_2Sl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_2Sl$timestamp)

qplot(timestamp, TempC, data = MRS_2Sl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

## ---------------------------
# Individual tanks: Soddie 3
MRS_3Hl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_3H_20261885.csv"), header=T)
names(MRS_3Hl)

# 2. Fix timestamp
MRS_3Hl$timestamp <- as.POSIXct(MRS_3Hl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_3Hl$timestamp)

# 3. Restrict for date range of deployment:
MRS_3Hl <- subset(MRS_3Hl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_3Hl$timestamp)

qplot(timestamp, TempC, data = MRS_3Hl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

MRS_3Sl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_3S_20261896.csv"), header=T)
names(MRS_3Sl)

# 2. Fix timestamp
MRS_3Sl$timestamp <- as.POSIXct(MRS_3Sl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_3Sl$timestamp)

# 3. Restrict for date range of deployment:
MRS_3Sl <- subset(MRS_3Sl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_3Sl$timestamp)

qplot(timestamp, TempC, data = MRS_3Sl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


## ---------------------------
# Individual tanks: Soddie 4
MRS_4Hl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_4H_20261887.csv"), header=T)
names(MRS_4Hl)

# 2. Fix timestamp
MRS_4Hl$timestamp <- as.POSIXct(MRS_4Hl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_4Hl$timestamp)

# 3. Restrict for date range of deployment:
MRS_4Hl <- subset(MRS_4Hl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_4Hl$timestamp)

qplot(timestamp, TempC, data = MRS_4Hl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

MRS_4Sl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_4S_20261888.csv"), header=T)
names(MRS_4Sl)

# 2. Fix timestamp
MRS_4Sl$timestamp <- as.POSIXct(MRS_4Sl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_4Sl$timestamp)

# 3. Restrict for date range of deployment:
MRS_4Sl <- subset(MRS_4Sl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_4Sl$timestamp)

qplot(timestamp, TempC, data = MRS_4Sl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

## ---------------------------
# Individual tanks: Soddie 5
MRS_5Hl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_5H_20261889.csv"), header=T)
names(MRS_5Hl)

# 2. Fix timestamp
MRS_5Hl$timestamp <- as.POSIXct(MRS_5Hl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_5Hl$timestamp)

# 3. Restrict for date range of deployment:
MRS_5Hl <- subset(MRS_5Hl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_5Hl$timestamp)

qplot(timestamp, TempC, data = MRS_5Hl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

MRS_5Sl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_5S_20261890.csv"), header=T)
names(MRS_5Sl)

# 2. Fix timestamp
MRS_5Sl$timestamp <- as.POSIXct(MRS_5Sl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_5Sl$timestamp)

# 3. Restrict for date range of deployment:
MRS_5Sl <- subset(MRS_5Sl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_5Sl$timestamp)

qplot(timestamp, TempC, data = MRS_5Sl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


## ---------------------------
# Individual tanks: Soddie 6
# Hypo sensor died

MRS_6Sl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_6S_20261892.csv"), header=T)
names(MRS_6Sl)

# 2. Fix timestamp
MRS_6Sl$timestamp <- as.POSIXct(MRS_6Sl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_6Sl$timestamp)

# 3. Restrict for date range of deployment:
MRS_6Sl <- subset(MRS_6Sl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_6Sl$timestamp)

qplot(timestamp, TempC, data = MRS_6Sl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

## ---------------------------
# Individual tanks: Soddie 7
MRS_7Hl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_7H_20261893.csv"), header=T)
names(MRS_7Hl)

# 2. Fix timestamp
MRS_7Hl$timestamp <- as.POSIXct(MRS_7Hl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_7Hl$timestamp)

# 3. Restrict for date range of deployment:
MRS_7Hl <- subset(MRS_7Hl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_7Hl$timestamp)

qplot(timestamp, TempC, data = MRS_7Hl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

MRS_7Sl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_7S_20261894.csv"), header=T)
names(MRS_7Sl)

# 2. Fix timestamp
MRS_7Sl$timestamp <- as.POSIXct(MRS_7Sl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_7Sl$timestamp)

# 3. Restrict for date range of deployment:
MRS_7Sl <- subset(MRS_7Sl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_7Sl$timestamp)

qplot(timestamp, TempC, data = MRS_7Sl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

## ---------------------------
# Individual tanks: Soddie 8
MRS_8Hl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_8H_20261895.csv"), header=T)
names(MRS_8Hl)

# 2. Fix timestamp
MRS_8Hl$timestamp <- as.POSIXct(MRS_8Hl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_8Hl$timestamp)

# 3. Restrict for date range of deployment:
MRS_8Hl <- subset(MRS_8Hl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_8Hl$timestamp)

qplot(timestamp, TempC, data = MRS_8Hl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

MRS_8Sl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_8S_20261886.csv"), header=T)
names(MRS_8Sl)

# 2. Fix timestamp
MRS_8Sl$timestamp <- as.POSIXct(MRS_8Sl$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(MRS_8Sl$timestamp)

# 3. Restrict for date range of deployment:
MRS_8Sl <- subset(MRS_8Sl,timestamp >= as.POSIXct('0019-07-02 14:00:00') & 
                    timestamp <= as.POSIXct('0019-08-18 00:00:00'))
range(MRS_7Sl$timestamp)

qplot(timestamp, TempC, data = MRS_8Sl, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))



## ---------------------------
# Aggregate data: 
Soddie_l <- rbind(MRS_1Hl, MRS_1Sl, MRS_2Hl, MRS_2Sl, MRS_3Hl, MRS_3Sl, 
                  MRS_4Hl, MRS_4Sl,MRS_5Hl, MRS_5Sl, MRS_6Sl, 
                  MRS_7Hl, MRS_7Sl,MRS_8Sl)
summary(Soddie_l)

Soddie_e <- rbind(MRS_1He, MRS_1Se, MRS_2He, MRS_2Se, MRS_3He, MRS_3Se,
                  MRS_4He, MRS_4Se, MRS_5He, MRS_5Se, MRS_6He, MRS_6Se,
                  MRS_7He, MRS_7Se, MRS_8He, MRS_8Se)

Soddie <- rbind(Soddie_e, Soddie_l)
summary(Soddie_e)
summary(Soddie$Tank)

## ---------------------------
# QAQC data: 

# 1. Flag temperature values:
Soddie.Q=Soddie %>% 
  mutate(temperature=ifelse(TempC>35, NA, TempC)) %>%
  mutate(hour=lubridate::hour(timestamp))%>%
  arrange(Tank, Position, timestamp)%>%
  group_by(Tank, Position, timestamp)%>% #this will get the nearest 15, but could be fewer if some are missing OR >35C, I think (?) the 35 are bogus so that is ok but you could
  mutate(mnT=rollapply(temperature, width = 15, FUN = mean, fill=NA),           # also filter out the NAs and >35s if you wanted to always have 15 values in your rolling window after removing bad values
         sdT=rollapply(temperature, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loT=mnT- (3*sdT), hiT=mnT+ (3*sdT))%>%
  mutate(mnL=rollapply(IntensityLux, width = 15, FUN = mean, fill=NA),           # also filter out the NAs and >35s if you wanted to always have 15 values in your rolling window after removing bad values
         sdL=rollapply(IntensityLux, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loL=mnL- (3*sdL), hiL=mnL+ (3*sdL))%>%
  full_join(., Soddie)%>% 
  mutate(
    flag_temperature=
      case_when( #may as well add the m in here since your metadata days that flag was used
        is.na(temperature) ~ 'm',
        temperature>35 ~ 'q',
        temperature<loT&!is.na(loT) ~ 'o',
        temperature>hiT&!is.na(hiT) ~ 'o',
        temperature<0 ~ 'q', TRUE ~ 'n')) %>%
  mutate(
    flag_Lux=
      case_when( #may as well add the m in here since your metadata days that flag was used
        is.na(IntensityLux) ~ 'm',
        IntensityLux<loL&!is.na(loL) ~ 'o',
        IntensityLux>hiL&!is.na(hiL) ~ 'o',
        IntensityLux<0 ~ 'q', TRUE ~ 'n'))

# 2.Check the QAQC
p <- ggplot(Soddie.Q, aes(x=timestamp, y=(temperature), colour =as.factor(flag_temperature), shape= Position)) +
  geom_point(alpha = 0.7)  +
  theme_classic() + facet_wrap(~Tank)

p2 <- ggplot(Soddie.Q, aes(x=timestamp, y=(IntensityLux), colour =as.factor(flag_Lux), shape= Position)) +
  geom_point(alpha = 0.7)  +
  theme_classic() + facet_wrap(~Tank)

names(Soddie.Q)

# 3. Remove unwanted variables:
Soddie.Q2 <- subset(Soddie.Q, select=c(Tank, timestamp, temperature, IntensityLux,
                                             Sensor, Position, Color, flag_temperature, flag_Lux))

# 4. Double chec for duplicated values:
Soddie.Q2%>%select(Tank, Position, timestamp)%>%duplicated()%>%sum() # 2 dups

View(Soddie.Q2%>%
       inner_join(
         Soddie.Q2 %>%
           group_by(Tank, Position, timestamp) %>%
           summarize(ct=dplyr::n())%>% filter(ct>1)))

# Remove values:
Soddie.Q3 = Soddie.Q2 %>%
  distinct(Tank, Position, timestamp, .keep_all = TRUE)

Soddie.Q3%>%select(Tank, Position, timestamp)%>%duplicated()%>%sum() 

# 5. Export and save data:
# write.csv(Soddie.Q3, paste0(outputDir, "SoddieHOBO_Q.csv")) # complied data file of all DO sensors along buoy line

