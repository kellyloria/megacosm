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


### End of summer data ###

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

MRS_4Sl <- read.csv(paste0(inputDir, "/2019_Soddie/Soddie_HOBO_2019/SummerRawHOBO/19SOD_4S_20261888_0.csv"), header=T)
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
#rbind: MRS_1He, 
Soddie_l <- rbind(MRS_1Sl, MRS_2Hl, MRS_2Sl, MRS_3Hl, MRS_3Sl, 
                  MRS_4Hl, MRS_4Sl,MRS_5Hl, MRS_5Sl, MRS_6Sl, 
                  MRS_7Hl, MRS_7Sl,MRS_8Sl)
names(MRS_2Hl)

