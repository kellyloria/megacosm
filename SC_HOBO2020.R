## ---------------------------
## QA'QC for 2020 Sandy Corner (SC) "Megacosm" Pilot: 
##  Zooplankton Community Comp Data Logger Data
##
## Author: Kelly A. Loria
## Date Created: 2020-06-22
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
if (dir.exists('/Users/kellyloria/Documents/Niwot\ LTER\ 2017-2019/Mesocosm/')){
  inputDir<- '/Users/kellyloria/Documents/Niwot\ LTER\ 2017-2019/Mesocosm/'
  outputDir<- '/Users/kellyloria/Desktop/' 
}

## ---------------------------
# Read in data and fix timestamp start tanks 1-20

# A Block #1-4:
A1H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank1_H_20261875.csv"), header=T)
A1H$timestamp <- as.POSIXct(A1H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(A1H$timestamp)
# Restrict for date range of deployment:
A1H <- subset(A1H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                    timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(A1H$timestamp)

qplot(timestamp, TempC, data = A1H, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

A1S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank1_S_20483158.csv"), header=T)
A1S$timestamp <- as.POSIXct(A1S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(A1S$timestamp)
# Restrict for date range of deployment:
A1S <- subset(A1S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(A1S$timestamp)

qplot(timestamp, TempC, data = A1S, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

##
A2H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank2_H_2026187.csv"), header=T)
A2H$timestamp <- as.POSIXct(A2H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(A2H$timestamp)
# Restrict for date range of deployment:
A2H <- subset(A2H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(A2H$timestamp)

A2S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank2_S_20483159.csv"), header=T)
A2S$timestamp <- as.POSIXct(A2S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(A2S$timestamp)
# Restrict for date range of deployment:
A2S <- subset(A2S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(A2S$timestamp)

##
A3H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank3_H_20261877.csv"), header=T)
A3H$timestamp <- as.POSIXct(A3H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(A3H$timestamp)
# Restrict for date range of deployment:
A3H <- subset(A3H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(A3H$timestamp)

A3S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank3_S_20483160.csv"), header=T)
A3S$timestamp <- as.POSIXct(A3S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(A3S$timestamp) # issue with battery failure in April 2020

##
##
A4H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank4_H_20261878.csv"), header=T)
A4H$timestamp <- as.POSIXct(A4H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(A4H$timestamp)
# Restrict for date range of deployment:
A4H <- subset(A4H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(A4H$timestamp)

A4S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank4_S_20483161.csv"), header=T)
A4S$timestamp <- as.POSIXct(A4S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(A4S$timestamp)
# Restrict for date range of deployment:
A4S <- subset(A4S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(A4S$timestamp)





## ---------------------------
# Aggregate logger data

SandCorn <- rbind(A1H, A1S, A2H, A2S, A3H, A3S, A4H, A4S)





