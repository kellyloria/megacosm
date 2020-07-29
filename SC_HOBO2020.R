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
library(lmerTest)
library(lme4)

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

##B block
# B Block #5-8:
B5H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank5_H_20261879.csv"), header=T)
B5H$timestamp <- as.POSIXct(B5H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(B5H$timestamp)
# Restrict for date range of deployment:
B5H <- subset(B5H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(B5H$timestamp)

B5S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank5_S_20483162.csv"), header=T)
B5S$timestamp <- as.POSIXct(B5S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(B5S$timestamp)
# Restrict for date range of deployment:
B5S <- subset(B5S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(B5S$timestamp)

##
B6H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank6_H_20261880.csv"), header=T)
B6H$timestamp <- as.POSIXct(B6H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(B6H$timestamp)
# Restrict for date range of deployment:
B6H <- subset(B6H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(B6H$timestamp)

B6S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank6_S_20483163.csv"), header=T)
B6S$timestamp <- as.POSIXct(B6S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(B6S$timestamp)
# Restrict for date range of deployment:
B6S <- subset(B6S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(B6S$timestamp)

##
B7H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank7_H_20261881.csv"), header=T)
B7H$timestamp <- as.POSIXct(B7H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(B7H$timestamp)
# Restrict for date range of deployment:
B7H <- subset(B7H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(B7H$timestamp)

B7S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank7_S_20483164.csv"), header=T)
B7S$timestamp <- as.POSIXct(B7S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(B7S$timestamp)
# Restrict for date range of deployment:
B7S <- subset(B7S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(B7S$timestamp)

##
B8H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank8_H_20261882.csv"), header=T)
B8H$timestamp <- as.POSIXct(B8H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(B8H$timestamp)
# Restrict for date range of deployment:
B8H <- subset(B8H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(B8H$timestamp)

B8S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank8_S_20483165.csv"), header=T)
B8S$timestamp <- as.POSIXct(B8S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(B8S$timestamp)
# Restrict for date range of deployment:
B8S <- subset(B8S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(B8S$timestamp)

##
# C9H = no data 
C9S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank9_S_20483166.csv"), header=T)
C9S$timestamp <- as.POSIXct(C9S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(C9S$timestamp)
# Restrict for date range of deployment:
C9S <- subset(C9S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(C9S$timestamp)

##
C10H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank10_H_20261886.csv"), header=T)
C10H$timestamp <- as.POSIXct(C10H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(C10H$timestamp)
# Restrict for date range of deployment:
C10H <- subset(C10H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(C10H$timestamp)

C10S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank10_S_20483167.csv"), header=T)
C10S$timestamp <- as.POSIXct(C10S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(C10S$timestamp)
# Restrict for date range of deployment:
C10S <- subset(C10S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(C10S$timestamp)

##
C11S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank11_S_20483168.csv"), header=T)
C11S$timestamp <- as.POSIXct(C11S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(C11S$timestamp)
# Restrict for date range of deployment:
C11S <- subset(C11S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(C11S$timestamp)

##
C12H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank12_H_20261888.csv"), header=T)
C12H$timestamp <- as.POSIXct(C12H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(C12H$timestamp)
# Restrict for date range of deployment:
C12H <- subset(C12H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(C12H$timestamp)

C12S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank12_S_20483169.csv"), header=T)
C12S$timestamp <- as.POSIXct(C12S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(C12S$timestamp)
# Restrict for date range of deployment:
C12S <- subset(C12S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(C12S$timestamp)


##
D13H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank13_H_20261889.csv"), header=T)
D13H$timestamp <- as.POSIXct(D13H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(D13H$timestamp)
# Restrict for date range of deployment:
D13H <- subset(D13H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(D13H$timestamp)

D13S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank13_S_20483170.csv"), header=T)
D13S$timestamp <- as.POSIXct(D13S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(D13S$timestamp)
# Restrict for date range of deployment:
D13S <- subset(D13S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(D13S$timestamp)

##
D14S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank14_S_20483171.csv"), header=T)
D14S$timestamp <- as.POSIXct(D14S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(D14S$timestamp)
# Restrict for date range of deployment:
D14S <- subset(D14S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(D14S$timestamp)

##
D15H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank15_H_20261890.csv"), header=T)
D15H$timestamp <- as.POSIXct(D15H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(D15H$timestamp)
# Restrict for date range of deployment:
D15H <- subset(D15H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(D15H$timestamp)

D15S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank15_S_20483172.csv"), header=T)
D15S$timestamp <- as.POSIXct(D15S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(D15S$timestamp)
# Restrict for date range of deployment:
D15S <- subset(D15S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(D15S$timestamp)

##
D16S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank16_S_20483173.csv"), header=T)
D16S$timestamp <- as.POSIXct(D16S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(D16S$timestamp)
# Restrict for date range of deployment:
D16S <- subset(D16S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(D16S$timestamp)

##
E17S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank17_S_20483174.csv"), header=T)
E17S$timestamp <- as.POSIXct(E17S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(E17S$timestamp)
# Restrict for date range of deployment:
E17S <- subset(E17S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(E17S$timestamp)

##
E18S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank18_S_20483175.csv"), header=T)
E18S$timestamp <- as.POSIXct(E18S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(E18S$timestamp)
# Restrict for date range of deployment:
E18S <- subset(E18S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(E18S$timestamp)

##
E19H <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank19_H_20261895.csv"), header=T)
E19H$timestamp <- as.POSIXct(E19H$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(E19H$timestamp)
# Restrict for date range of deployment:
E19H <- subset(E19H,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-11 12:00:00'))
range(D15H$timestamp)

E19S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank19_S_20483176.csv"), header=T)
E19S$timestamp <- as.POSIXct(E19S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(E19S$timestamp)
# Restrict for date range of deployment:
E19S <- subset(E19S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(E19S$timestamp)

##
E20S <- read.csv(paste0(inputDir, "/2020SandyCorner/20200524_HOBO/Tank20_S_20483177.csv"), header=T)
E20S$timestamp <- as.POSIXct(E20S$Date.Time..GMT.06.00, format="%m/%d/%Y %H:%M")
range(E20S$timestamp)
# Restrict for date range of deployment:
E20S <- subset(E20S,timestamp >= as.POSIXct('0019-11-01 10:00:00') & 
                 timestamp <= as.POSIXct('0020-06-04 9:30:00'))
range(E20S$timestamp)


## ---------------------------
# Aggregate logger data
Ablock <- rbind(A1H, A1S, A2H, A2S, A3H, A3S, A4H, A4S)
Ablock$Block <- "A"

Bblock <- rbind(B5H, B5S, B6H, B6S, B7H, B7S, B8H, B8S)
Bblock$Block <- "B"

Cblock <- rbind(C9S, C10H, C10S, C11S, C12H, C12S)
Cblock$Block <- "C"

Dblock <- rbind(D13H, D13S, D14S, D15H, D15S, D16S)
Dblock$Block <- "D"

Eblock <- rbind(E17S, E18S, E19H, E19S, E20S)
Eblock$Block <- "E"


SandCorn <- rbind(Ablock, Bblock, Cblock, Dblock, Eblock)
names(SandCorn)
## ---------------------------
# QAQC data: 

colnames(SandCorn)[7] = "IntensityLux"

# 1. Flag temperature values:
SandCorn.Q=SandCorn %>% 
  mutate(temperature=ifelse(TempC>35, NA, TempC)) %>%
  mutate(hour=lubridate::hour(timestamp))%>%
  arrange(Tank_ID, HOBO_position, timestamp)%>%
  group_by(Tank_ID, HOBO_position, timestamp)%>% #this will get the nearest 15, but could be fewer if some are missing OR >35C, I think (?) the 35 are bogus so that is ok but you could
  mutate(mnT=rollapply(temperature, width = 15, FUN = mean, fill=NA),           # also filter out the NAs and >35s if you wanted to always have 15 values in your rolling window after removing bad values
         sdT=rollapply(temperature, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loT=mnT- (3*sdT), hiT=mnT+ (3*sdT))%>%
  mutate(mnL=rollapply(IntensityLux, width = 15, FUN = mean, fill=NA),           # also filter out the NAs and >35s if you wanted to always have 15 values in your rolling window after removing bad values
         sdL=rollapply(IntensityLux, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loL=mnL- (3*sdL), hiL=mnL+ (3*sdL))%>%
  full_join(., SandCorn)%>% 
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
p <- ggplot(SandCorn.Q, aes(x=timestamp, y=(temperature), colour =as.factor(flag_temperature))) +
  geom_point(alpha = 0.7)  +
  theme_classic() + facet_wrap(~Tank_ID)

p2 <- ggplot(SandCorn.Q, aes(x=timestamp, y=(temperature), colour =as.factor(flag_temperature))) +
  geom_point(alpha = 0.7)  +
  theme_classic() + facet_wrap(~Color)

p3 <- ggplot(SandCorn.p, aes(x=timestamp, y=(temperature), colour =as.factor(Color))) +
  geom_point(alpha = 0.3)  + scale_color_manual(values=c("#dec1a0", "#000000")) +
  theme_bw() + facet_grid(Block~.)
#ggsave(paste0(outputDir,("SC2020HOBO.pdf")), p3, scale = 1.75, width = 15, height = 30, units = c("cm"), dpi = 500)

# ice out plot 
SandCorn.p <- subset(SandCorn.Q2, timestamp >= as.POSIXct('0020-04-27 00:00:00') & timestamp <= as.POSIXct('0020-06-11 00:00:00'))
summary(SandCorn.p$timestamp)

names(SandCorn.Q)

# 3. Remove unwanted variables:
SandCorn.Q2 <- subset(SandCorn.Q, select=c(Block, Tank_ID, Color, HOBO_position, HOBO_Serial, timestamp, temperature,
                                         IntensityLux, flag_temperature, flag_Lux, Notes))

# 4. Double chec for duplicated values:
SandCorn.Q2%>%select(Tank_ID, HOBO_position, timestamp)%>%duplicated()%>%sum() # 0

# if there were duplicates use code below:
#View(Soddie.Q2%>%
#       inner_join(
#         Soddie.Q2 %>%
#           group_by(Tank, Position, timestamp) %>%
#           summarize(ct=dplyr::n())%>% filter(ct>1)))

# Remove values:
#Soddie.Q3 = Soddie.Q2 %>%
#  distinct(Tank, Position, timestamp, .keep_all = TRUE)
#Soddie.Q3%>%select(Tank, Position, timestamp)%>%duplicated()%>%sum() 

# 5. Export and save data:
# write.csv(SandCorn.Q2, paste0(outputDir, "SC2020HOBO_Q.csv")) # complied data file of all DO sensors along buoy line

## ---------------------------
# Analyze the data:
summary(SandCorn.Q2)
# remove outliers 
SandCorn.Q3 <- subset(SandCorn.Q2, flag_temperature=="n")
summary(SandCorn.Q3)

SandCorn.Q4 <- subset(SandCorn.Q3, timestamp >= as.POSIXct('0020-04-27 00:00:00') & timestamp <= as.POSIXct('0020-06-11 00:00:00'))
summary(SandCorn.Q4$timestamp)
 
hist(SandCorn.Q4$temperature)
SC.mod <- t.test(temperature~Color, data = SandCorn.Q4)
summary(SC.mod)

SC.mod2 <- glm(temperature~Color, data = SandCorn.Q4)
summary(SC.mod2)

SC.mod3 <- lmer(temperature ~ Color + (1|Block), data = SandCorn.Q4)
summary(SC.mod3)
hist(residuals(SC.mod3)) # not the greatest residuL distribution


