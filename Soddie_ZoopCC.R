## ---------------------------
## QA'QC for 2019 Soddie "Megacosm" Pilot: 
##  Zooplankton Community Comp Data Logger Data
##
## Author: Kelly A. Loria
## Date Created: 2020-06-15
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
# Read in data and fix timestamp
Sod_ZP <- read.csv(paste0(inputDir, "/2019_Soddie/2019_mescosm_zoopQ.csv"), header=T)
names(Sod_ZP)

# Fix date
Sod_ZP$date <- as.Date.character(Sod_ZP$date.collected, format="%m/%d/%y")
range(Sod_ZP$date)
summary(Sod_ZP)
names(Sod_ZP)

# Aggregate all tows rows into sample total:
Sod_ZP.Q <- Sod_ZP %>% 
  group_by(Tank, date) %>% 
  summarise("water_volume" = max(VOLUME.EXAMINED..ML., na.rm = T),
            "total"= sum(Total.Specimen.number, na.rm = T),
            "D.pulicaria"= sum(Daphnia.pulex, na.rm = T),
            "D.pulicaria_neonate"= sum(Daphnia.pulex.Neonate, na.rm = T),
            "ephippia"= sum(ephippia.only, na.rm = T),
            "H.shoshone"= sum(Hesperodiaptomus.shoshone, na.rm = T),
            "Ergasilus"= sum(Ergasilus.spp., na.rm = T),
            "naupli"= sum(naupilus, na.rm = T),
            "Chydoridae"= sum(Chydroidae, na.rm = T),
            "Ostracoda"= sum(Ostracoda.spp., na.rm = T),
            "Chaoborus"= sum(Chaoborus, na.rm = T),
            "Chronomid"= sum(chronomid, na.rm = T),
            "Ephemeroptera"= sum(Ephemeroptera, na.rm = T),
            "Odonata"= sum(Dragonfly.larvae, na.rm = T),
            "Trichoptera"= sum(tricop, na.rm = T),
            "Mite"= sum(Mite, na.rm = T),
            "Unknown"= sum(unknown, na.rm = T))
names(Sod_ZP.Q)
summary(z19com.qa)

# calculate taxa density
Sod_ZP.Q$total.D <- (Sod_ZP.Q$total/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$D.pulicaria.D <- (Sod_ZP.Q$D.pulicaria/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$Neonate.D <- (Sod_ZP.Q$D.pulicaria_neonate/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$ephippia.D <- (Sod_ZP.Q$ephippia/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$H.shoshone.D <- (Sod_ZP.Q$H.shoshone/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$Ergasilus.D <- (Sod_ZP.Q$Ergasilus/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$naupli.D <- (Sod_ZP.Q$naupli/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$Chydoridae.D <- (Sod_ZP.Q$Chydoridae/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$Ostracoda.D <- (Sod_ZP.Q$Ostracoda/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$Chaoborus.D <- (Sod_ZP.Q$Chaoborus/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$Chronomid.D <- (Sod_ZP.Q$Chronomid/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$Ephemeroptera.D <- (Sod_ZP.Q$Ephemeroptera/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$Odonata.D <- (Sod_ZP.Q$Odonata/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$Trichoptera.D <- (Sod_ZP.Q$Trichoptera/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$Mite.D <- (Sod_ZP.Q$Mite/ Sod_ZP.Q$water_volume)
Sod_ZP.Q$Unknown.D <- (Sod_ZP.Q$Unknown/ Sod_ZP.Q$water_volume)


#write.csv(Sod_ZP.Q, paste0(outputDir, "SoddieZopCC_Q.csv")) # complied data file of all DO sensors along buoy line




