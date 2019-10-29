#KAL: 2019-04-23

## Check quality
## Organize 
## Aggregate 

library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(vegan)
library(gridExtra)

#################
# 1st year 2008 #

# Sensor limits:
## Range: 
#    Temperature: -20° to 50°C (-4° to 158°F)
#    Light: 0 to 320,000 lux (0 to 30,000 lumens/ft2) 

## Science discovery color trial tank 1 top sensor ##
#############
# switched around column names to get rid of symbols
SDC_1LT <- read.csv("SDC_1LS_20261887.csv")
names(SDC_1LT)

# check timestamp
SDC_1LT$timestamp <- as.POSIXct(SDC_1LT$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(SDC_1LT$timestamp)

# Temperature range (January- August 2008)
hist(SDC_1LT$Temp_C) # some high temps 
#   Anything over 20'C highly unlikely especially in winter

# plot to see if increase in temp/ decrease in light might correspond to removal 
qplot(timestamp, Temp_C, data = SDC_1LT, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

hist(SDC_1LT$Lux)
qplot(timestamp, Lux, data = SDC_1LT, geom="point", ylab = "Lux") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Deployed 0018-11-18 08:00:00 - 0019-03-20 13:38:00

# Date restriction 
SDC_1LT <- subset(SDC_1LT,timestamp >= as.POSIXct('0018-11-18 08:00:00') & 
                  timestamp <= as.POSIXct('0019-03-20 13:38:00'))
range(SDC_1LT$timestamp)

write.csv(SDC_1LT, "SDC_1LT_Q.csv")

## Science discovery color trial tank 1 bottom sensor ##
#############
# switched around column names to get rid of symbols
SDC_1LH <- read.csv("SDC_1LH_20483158.csv")
names(SDC_1LH)

# check timestamp
SDC_1LH$timestamp <- as.POSIXct(SDC_1LH$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(SDC_1LH$timestamp)

# Temperature range (January- August 2008)
hist(SDC_1LH$Temp_C) # some high temps 
#   Anything over 20'C highly unlikely especially in winter

# plot to see if increase in temp/ decrease in light might correspond to removal 
qplot(timestamp, Temp_C, data = SDC_1LH, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

hist(SDC_1LH$Lux)
qplot(timestamp, Lux, data = SDC_1LH, geom="point", ylab = "Lux") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Deployed 0018-11-18 08:00:00 - 0019-03-20 13:38:00

# Date restriction 
SDC_1LH <- subset(SDC_1LH,timestamp >= as.POSIXct('0018-11-18 08:00:00') & 
                    timestamp <= as.POSIXct('0019-03-20 13:38:00'))
range(SDC_1LH$timestamp)

write.csv(SDC_1LH, "SDC_1LH_Q.csv")

## Science discovery color trial tank 2 botton sensor ##
#############
# switched around column names to get rid of symbols
SDC_2DB <- read.csv("SDC_2DH_20483159.csv")
names(SDC_2DB)

# check timestamp
SDC_2DB$timestamp <- as.POSIXct(SDC_2DB$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(SDC_2DB$timestamp)

# Temperature range (January- August 2008)
hist(SDC_2DB$Temp_C) 

# plot to see if increase in temp/ decrease in light might correspond to removal 
qplot(timestamp, Temp_C, data = SDC_2DB, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

hist(SDC_2DB$Lux)
qplot(timestamp, Lux, data = SDC_2DB, geom="point", ylab = "Lux") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Deployed 0018-11-18 08:00:00 - 0019-03-20 13:38:00

# Date restriction 
SDC_2DB <- subset(SDC_2DB,timestamp >= as.POSIXct('0018-11-18 08:00:00') & 
                    timestamp <= as.POSIXct('0019-03-20 13:38:00'))
range(SDC_2DB$timestamp)

write.csv(SDC_2DB, "SDC_2DB_Q.csv")



## Science discovery color trial tank 2 botton sensor ##
#############
# switched around column names to get rid of symbols
SDC_2DT <- read.csv("SDC_2DS_20261888.csv")
names(SDC_2DT)

# check timestamp
SDC_2DT$timestamp <- as.POSIXct(SDC_2DT$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(SDC_2DT$timestamp)

# Temperature range (January- August 2008)
hist(SDC_2DB$Temp_C) 

# plot to see if increase in temp/ decrease in light might correspond to removal 
qplot(timestamp, Temp_C, data = SDC_2DT, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

hist(SDC_2DB$Lux)
qplot(timestamp, Lux, data = SDC_2DT, geom="point", ylab = "Lux") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Deployed 0018-11-18 08:00:00 - 0019-03-20 13:38:00

# Date restriction 
SDC_2DT <- subset(SDC_2DT,timestamp >= as.POSIXct('0018-11-18 08:00:00') & 
                    timestamp <= as.POSIXct('0019-03-20 13:38:00'))
range(SDC_2DT$timestamp)

write.csv(SDC_2DT, "SDC_2DT_Q.csv")



## Science discovery color trial tank 3 surface sensor ##
#############
# switched around column names to get rid of symbols
SDC_3LT <- read.csv("SDC_3LS_20261889.csv")
names(SDC_3LT)

# check timestamp
SDC_3LT$timestamp <- as.POSIXct(SDC_3LT$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(SDC_3LT$timestamp)

# Temperature range (January- August 2008)
hist(SDC_3LT$Temp_C) 

# plot to see if increase in temp/ decrease in light might correspond to removal 
qplot(timestamp, Temp_C, data = SDC_3LT, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

hist(SDC_3LT$Lux)
qplot(timestamp, Lux, data = SDC_3LT, geom="point", ylab = "Lux") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Deployed 0018-11-18 08:00:00 - 0019-03-20 13:38:00

# Date restriction 
SDC_3LT <- subset(SDC_3LT,timestamp >= as.POSIXct('0018-11-18 08:00:00') & 
                    timestamp <= as.POSIXct('0019-03-20 13:38:00'))
range(SDC_3LT$timestamp)

write.csv(SDC_3LT, "SDC_3LT_Q.csv")


## Science discovery color trial tank 3 botton sensor ##
#############
# switched around column names to get rid of symbols
SDC_3LB <- read.csv("SDC_3LH_20483160.csv")
names(SDC_3LB)

# check timestamp
SDC_3LB$timestamp <- as.POSIXct(SDC_3LB$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(SDC_3LB$timestamp)

# Temperature range (January- August 2008)
hist(SDC_3LB$Temp_C) 

# plot to see if increase in temp/ decrease in light might correspond to removal 
qplot(timestamp, Temp_C, data = SDC_3LB, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

hist(SDC_3LB$Lux)
qplot(timestamp, Lux, data = SDC_3LB, geom="point", ylab = "Lux") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Deployed 0018-11-18 08:00:00 - 0019-03-20 13:38:00

# Date restriction 
SDC_3LB <- subset(SDC_3LB,timestamp >= as.POSIXct('0018-11-18 08:00:00') & 
                    timestamp <= as.POSIXct('0019-03-20 13:38:00'))
range(SDC_3LB$timestamp)

write.csv(SDC_3LB, "SDC_3LB_Q.csv")

## Science discovery color trial tank 4 botton sensor ##
#############
# switched around column names to get rid of symbols
SDC_4DB <- read.csv("SDC_4DH_20483161.csv")
names(SDC_4DB)

# check timestamp
SDC_4DB$timestamp <- as.POSIXct(SDC_4DB$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(SDC_4DB$timestamp)

# Temperature range (January- August 2008)
hist(SDC_4DB$Temp_C) 

# plot to see if increase in temp/ decrease in light might correspond to removal 
qplot(timestamp, Temp_C, data = SDC_4DB, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

hist(SDC_4DB$Lux)
qplot(timestamp, Lux, data = SDC_4DB, geom="point", ylab = "Lux") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Deployed 0018-11-18 08:00:00 - 0019-03-20 13:38:00

# Date restriction 
SDC_4DB <- subset(SDC_4DB,timestamp >= as.POSIXct('0018-11-18 08:00:00') & 
                    timestamp <= as.POSIXct('0019-03-20 13:38:00'))
range(SDC_4DB$timestamp)

write.csv(SDC_4DB, "SDC_4DB_Q.csv")


## Science discovery color trial tank 4 surface sensor ##
#############
# switched around column names to get rid of symbols
SDC_4DT <- read.csv("SDC_4DS_20261896.csv")
names(SDC_4DT)

# check timestamp
SDC_4DT$timestamp <- as.POSIXct(SDC_4DT$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(SDC_4DT$timestamp)

# Temperature range (January- August 2008)
hist(SDC_4DT$Temp_C) 

# plot to see if increase in temp/ decrease in light might correspond to removal 
qplot(timestamp, Temp_C, data = SDC_4DT, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

hist(SDC_4DT$Lux)
qplot(timestamp, Lux, data = SDC_4DT, geom="point", ylab = "Lux") +
  scale_x_datetime(date_breaks = "96 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Deployed 0018-11-18 08:00:00 - 0019-03-20 13:38:00

# Date restriction 
SDC_4DT <- subset(SDC_4DT,timestamp >= as.POSIXct('0018-11-18 08:00:00') & 
                    timestamp <= as.POSIXct('0019-03-20 13:38:00'))
range(SDC_4DT$timestamp)

write.csv(SDC_4DT, "SDC_4DT_Q.csv")




Toplarge <- rbind(SDC_1LT, SDC_2DT)

SDC_1LH$date <- (date(SDC_1LH$timestamp))
SDC_2DB$date <- (date(SDC_2DB$timestamp))

SDC_1LH.2 <-SDC_1LH %>% 
  subset(date > "0019-01-15")
SDC_1LH.1 <-SDC_1LH %>% 
  subset(date < "0019-01-15")

SDC_1LH.1$Temp_C.1 <- SDC_1LH.1$Temp_C - 3
SDC_1LH.2$Temp_C.1 <- SDC_1LH.2$Temp_C - 2
SDC_1LH.3 <- rbind(SDC_1LH.1, SDC_1LH.2) 

SDC_2DB.2 <-SDC_2DB %>% 
  subset(date > "0019-01-15")
SDC_2DB.1 <-SDC_2DB %>% 
  subset(date < "0019-01-15")
SDC_2DB.2$Temp_C.1 <- SDC_2DB.2$Temp_C - 1.25
SDC_2DB.1$Temp_C.1 <- SDC_2DB.1$Temp_C - 3
SDC_2DB.3 <- rbind(SDC_2DB.1, SDC_2DB.2)

Bottomlarge <- rbind(SDC_2DB.3, SDC_1LH.3)

Topall <- rbind(SDC_1LT, SDC_2DT, SDC_4DT, SDC_3LT)
summary(Topall)


all <- rbind(SDC_1LT, SDC_2DT, SDC_4DT, SDC_3LT, SDC_4DB, SDC_3LB,SDC_2DB, SDC_1LH)
summary(all)

Topsmall <- rbind(SDC_4DT, SDC_3LT)

# All Loggers

Bottomall <- rbind(SDC_4DB, SDC_3LB,SDC_2DB, SDC_1LH)
#write.csv(HOBO2008_2011, "HOBO2008_2011Q.csv")

# more date bins
SDC$date <- (date(SDC$timestamp))

# more date bins
Topall$date <- (date(Topall$timestamp))
Bottomall$date <- (date(Bottomall$timestamp))
all$date <- (date(all$timestamp))
Toplarge$date <- (date(Toplarge$timestamp))
Bottomlarge$date <- (date(Bottomlarge$timestamp))


Topall.ave <-Topall %>% 
  group_by(tank, date, color) %>%
  summarise("DayT"  = mean(Temp_C))

Bottomall.ave <-Bottomall %>% 
  group_by(tank, date, color) %>%
  summarise("DayT"  = mean(Temp_C))

all.ave <-all %>% 
  group_by(tank, date, color) %>%
  summarise("DayT"  = mean(Temp_C))

# time cut off for melt only 
all.ave.s <-all.ave %>% 
  subset(date > "0019-01-20") 

# time cut off for melt only 
Bottomall.ave.s <-Bottomall.ave %>% 
  subset(date > "0019-01-20") 

Toplarge.ave <-Toplarge %>% 
  group_by(tank, date, color) %>%
  summarise("DayT"  = mean(Temp_C)) %>%
  subset(date > "0019-01-20") 

Bottomlarge.ave <-Bottomlarge %>% 
  group_by(tank, date, color) %>%
  summarise("DayT"  = mean(Temp_C.1)) %>%
  subset(date > "0019-01-05")


names(SDC) 
allhoboplot <- ggplot(data = Toplarge.ave,
                  aes(x = date,
                      y = DayT,
                      color = (color), shape=as.factor(color))) +
  geom_point() + geom_line() +
  labs( x = "Date", y = "Temp") +  theme_bw()  + 
  scale_y_continuous(breaks=seq(-4,16,2)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
  scale_x_date(date_breaks = "4 days", labels = date_format("%b %d")) +
  scale_color_manual(values=alpha(c("#ddbf6c", "black")),0.85) +  
  scale_shape_manual(values=c(17, 15)) 
#+ facet_grid(.~depth)#+ facet_grid(color~.)

allhoboplot <- ggplot(data = Bottomlarge.ave,
                      aes(x = date,
                          y = DayT,
                          color = (color))) +
  geom_point() + geom_line() +
  labs( x = "Date", y = "Temperature ('C)") +  theme_bw()  + 
  scale_y_continuous(breaks=seq(-4,16,2)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
  scale_x_date(date_breaks = "4 days", labels = date_format("%b %d")) +
  scale_color_manual(values=alpha(c("black", "#ddbf6c")),0.85) 
#+ facet_grid(.~depth)#+ facet_grid(color~.)
ggsave("allhoboplot.jpeg", allhoboplot, scale = 1.75, width = 10, height = 5, units = c("cm"), dpi = 500)


allhoboplot <- ggplot(data = all.ave.s,
                      aes(x = date,
                          y = DayT,
                          color = (color), shape=as.factor(color))) +
  geom_point() + geom_line() +
  labs( x = "Date", y = "Temp") +  theme_bw()  + 
  scale_y_continuous(breaks=seq(-4,16,2)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
  scale_x_date(date_breaks = "4 days", labels = date_format("%b %d")) +
  scale_color_manual(values=alpha(c("#ddbf6c", "black")),0.85) +  
  scale_shape_manual(values=c(15, 17)) 
#+ facet_grid(.~depth)#+ facet_grid(color~.)
  
  
  

ggsave(filename = "2019Topsmallplot.pdf", allhoboplot, scale = 0.75, width = 50,
       height = 20, units = c("cm"),
       dpi = 300)


summary(Toplarge$serial)
# See if the sensors varry
FT_SDC3 <- read.csv("20483160_FT_Test_SDC3LB.csv")
names(FT_SDC3)
FT_SDC3$timestamp <- as.POSIXct(FT_SDC3$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(FT_SDC3$timestamp)
FT_SDC3 <- subset(FT_SDC3,timestamp >= as.POSIXct('0019-03-21 15:00:00') & 
                    timestamp <= as.POSIXct('0019-03-25 10:00:00'))
names(FT_SDC3)
sd1.mod <- glm(Temp_C ~ timestamp, data = FT_SDC3)
summary(sd1.mod)

FT_SDC4 <- read.csv("20483161_FT_Test_SDC4DB.csv")
names(FT_SDC4)
FT_SDC4$timestamp <- as.POSIXct(FT_SDC4$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(FT_SDC4$timestamp)
FT_SDC4 <- subset(FT_SDC4,timestamp >= as.POSIXct('0019-03-21 15:00:00') & 
                    timestamp <= as.POSIXct('0019-03-25 10:00:00'))
names(FT_SDC4)
sd2.mod <- glm(Temp_C ~ timestamp, data = FT_SDC4)
summary(sd2.mod)

light<- c(7.601e-05 + 3.888e-06)
dark<- c(7.740e-05 + 3.978e-06)
light-dark

#FT_SDC3 <- read.csv("20483160_FT_Test_SDC3LB.csv")
#names(FT_SDC3)
#FT_SDC4 <- read.csv("20483161_FT_Test_SDC4DB.csv")
#names(FT_SDC4)

#2019_AllTankAve.csv

tempdiff2 <- read.csv("SDC_LargeQ.csv")
tempdiff2$timestamp2 <- as.POSIXct(tempdiff2$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(tempdiff2$timestamp2)


tempdiff3 <- read.csv("SDC_TopLarge_Q.csv")
tempdiff3$timestamp2 <- as.POSIXct(tempdiff3$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(tempdiff3$timestamp2)

tempdiff4 <- read.csv("SDC_TopSmall_Q.csv")
tempdiff4$timestamp2 <- as.POSIXct(tempdiff4$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(tempdiff4$timestamp2)

tempdiff5 <- read.csv("SDC_smallbottom_Q.csv")
tempdiff5$timestamp2 <- as.POSIXct(tempdiff5$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(tempdiff5$timestamp2)

mean(tempdiff2$TempDiff)

allhoboplotdif2 <- ggplot(data = tempdiff2,
                      aes(x = timestamp2,
                          y = TempDiff)) +
  geom_point() + geom_hline(aes(yintercept=0), color="red", size=1) +
  labs( x = "Timestamp (30 minutes)",  
        y = "Temperature (Dark-Light)") + theme_bw() + 
  #scale_x_datetime(limits = ymd_hms(c("0018-11-18 08:00:00", "0019-03-20 13:30:00")), date_breaks = "24 hour", labels = date_format("%b %d")) + 
  scale_y_continuous(breaks=seq(-4,7,1)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

ggsave(filename = "smalltempdiff.pdf", allhoboplotdif2, scale = 0.85, width = 32,
       height = 10, units = c("cm"),
       dpi = 300)

names(tempdiff2)
mean(tempdiff2$TempDiff)
# histogram of temperature bottom differences
p <-ggplot(tempdiff2, aes(x=TempDiff, color=position)) +
  geom_histogram(fill="gray85", position="dodge") + 
  labs( x = "Difference in Temp (C)", y = "Count") +
  geom_vline(aes(xintercept=0, color="grey25"), linetype="solid") + 
  geom_vline(aes(xintercept=-0.0780, color="brickred"), linetype="dashed") +
  theme_bw() + scale_color_manual(values=c("#727272","#af1515", "#1d3559"))

ggsave(filename = "Largebottomsensor_tempdiff_hist.pdf", p, scale = 0.85, width = 20,
       height = 13, units = c("cm"),
       dpi = 300)

# predictions for light and dark 
tankpred1 <- ggplot(data = tempdiff2,
                    aes(x = Temp_dark,
                        y = Temp_light)) +
  geom_point() + geom_abline(colour = "red", intercept = 0, slope = 1) +
  labs(title="Large tanks: Bottom sensors", x = "Temperature Dark",  
        y = "Temperature Light") + 
  scale_y_continuous(limits = c(0, 13), breaks=seq(0,14,1.5)) + 
  scale_x_continuous(limits = c(0, 13), breaks=seq(0,14,1.5)) +
  theme_bw() #+ 


tankpred2 <- ggplot(data = tempdiff3,
                    aes(x = Temp_D,
                        y = Temp_L)) +
  geom_point() + geom_abline(colour = "red", intercept = 0, slope = 1) +
  labs(title="Large tanks: Top sensors", x = "Temperature Dark",  
       y = "Temperature Light") + 
  scale_y_continuous(limits = c(-5, 15), breaks=seq(-5,15,2.5)) + 
  scale_x_continuous(limits = c(-5, 15), breaks=seq(-5,15,2.5)) +
  theme_bw()



tankpred3 <- ggplot(data = tempdiff5,
                    aes(x = Temp_D,
                        y = Temp_L)) +
  geom_point() + geom_abline(colour = "red", intercept = 0, slope = 1) +
  labs(title="Small tanks: Bottom sensors", x = "Temperature Dark",  
       y = "Temperature Light") + 
  scale_y_continuous(limits = c(0, 13), breaks=seq(0,14,1.5)) + 
  scale_x_continuous(limits = c(0, 13), breaks=seq(0,14,1.5)) +
  theme_bw()

tankpred4 <- ggplot(data = tempdiff4,
                    aes(x = Temp_D,
                        y = Temp_L)) +
  geom_point() + geom_abline(colour = "red", intercept = 0, slope = 1) +
  labs(title="Small tanks: Top sensors", x = "Temperature Dark",  
       y = "Temperature Light") + 
  scale_y_continuous(limits = c(-5, 15), breaks=seq(-5,15,2.5)) + 
  scale_x_continuous(limits = c(-5, 15), breaks=seq(-5,15,2.5)) +
  theme_bw()

p <- grid.arrange(tankpred2, tankpred1, tankpred4, tankpred3, nrow = 2, ncol=2)

ggsave(filename = "2019SDpredpanel.pdf", p, scale = 0.85, width = 30,
       height = 30, units = c("cm"),
       dpi = 300)

tank.4glm <- glm(tempdiff4$Temp_D~tempdiff4$Temp_L)
acf(residuals(tank.4glm))

tank.2glm <- glm(tempdiff2$Temp_dark~tempdiff2$Temp_light)
acf(residuals(tank.2glm))


#2019_AllTankAve.csv

AllTankAve <- read.csv("2019_AllTankAve.csv")

AllTankAve$timestamp <- as.POSIXct(AllTankAve$Date.Time_GMT_minus06.00, format="%m/%d/%Y %H:%M")
range(AllTankAve$timestamp)

names(AllTankAve) 
allhoboplot <- ggplot(data = AllTankAve,
                      aes(x = timestamp,
                          y = T_Ave,
                          color = (color), shape=as.factor(size))) +
  geom_point() +  
  labs( x = "Date",  
        y = "Temp") + #geom_jitter(width = 0.18, height = 0) + 
  theme_bw() +
  scale_colour_manual(values = c("black","burlywood1")) + 
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) + 
  scale_y_continuous(breaks=seq(-4,16,2)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) +
  #scale_shape_manual(values=c(16,16, 16, 16)) + 
  facet_grid(size~.)

ggsave(filename = "2019Toplargeplot.pdf", allhoboplot, scale = 0.75, width = 40,
       height = 20, units = c("cm"),
       dpi = 300)