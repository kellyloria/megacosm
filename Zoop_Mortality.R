# H. Shoshone temperature based mortality experiment 
# 2019-10-23

library(ggplot2)
library(dplyr)
library(lubridate)
library(vegan)
library(gridExtra)

d <- read.csv("Zoop_mortality_TOD.csv")
names(d)

# fix date variable:
d$timestamp1 <- as.POSIXct(d$timestamp, format="%m/%d/%Y %H:%M")
range(d$timestamp1)

# make column for % dead
d$per_dead <- (d$total.dead/ 9)

mortplot <- ggplot(data = d, aes(x = timestamp1, y = per_dead,
                          color =as.factor(Incubator), shape=as.factor(Treatment))) +
  geom_point() + geom_line() + labs( x = "Date", y = "Percent dead") + 
  #geom_jitter(width = 0.18, height = 0) + 
  theme_bw() + scale_x_datetime(date_breaks = "8 hour") + 
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
  facet_grid(Treatment~.)

#not much of a trend:
morttempplot <- ggplot(data = d, aes(x = Temperature, y = total.dead,
                                 color =as.factor(Incubator), shape=as.factor(Treatment))) +
  geom_point() + labs( x = "Temp", y = "Number dead")  

#ggsave(filename = "19mortalplot.jpeg", mortplot, scale = 0.75, width = 40,
#       height = 20, units = c("cm"),
#       dpi = 300)


## Traits ##
e <- read.csv("Zoop_mortalitytraits.csv")
names(e)


# plot size and TOD
sizeTODplot <- ggplot(data = e, aes(x = observation.number, y = Size_mm,
                                 color =as.factor(Sex), shape=as.factor(Sex))) +
  geom_point() + labs( x = "Observation number", y = "Size (mm)") +
  #geom_jitter(width = 0.18, height = 0) + 
  theme_bw() 

sizeTODplot <- ggplot(data = e, aes(x = observation.number, y = total_dead,
                                    color =as.factor(Sex), shape=as.factor(Sex))) +
  geom_point() + labs( x = "Observation number", y = "Size (mm)") +
  #geom_jitter(width = 0.18, height = 0) + 
  theme_bw() 

# bar plot of sex of individuals dead at each observation  
sizeTODplot <- ggplot(data = e, aes(x = observation.number, y=number_dead)) +
  geom_bar(fill = Sex) + labs( x = "Observation number", y = "Size (mm)") +
  #geom_jitter(width = 0.18, height = 0) + 
  theme_bw() 

ggplot(data=e, aes(x=observation.number, y=number_dead, fill=Sex)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  theme_minimal()


## Sex based traits ##
f <- read.csv("zoop_mortalitysex.csv")
names(f)

# fix date variable:
f$timestamp1 <- as.POSIXct(f$timestamp, format="%m/%d/%Y %H:%M")
range(f$timestamp1)

sizeTODplot <- ggplot(data = f) +
  geom_point(aes(x = timestamp1, y = per.Fdead), size= 2, color=  alpha(c("#e01809"), 0.7)) + 
  geom_line(aes(x = timestamp1, y = per.Fdead), size= 1.2, color=  alpha(c("#e01809"), 0.7)) +
  geom_point(aes(x = timestamp1, y = per.Gdead), size= 2, color=  alpha(c("#8a50a1"), 0.7)) + 
  geom_line(aes(x = timestamp1, y = per.Gdead), size= 1.2, color=  alpha(c("#8a50a1"), 0.7)) +
  geom_point(aes(x = timestamp1, y = per.Mdead), size= 2, color=  alpha(c("#1881cc"), 0.7)) + 
  geom_line(aes(x = timestamp1, y = per.Mdead), size= 1.2, color=  alpha(c("#1881cc"), 0.7)) +
  scale_x_datetime(date_breaks = "24 hour", labels = date_format("%b %d")) + 
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  labs( x = "Observation number", y = "Proportion population dead") + 
  theme_bw() 

ggsave(filename = "sizeTODplot.jpeg", sizeTODplot, scale = 0.75, width = 15,
       height = 12, units = c("cm"),
       dpi = 300)
