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

ggsave(filename = "19mortalplot.jpeg", mortplot, scale = 0.75, width = 40,
       height = 20, units = c("cm"),
       dpi = 300)