# Water quality data from Soddie pilots in 2019

library(tidyverse)
library(lubridate)
library(MuMIn)

#d <- read.csv(file.choose())
#
names(d)
d$amount

summary(d$pack)
range(na.omit(d$PAR))

par_plot <- qplot(date, log10(PAR), data = d, geom="point", ylab = "log10(PAR)", 
      color = factor(amount), shape=factor(removed)) +
  theme_classic() + theme(axis.text.x = element_text(angle = 25, 
                                                     vjust = 1.0, hjust = 1.0), 
                          panel.border = element_rect(linetype = "solid", fill = NA))



ggsave("par_plot.pdf",
       scale = 2, width = 8, height = 5, units = c("cm"),
       dpi = 300)


diff_par_plot <- qplot(date, (diff_PAR_avec_t), data = d, geom="point", ylab = "Difference in PAR", 
      color = factor(amount), shape=factor(removed)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  theme_classic() + theme(axis.text.x = element_text(angle = 25, 
                                                     vjust = 1.0, hjust = 1.0), 
                          panel.border = element_rect(linetype = "solid", fill = NA))

ggsave("diff_par_plot.pdf",
       scale = 2, width = 8, height = 5, units = c("cm"),
       dpi = 300)

PAR.mod1 <- lmer(diff_PAR_avec_t ~ pack + (1|removed), data=d)
summary(PAR.mod1)

PAR.mod2 <- glm(diff_PAR_avec_t ~ pack, data=d)
summary(PAR.mod2)

PAR.mod3 <- glm(PAR ~ treatment, data=d)
summary(PAR.mod3)

PAR.mod1.p <- lmer(PAR ~ pack + (1|removed), data=d)
summary(PAR.mod1.p)

PAR.mod2 <- glm(PAR ~ pack, data=d)
summary(PAR.mod2)


PAR_TC <- aggregate(PAR ~ treatment, data=d, FUN=mean) 
PAR_TC$SE <- aggregate(PAR ~ treatment, data=d, FUN=se)[,2]


Temp_TC <- aggregate(Temperature..C. ~ Color..black.biege., data=d, FUN=mean) 
Temp_TC$SE <- aggregate(Temperature..C. ~ Color..black.biege., data=d, FUN=se)[,2]

mean(na.omit(d$Temperature..C.))

p <- ggplot(PAR_TC, aes(x=treatment, y=PAR, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=PAR-SE, ymax=PAR+SE), width=0,
                position=position_dodge(.9)) +
  scale_y_continuous(name="Average PAR", limits=c(0, 730), breaks=seq(0,730, 100))

p + scale_fill_manual(values=c("dodgerblue2", "darkseagreen4")) + theme_classic() + 
  theme(axis.text.x = element_text(vjust = 1.0),  
        panel.border = element_rect(linetype = "solid", fill = NA))



ggsave("DOC_PAR_pot.pdf",
       scale = 2, width = 5, height = 4, units = c("cm"),
       dpi = 300)

names(d)
d$date1 <- as.Date(as.Date.character(d$date, format="%m/%d/%y"))
range(d$date1)

d1 <- subset(d, date1<"2018-07-24", 
              select = tank:date1)
range(d1$date1)

DOCmod <- lmer(PAR ~ treatment + (1|tank), data=d)
summary(DOCmod)

DOCmod1 <- glm(PAR ~ treatment, data=d1)
summary(DOCmod1)

#lp75 <- subset(d, pack=="willow75",
#                select=tank:diff_PAR_avec_t)

#lp250 <- subset(d, pack=="willow250",
#               select=tank:diff_PAR_avec_t)

#lp500 <- subset(d, pack=="willow500",
#                select=tank:diff_PAR_avec_t)

#lp1500 <- subset(d, pack=="willow1500",
#                select=tank:diff_PAR_avec_t)


#plot(x = lp75$date, y=lp75$diff_PAR_avec_t, type="l", lty =1, lwd = 0.8, col= "tomato", 
#     ylab="Average water temperature", xlab="Date (aggregated to time-stamp)")
#abline(h=0)
#lines(x = Temp$timestamp, y=Temp$Average_temp, lty =1, lwd = 0.8, col= "dodgerblue")
#legend("bottomright", legend=c("Warmed", "Control"),
#       col=c("tomato", "dodgerblue"), pch=19, cex=0.75)

#################################################
## LOOK at tanks!
## shape for whether DOC was pulled from tank




######################
# Soddie 2019 DOC data

doc <- read.csv("2019_DOCpilot.csv")
names(doc)

# fix and add in more temporal parameters 
doc$date1 <- as.Date.character(doc$date, format="%m/%d/%y")
range(doc$date1)

#gl4.zo <- transform(gl4.zoo2, ndate = as.numeric(date),
#                    year = as.numeric(format(date, '%Y')),
#                    nmonth = as.numeric(format(date, '%m')),
#                    doy    = as.numeric(format(date, '%j')))

#gl4.zo$weekn <- week(gl4.zo$date)
#gl4.zo$weekny <- paste(gl4.zo$weekn, gl4.zo$year)

# restrict to 7/24/19
doc.0 <- doc %>% 
  subset(date1 == "2019-07-10")

doc.1 <- doc %>% 
  subset(date1 == "2019-07-24")

# restrict to after 7/24/19
doc.2 <- doc %>% 
  subset(date1 == "2019-08-19")

se <- function(x) sqrt(var(x)/length(x))

DOC_TC0 <- aggregate(DOC ~ treatment, data=doc.0, FUN=mean) 
DOC_TC0$SE <- aggregate(DOC ~ treatment, data=doc.0, FUN=se)[,2]
DOC_TC0$date <- "2019-07-10"

DOC_TC1 <- aggregate(DOC ~ treatment, data=doc.1, FUN=mean) 
DOC_TC1$SE <- aggregate(DOC ~ treatment, data=doc.1, FUN=se)[,2]
DOC_TC1$date <- "2019-07-19"

DOC_TC2 <- aggregate(DOC ~ treatment, data=doc.2, FUN=mean) 
DOC_TC2$SE <- aggregate(DOC ~ treatment, data=doc.2, FUN=se)[,2]
DOC_TC2$date <- "2019-08-06"

doc_2 <- rbind(DOC_TC1, DOC_TC2)

p <- ggplot(doc_2, aes(x=date, y=DOC, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=DOC-SE, ymax=DOC+SE), width=0,
                position=position_dodge(.9)) +
  scale_y_continuous(name="DOC (mg/L)", limits=c(0, 9), breaks=seq(0, 9, 1.5))

p + scale_fill_manual(values=c("dodgerblue2", "darkseagreen4")) + theme_classic() + 
  theme(axis.text.x = element_text(vjust = 1.0),  
        panel.border = element_rect(linetype = "solid", fill = NA))

#ggsave("DOC_pot.jpg",
#       scale = 2, width = 5, height = 4, units = c("cm"),
#       dpi = 300)







