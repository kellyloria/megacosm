
d <- read.csv(file.choose())
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