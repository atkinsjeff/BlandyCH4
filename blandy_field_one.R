#Blandy script one looking at data through August 4 for presentation

rm(list=ls())

field<-read.csv(file="c:\\Users\\Jeff\\Documents\\R\\DATA\\blandy_field.csv",head=TRUE)
field <-data.frame(field)

field$DATE <- as.Date(field$DATE, format="%m/%d/%Y")
library(ggplot2)

pFLUX <- ggplot(data=field, aes(y=FLUX, x=THETA, color=as.factor(DISTANCE)))+
  geom_point(size=10)+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme( legend.justification=c(1,1), legend.position=c(1,1), legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
         legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  labs(y=expression("F" ["SOIL"]  ))+
  xlab("")

pFLUX

#water level
water<-read.csv(file="c:\\Users\\Jeff\\Documents\\R\\DATA\\lake_level.csv",head=TRUE)
water <-data.frame(water)

water$date <- as.Date(water$date, format="%m/%d/%Y")

lm.lake <-lm(water$level ~ water$date)
pLAKE <-ggplot(data=water, aes(y=level, x=date))+
  geom_line( size=4, color="#002244")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(0,3), expand = c(0,0))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme( legend.justification=c(1,1), legend.position=c(1,1), legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
         legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
ylab("WATER LEVEL (ft)")+
xlab("")
pLAKE

#soil moisture data
d5<-read.csv(file="c:\\Users\\Jeff\\Documents\\R\\DATA\\decagon_five_min.csv",head=TRUE)
d5 <-data.frame(d5)
d15 <-read.csv(file="c:\\Users\\Jeff\\Documents\\R\\DATA\\decagon_15_min.csv",head=TRUE)
d15 <-data.frame(d15)

d5$DATETIME <- as.POSIXct(strptime(d5$DATETIME, "%m/%d/%Y %H:%M"))
d15$DATETIME <- as.POSIXct(strptime(d15$DATETIME, "%m/%d/%Y %H:%M"))


#reshaping for plotting
library(reshape2)

decagonx <- melt(d5, id.vars ="DATETIME")
decagony <- melt(d15, id.vars="DATETIME")

decagon5_10 <-melt(d5, id.vars="DATETIME", measure.vars= c("ATEN50", "ATEN20", "ATEN0"))
decagon5_20 <-melt(d5, id.vars="DATETIME", measure.vars= c("ATWENTY50", "ATWENTY20", "ATWENTY0"))
#plot for andy o m soil moisture data
decagon <- ggplot(data=decagony, aes(y=value, x=DATETIME, color=variable))+
  geom_point( size=4)+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  #scale_y_continuous(limits=c(0,0.6), expand = c(0,0))+
  scale_fill_discrete(name="DEPTH",
                      breaks=c("ATEN50", "ATEN20", "ATEN0", "ATWENTY50", "ATWENTY20", "ATWENTY0"),
                      labels=c("10m (50cm)", "10m (20cm)", "10m(5cm)", "20m (50cm)", "20m (20cm)", "20m(5cm)" ))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme( legend.position="none" )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  ylab("VWC")+
  xlab("")
  
decagon
#plot for andy 10 m soil moisture
decagon10 <- ggplot(data=decagon5_10, aes(y=value, x=DATETIME, color=variable))+
  geom_point( size=4)+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  #scale_y_continuous(limits=c(0,0.6), expand = c(0,0))+
  scale_fill_discrete(name="DEPTH",
                      breaks=c("ATEN50", "ATEN20", "ATEN0", "ATWENTY50", "ATWENTY20", "ATWENTY0"),
                      labels=c("10m (50cm)", "10m (20cm)", "10m(5cm)", "20m (50cm)", "20m (20cm)", "20m(5cm)" ))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme( legend.position="none" )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  ylab("VWC")+
  xlab("")

decagon10

#plot for andy 20 m soil moisture
decagon20 <- ggplot(data=decagon5_20, aes(y=value, x=DATETIME, color=variable))+
  geom_point( size=4)+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  #scale_y_continuous(limits=c(0,0.6), expand = c(0,0))+
  scale_fill_discrete(name="DEPTH",
                      breaks=c("ATEN50", "ATEN20", "ATEN0", "ATWENTY50", "ATWENTY20", "ATWENTY0"),
                      labels=c("10m (50cm)", "10m (20cm)", "10m(5cm)", "20m (50cm)", "20m (20cm)", "20m(5cm)" ))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme( legend.position="none" )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  ylab("VWC")+
  xlab("")

decagon20

#meth
methane <-read.csv(file="c:\\Users\\Jeff\\Documents\\R\\DATA\\meth.csv",head=FALSE)

lm.meth <- lm(methane$V1 ~ methane$V2)
summary.lm(lm.meth)


#redox
field$REDOX <- rowMeans(field[,9:10])
                
pREDOX <-ggplot(data=field, aes(y=REDOX, x=THETA, color=DISTANCE)) +
  geom_point(size=10)+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme( legend.justification=c(1,1), legend.position=c(1,1), legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
         legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  xlab("VWC")
  

pREDOX

  lm.REDOX <-lm(field$REDOX ~ field$THETA)
summary.lm(lm.REDOX)
                