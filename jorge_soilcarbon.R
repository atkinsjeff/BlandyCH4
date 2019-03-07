#Jorge worm project soil carbon
require(ggplot2)
require(dplyr)
require(plyr)
require(wesanderson)
#data directory

data_dir <- "./data/"

soildata<- read.csv(paste0(data_dir, "jorge_carbon.csv"), header = TRUE, strip.white = TRUE)


reg.carbon <- lm(soildata$C ~ soildata$DISTANCE)

summary.lm(reg.carbon)


#soil carbon graphs


library(ggplot2)
label1 = paste("R^2 == 0.5192")   #R2 pH against distance

x11()
 ggplot(data=soildata, aes(x=DISTANCE, y=C, color=TRANSECT)) + 
  geom_point(size=5)+
  scale_color_manual(values= wes_palette("Darjeeling"))+
  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(0,4), expand = c(0,0))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(legend.justification=c(1,1), legend.position=c(0.95,0.95), legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
        legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
#   geom_smooth(method=lm, se=FALSE, size=2, color="#c60c30")+
#   annotate("text",x=10, y=1, label=label1, parse=TRUE, size =12)+
#   annotate("text",x=20, y=7.25, label= "y==-0.0452*x + 7.013 ", parse=TRUE, size = 12)
xlab("Distance (m)")+
  ylab("Total Soil C (%)")

x11()
ggplot(data=soildata, aes(x=DISTANCE, y=N, color=TRANSECT)) + 
  geom_point(size=5)+
  scale_color_manual(values= wes_palette("Darjeeling"))+
  #scale_x_discrete(expand=c(0,0))+
  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(0,0.5), expand = c(0,0))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(legend.justification=c(1,1), legend.position=c(0.95,0.95), legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
        legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  #   geom_smooth(method=lm, se=FALSE, size=2, color="#c60c30")+
  #   annotate("text",x=10, y=1, label=label1, parse=TRUE, size =12)+
  #   annotate("text",x=20, y=7.25, label= "y==-0.0452*x + 7.013 ", parse=TRUE, size = 12)
  xlab("Distance (m)")+
  ylab("Total Soil N (%)")

pNITROGEN

#working with bulk density graphs
bulk<-read.csv(file= paste0(data_dir, "jorge_bulk.csv"), head=TRUE,strip.white=TRUE)

x11()
ggplot(data=bulk, aes(x=DISTANCE, y=BULK, color=TRANSECT)) + 
  geom_point(size=5)+
  scale_color_manual(values= wes_palette("Darjeeling"))+
  #scale_x_discrete(expand=c(0,0))+
  #scale_y_continuous(limits=c(0,3.25), expand = c(0,0))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(legend.justification=c(1,1), legend.position=c(0.95,0.95), legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
        legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
#   labs( (x =expression("Distance m"),
#          y = expression("Soil Bulk Density g cm^3"))
ylab(expression("Soil Bulk Density "*"("*"g cm"^3 * ")")  )+
  xlab("Distance (m)")
pBULK



#JOrge SOM data
som <- read.csv(file = paste0(data_dir, "jorge_som.csv"), head=TRUE, strip.white=TRUE)
som$SOM <- som$SOM * 100
x11()
 ggplot(data=som, aes(x=DISTANCE, y=SOM, color=TRANSECT)) + 
  geom_point(size=5)+
  scale_color_manual(values= wes_palette("Darjeeling"))+  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(0,10), expand = c(0,0))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(legend.justification=c(1,1), legend.position=c(0.95,0.95), legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
        legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  #   labs( (x =expression("Distance m"),
  #          y = expression("Soil Bulk Density g cm^3"))
  ylab("SOM (%)" )+
  xlab("Distance (m)")
pSOM

