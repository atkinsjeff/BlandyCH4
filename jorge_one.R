#jorge

rm(list=ls())

master<-read.csv(file="c:\\Users\\Jeff\\Documents\\R\\DATA\\jorge.csv",head=TRUE)
master <-data.frame(master)




####normality tests
shapiro.test(master$T5)
qqnorm(master$T5)

shapiro.test(master$T12)
qqnorm(master$T12)

shapiro.test(master$T20)
qqnorm(master$T20)

shapiro.test(master$pH)
qqnorm(master$pH)

shapiro.test(master$THETA12)
qqnorm(master$THETA12)

shapiro.test(master$THETA20)
qqnorm(master$THETA20)


#reformatting date column into date format
master$Date <-as.Date(master$Date, format="%m/%d/%Y")
master$DISTANCEX <- factor(master$DISTANCE)
#Linear regression of Lumbricus abundance against soil pH
lum.regress <- lm( master$Lumbricus ~ master$pH)
summary.lm(lum.regress)

library(ggplot2)

#plot Lumbricus vs soil pH at 5 cm
pTEST <- ggplot(master, aes(x=THETA20, y=Eisenia)) +
  geom_point(size=15, alpha=0.5, shape=1, colour="black") 
pTEST

testreg <- lm(master$pH ~ master$THETA20)
summary.lm(testreg)

pHreg <- lm(master$pH ~ master$DISTANCE)
summary.lm(pHreg)

lum.time.regress <-(master$T12 ~ lm(as.numeric(master$DISTANCE)))
summary(lum.time.regress)

andy <- subset(master, TRANSECT=="A")
bob <- subset(master, TRANSECT== "B")
carl <- subset(master, TRANSECT== "C")
dan <- subset(master, TRANSECT== "D")

testing <- master
testing <- na.omit(testing)
test2 <- c(master$pH, master$DISTANCE)


test5 <- testing[c("DISTANCE","pH")]
#PH graph
label1 = paste("R^2 == 0.5192")   #R2 pH against distance


ppH <- ggplot(master, aes(x=DISTANCE, y=pH))+
  geom_point( size=10, color="#002244")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(5,8), expand = c(0,0))+
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
  geom_smooth(method=lm, se=FALSE, size=2, color="#c60c30")+
  annotate("text",x=20, y=7.6, label=label1, parse=TRUE, size =12)+
  annotate("text",x=20, y=7.25, label= "y==-0.0452*x + 7.013 ", parse=TRUE, size = 12)
  xlab("Distance (m)")
  
ppH

#temp Graph
pT5 <- ggplot(master, aes(x=DISTANCE, y=T5))+
  geom_point( size=10, color="#002244")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  #scale_y_continuous(limits=c(15,30), expand = c(0,0))+
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
  geom_smooth(method=lm, se=FALSE, size=2, color="#c60c30")+
  #annotate("text",x=20, y=7.6, label=label1, parse=TRUE, size =12)+
  #annotate("text",x=20, y=7.25, label= "y==-0.0452*x + 7.013 ", parse=TRUE, size = 12)
xlab("Distance (m)")

pT5

pTHETA20 <- ggplot(master, aes(x=DISTANCE, y=THETA20))+
  geom_point( size=10, color="#002244")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  #scale_y_continuous(limits=c(15,30), expand = c(0,0))+
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
  geom_smooth(method=lm, se=FALSE, size=2, color="#c60c30")+
  #annotate("text",x=20, y=7.6, label=label1, parse=TRUE, size =12)+
  #annotate("text",x=20, y=7.25, label= "y==-0.0452*x + 7.013 ", parse=TRUE, size = 12)
  xlab("Distance (m)")

pTHETA20

pTHETA12 <- ggplot(master, aes(x=DISTANCE, y=THETA12))+
  geom_point( size=10, color="#002244")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  #scale_y_continuous(limits=c(15,30), expand = c(0,0))+
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
  geom_smooth(method=lm, se=FALSE, size=2, color="#c60c30")+
  #annotate("text",x=20, y=7.6, label=label1, parse=TRUE, size =12)+
  #annotate("text",x=20, y=7.25, label= "y==-0.0452*x + 7.013 ", parse=TRUE, size = 12)
  xlab("Distance (m)")

pTHETA12

ppH <- ggplot(master, aes(x=DISTANCE, y=pH))+
  geom_point( size=10, color="#002244")+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(5,8), expand = c(0,0))+
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
  geom_smooth(method=lm, se=FALSE, size=2, color="#c60c30")+
  annotate("text",x=20, y=7.6, label=label1, parse=TRUE, size =12)+
  annotate("text",x=20, y=7.25, label= "y==-0.0452*x + 7.013 ", parse=TRUE, size = 12)
xlab("Distance (m)")

ppH
pHANDY <- ggplot(andy, aes(x=Date, y=pH, color=DISTANCE))+
  geom_point( size=10)+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(5,8), expand = c(0,0))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme( legend.position="bottom", legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
         legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  theme(axis.title.x = element_blank())

pHANDY

pHBOB <- ggplot(bob, aes(x=Date, y=pH, color=DISTANCE))+
  geom_point( size=10)+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(5,8), expand = c(0,0))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme( legend.position="bottom", legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
         legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  theme(axis.title.x = element_blank())

pHBOB

pHCARL <- ggplot(carl, aes(x=Date, y=pH, color=DISTANCE))+
  geom_point( size=10)+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(5,8), expand = c(0,0))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme( legend.position="bottom", legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
         legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  theme(axis.title.x = element_blank())

pHCARL

pHDAN<- ggplot(dan, aes(x=Date, y=pH, color=DISTANCE))+
  geom_point( size=10)+
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(5,8), expand = c(0,0))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme( legend.position="bottom", legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
         legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  theme(axis.title.x = element_blank())

pHDAN

#multiplot command to put all together

##################################################
#multiplot function
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#####################################
multiplot(pHANDY, pHBOB, pHCARL, pHDAN, cols=2)
#subsetting for data calculation for further analysis
ONE <- subset(master, PERIOD == 1)

AONE <- subset(ONE, TRANSECT== "A")
BONE <- subset(ONE, TRANSECT== "B")
CONE <- subset(ONE, TRANSECT== "C")
DONE <- subset(ONE, TRANSECT== "D")

colSums(Filter(is.numeric, AONE))
colSums(Filter(is.numeric, BONE))
colSums(Filter(is.numeric, CONE))
colSums(Filter(is.numeric, DONE))

TWO <- subset(master, PERIOD == 2)

ATWO <- subset(TWO, TRANSECT== "A")
BTWO <- subset(TWO, TRANSECT== "B")
CTWO <- subset(TWO, TRANSECT== "C")
DTWO <- subset(TWO, TRANSECT== "D")

colSums(Filter(is.numeric, ATWO))
colSums(Filter(is.numeric, BTWO))
colSums(Filter(is.numeric, CTWO))
colSums(Filter(is.numeric, DTWO))

THREE <- subset(master, PERIOD == 3)

ATHREE <- subset(THREE, TRANSECT== "A")
BTHREE <- subset(THREE, TRANSECT== "B")
CTHREE <- subset(THREE, TRANSECT== "C")
DTHREE <- subset(THREE, TRANSECT== "D")

colSums(Filter(is.numeric, ATHREE))
colSums(Filter(is.numeric, BTHREE))
colSums(Filter(is.numeric, CTHREE))
colSums(Filter(is.numeric, DTHREE))


#Further working diversity
diverse<-read.csv(file="c:\\Users\\Jeff\\Documents\\R\\DATA\\species_diversity.csv",head=TRUE)
diverse <- data.frame(diverse)

library(ggplot2)

pDiversity <- ggplot(data=diverse, aes(x=PERIOD, y=H, fill=TRANSECT)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(0,1.6), expand = c(0,0))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
        legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())
 
pDiversity

pEven <- ggplot(data=diverse, aes(x=PERIOD, y=Even, fill=TRANSECT)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))+
  #scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(limits=c(0,1.1), expand = c(0,0))+
  theme_classic()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.text=element_text(color='black', size=24), legend.background = element_rect( fill="white"), 
        legend.key=element_rect(color="white", fill="white"), legend.title=element_blank() )+
  theme(panel.border = element_rect(colour="black", fill=NA))+
  theme(axis.title.x= element_text(size=32), 
        axis.text.x = element_text(size=(24)))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24)))+
  theme(axis.ticks = element_blank())+
  ylab("EVENNESS")

pEven



