#blandy field data
require(ggplot2)
require(dplyr)
require(plyr)
require(wesanderson)
#data directory

data_dir <- "./data/"
#this loads the table header=true tells it that there is a header line that provides column names

#plot labels
fluxlabel = expression(paste(F[SOIL]~(mu ~mol ~CO[2] ~m^-2 ~s^-1)), parse=TRUE)
vwclabel = expression(paste(VWC~(m^3~m^-3)))
templabel = expression(paste(T[SOIL] ~(degree~C)), parse=TRUE )



field <- read.csv( file = paste0(data_dir, "ATKINS_FIELD_DATA.csv"), header=TRUE, fill=TRUE)

str(field)

field$DISTANCE <-as.factor(field$DISTANCE)
field$DATE <- as.Date(as.character(field$DATE), format= "%m/%d/%Y")

#converting redox to mV and adding correction factor (+200 mV due to msmts taken with AgCl probe)
field$REDOX1 <- (field$REDOX1 * 1000) + 200
field$REDOX2 <- (field$REDOX2 * 1000) + 200
field$REDOX5 <- (field$REDOX5 * 1000) + 200
field$REDOX20 <- (field$REDOX20 * 1000) + 200

x11()
p.CO2_T5 <- ggplot(field, aes(x=T5, y=FLUX, color=DISTANCE))+
  geom_point(size=8)

p.CO2_T12 <- ggplot(field, aes(x=T12, y=FLUX, color=DISTANCE))+
  geom_point(size=8)



p.CO2_T5
p.CO2_T12
p.CO2_T20

field2 <- field
field2$DISTANCE[field2$DISTANCE == 25] <- NA
field2 <- subset(field2, !is.na(field2$DISTANCE))
field2$DISTANCE <- paste(field2$DISTANCE, "m", sep=" ")
field2$DISTANCE <- as.factor(field2$DISTANCE)
#order the factors
field2$DISTANCE <- factor(field2$DISTANCE, levels(field2$DISTANCE)[c(1,5,2:4)])

p.CO2_THETA <- ggplot(field2, aes(x=THETA, y=FLUX))+
  geom_point(shape=1, size =4)+
     theme_bw()+
     theme(axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20))+
     ylab(fluxlabel)+
     xlab(vwclabel)+
     stat_smooth(method = lm)
x11()
p.CO2_THETA

# let's look at some stats
m.theta <- lm(FLUX ~ THETA, data = field2)
summary(m.theta)

p.CO2_THETA + facet_grid(DISTANCE ~ .)+
     theme(strip.text.y = element_text(size=20))

p.CO2_T20 <- ggplot(field2, aes(x=T20, y=FLUX))+
     geom_point(size=4)+
     theme_bw()+
     theme(axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20))+
     ylab(fluxlabel)+
     xlab(templabel)
x11()
p.CO2_T20

p.CO2_T20 + facet_grid(DISTANCE ~ .)+
     theme(strip.text.y = element_text(size=20))

####
p.CO2_redox <- ggplot(field2, aes(x=REDOX20, y=FLUX))+
     geom_point(size=4)+
     theme_bw()+
     theme(axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20))+
     ylab(fluxlabel)+
     xlab("Redox Potential (mv)")

x11()
p.CO2_redox

m.redox <- lm(FLUX ~ REDOX20, data = field2)
summary(m.redox)

p.CO2_redox + facet_grid(DISTANCE ~ .)+
     theme(strip.text.y = element_text(size=20))

p.CO2_time <- ggplot(field, aes(x=DATE, y=FLUX))+
  geom_point(size=6)+
  geom_smooth(method=lm, SE=FALSE)+
  scale_y_continuous(limits= c(0,20), expand=c(0,0))+
  theme_bw()+
  labs(y="")+
  labs(x="")+
  theme(axis.text.x = element_text(size=(20), colour="black"))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24), colour="black"),
        strip.text.x= element_text(size = (24), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_grid(TRANSECT~DISTANCE)

x11()
p.CO2_time

#putting redox against CO2
p.redox5 <- ggplot(field, aes(y=FLUX, x=REDOX5))+
     geom_point(size=4)+
     theme_bw()+
     theme(axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20))+
     ylab(fluxlabel)+
     xlab("Redox Potential (mv) at 5 cm")

x11()
p.redox5


p.theta <- ggplot(field, aes(x=DATE, y=THETA, color=as.factor(DISTANCE)))+
  geom_line(size=2)+
  
  scale_y_continuous(limits= c(0,100), expand=c(0,0))+
  theme_bw()+
  labs(y=vwclabel)+
  labs(x="")+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.title.y= element_text(size=24), 
        axis.text.y = element_text(size=(24), colour="black"),
        strip.text.x= element_text(size = (24), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
x11()
p.theta

p.flux <- ggplot(field, aes(x=T5, y=FLUX))+
  geom_point(size=8, color="darkorange")+
  scale_y_continuous(limits= c(0,20), expand=c(0,0))+
  theme_bw()+
  labs(y=fluxlabel)+
  labs(x=templabel)+
  theme(axis.text.x = element_text(size=(20), colour="black"))+
  theme(axis.title.y= element_text(size=24), 
        axis.text.y = element_text(size=(20), colour="black"),
        axis.title.x= element_text(size = (24), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p.flux
library(grid)
library(gridExtra)
grid.arrange(p.redox20, p.theta, p.flux)

shapiro.test(field$REDOX5)
hist(field$REDOX5)


#bringing in the meth
field.meth <- read.csv(paste0(data_dir, "meth_lm_output.csv"), header=TRUE, fill=TRUE)
field.meth2 <- read.csv(paste0(data_dir, "meth_lm_output_revised.csv"), header=TRUE, fill=TRUE)
 
library(lubridate)
library(reshape)
field$jd <- yday(field$DATE)
field$x <- substr(field$TRANSECT,1,1)

field$id <-paste(field$jd,tolower(field$x),as.character(field$DISTANCE))
field$id <- as.factor(field$id)
total.field <- merge(field.meth, field, by = "id", all=TRUE)
total.field2 <-merge(field.meth2, field, by = "id", all=TRUE)

#removing likely high value
threshold <- 1000
total.field2 <- subset(total.field2, total.field2[ , 5] < threshold)

#graph labels
fluxlabel = expression(paste(F[SOIL]~(mu ~mol ~CO[2] ~m^-2 ~s^-1)), parse=TRUE)
ch4label = expression(paste(CH[4]~(nmol ~CH[2] ~m^-2 ~s^-1)), parse=TRUE)
templabel = expression(paste(T[SOIL] ~(degree~C)), parse=TRUE )
vwclabel = expression(paste(VWC~(m^3~m^-3)))
redoxlabel = expression(paste(redox ~potential~(mV)))

p.ch4flux <- ggplot(total.field2, aes(x=ch4.flux, y=FLUX, color=DISTANCE))+
  geom_point(size=8)+
  scale_y_continuous(limits= c(0,20), expand=c(0,0))+
  theme_bw()+
  labs(y=fluxlabel)+
  labs(x=ch4label)+
  theme(axis.text.x = element_text(size=(20), colour="black"))+
  theme(axis.title.y= element_text(size=24), 
        axis.text.y = element_text(size=(20), colour="black"),
       axis.title.x= element_text(size = (24), colour="black"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
x11()
p.ch4flux


###ch4 against theta
p.CH4_THETA2 <- ggplot(total.field2, aes(x=THETA, y=ch4.flux))+
     geom_point(size=4, shape =1)+
     theme_bw()+
     theme(axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20))+
     ylab(ch4label)+
     xlab(vwclabel)

x11()
p.CH4_THETA2

p.CH4_THETA2 + facet_grid(DISTANCE ~ .)+
     theme(strip.text.y = element_text(size=20))

p.CH4_TEMP <- ggplot(total.field2, aes(x=T20, y=ch4.flux))+
     geom_point(size=4, shape =1)+
     theme_bw()+
     theme(axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20))+
     ylab(ch4label)+
     xlab(templabel)
x11()
p.CH4_TEMP

p.CH4_R20 <- ggplot(total.field2, aes(x=REDOX20, y=ch4.flux))+
     geom_point(size=4, shape =1)+
     theme_bw()+
     theme(axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20))+
     ylab(ch4label)+
     xlab("Redox Potential (mV)")+
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())+
     xlim(0,800)+
     annotate("text", x = 700, y = 800, label = "20 cm", size = 12)

p.CH4_R20

p.CH4_R5 <- ggplot(total.field2, aes(x=REDOX5, y=ch4.flux))+
     geom_point(size=4, shape =16)+
     theme_bw()+
     theme(axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20))+
     ylab(ch4label)+
     xlab("Redox Potential (mV)")+
     theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank())+
     xlim(0,800)+
     annotate("text", x = 700, y = 800, label = "5 cm", size = 12)

p.CH4_R5
# a smattering of possible models... just made up on the spot
# with more effort some better candidates should be added
# a smattering of possible models...
models <- list(lm(y~x, data = total.field), 
               lm(y~I(1/x), data=total.field),
               lm(y ~ log(x), data = dat),
               nls(y ~ I(1/x*a) + b*x, data = dat, start = list(a = 1, b = 1)), 
               nls(y ~ (a + b*log(x)), data=dat, start = setNames(coef(lm(y ~ log(x), data=dat)), c("a", "b"))),
               nls(y ~ I(exp(1)^(a + b * x)), data=dat, start = list(a=0,b=0)),
               nls(y ~ I(1/x*a)+b, data=dat, start = list(a=1,b=1))
)

selected <-c("0","5","10","15","20")

total.field3 <-total.field2[total.field2$DISTANCE %in% selected,]
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

rgb.palette <- colorRampPalette(c("red", "orange", "blue"),
                                space = "rgb")
p.ch4time <- ggplot(total.field2, aes(x=DATE, y=ch4.flux, color=THETA))+
  geom_point(size=12)+
  #scale_y_continuous(limits= c(-200,900), expand=c(0,0))+
#   theme_bw()+
  labs(y=ch4label)+
  labs(x="")+
  ylim(-25, 1000)+
  #scale_colour_gradient(values=timcolors(64))+
scale_colour_gradient("VWC", limits=c(20, 70), low="orangered", high="blue1", space="rgb")+
  theme(axis.text.x = element_text(size=(20), colour="black"))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24), colour="black"),
        strip.text.x= element_text(size = (24), colour="black"))+
  theme(legend.justification=c(1,1), 
        legend.position=c(1,1),
        legend.title=element_text(color='black', size=24),
        legend.text=element_text(color='black', size=16))
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())+
  geom_hline(aes(yintercept=0))
  #stat_smooth(method = "nls", formula = , data=total.field, start = list(a=0,b=0), size = 1, se = FALSE, colour = "red")  
x11()
p.ch4time

p.ch4fluxtime <- ggplot(total.field3, aes(x=DATE, y=FLUX, color=THETA))+
  geom_point(size=12)+
  #scale_y_continuous(limits= c(-200,900), expand=c(0,0))+
  #theme_bw()+
  labs(y=fluxlabel)+
  labs(x="")+
  scale_y_continuous(limits= c(0,15), expand=c(0,0))+
  #scale_colour_manual(values=cbPalette)+
  scale_colour_gradient("VWC", limits=c(20, 70), low="orangered", high="blue1", space="rgb")+
  theme(axis.text.x = element_text(size=(20), colour="black"))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24), colour="black"),
        strip.text.x= element_text(size = (24), colour="black"))+
  #facet_grid(DISTANCE ~ .)

  theme(legend.justification=c(1,1), 
        legend.position=c(1,1),
        legend.title=element_text(color='black', size=24),
        legend.text=element_text(color='black', size=16))
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
#stat_smooth(method = "nls", formula = , data=total.field, start = list(a=0,b=0), size = 1, se = FALSE, colour = "red")  
x11()
p.ch4fluxtime

#facetgrid
p.flux.time.facet<- ggplot(total.field2, aes(x=DATE, y=FLUX))+
  geom_point()+
  facet_grid(DISTANCE~.)

df.ch4 <- total.field2[ , c("ch4.flux", "jd")]

df.ch4 <- na.omit(df.ch4)

#fit first degree polynomial equation:
fit  <- lm(df.ch4$ch4.flux~df.ch4$jd)
#second degree
fit2 <- lm(df.ch4$ch4.flux~poly(df.ch4$jd,2,raw=TRUE))
#third degree
fit3 <- lm(df.ch4$ch4.flux~poly(df.ch4$jd,3,raw=TRUE))
#fourth degree
fit4 <- lm(df.ch4$ch4.flux~poly(df.ch4$jd,4,raw=TRUE))
#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(30,160, by=1)

#fitting nls model
model <- lm(df.ch4$ch4.flux ~ poly(df.ch4$jd, 3))

intervals <- predict(model, data.frame(x=df.ch4$jd),interval='confidence',
                     level=0.95)

plot(df.ch4$jd, df.ch4$ch4.flux, pch=19,ylim=c(-100,900))
     lines(df.ch4$jd, intervals[,1],col='green',lwd=2)
     

plot(df.ch4$jd,df.ch4$ch4.flux,pch=19,ylim=c(-100,900))
lines(predict(fit), col="red")
lines(xx, predict(fit2, data.frame(df.ch4$jd=xx)), col="green")
lines(xx, predict(fit3, data.frame(df.ch4$jd=xx)), col="blue")
lines(xx, predict(fit4, data.frame(df.ch4$jd=xx)), col="purple")



lines(xx, predict(fit, data.frame(df.ch4$jd=xx)), col="red")
lines(xx, predict(fit2, data.frame(df.ch4$jd=xx)), col="green")
lines(xx, predict(fit3, data.frame(df.ch4$jd=xx)), col="blue")
lines(xx, predict(fit4, data.frame(df.ch4$jd=xx)), col="purple")


#####REGRESSIONS
#REGRESSION FUNCTION
REGRESS <- function(x,a,b) {a *exp(b * x)} 

NEGEXP <- function(x,a,b) { a * exp(-b*x)}
#TEMPERATURE regressions for all data
fitREDOX <- nls(ch4.flux ~ NEGEXP(REDOX5,a,b), data = total.field2, start = c(a=0.5, b=0.5), trace=T) 
coeffTEMP <- coef(fitTEMP)
#Temperature curve
sTEMP<-seq(from=min(CVI$TEMP,na.rm=T),to=max(CVI$TEMP,na.rm=T),length=50)
curveTEMP <- data.frame(TEMP=sTEMP,FLUX=REGRESS(sTEMP, a=coeffTEMP[1], b=coeffTEMP[2]))

#Temp regressions for above and below
fitABOVE <- nls(FLUX ~ REGRESS(TEMP,a,b), data = CVI_ABOVE, start = c(a=1, b= 0.5), trace=T)
coeffABOVE <- coef(fitABOVE)


p.ch4redox <- ggplot(total.field2, aes(x=REDOX5, y=ch4.flux, color=THETA))+
  geom_point(size=12)+
  #scale_y_continuous(limits= c(-200,900), expand=c(0,0))+
#   theme_bw()+
  labs(y=ch4label)+
  ylim(-25, 1000)+
  labs(x="Redox Potential (mV)")+
  scale_colour_gradient(limits=c(20, 70), low="orangered", high="blue1", space="rgb")+
  theme(axis.text.x = element_text(size=(20), colour="black"))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24), colour="black"),
        axis.title.x= element_text(size = (32), colour="black"))+
  theme(legend.justification=c(1,1), 
        legend.position=c(1,1),
        legend.title=element_blank(),
        legend.text=element_text(color='black', size=24), 
        legend.background = element_rect( fill="#FFFFFF"), 
        legend.key=element_rect(color="#FFFFFF", fill="#FFFFFF") )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_hline(aes(yintercept=0))
#stat_smooth(method = "nls", formula = , data=total.field, start = list(a=0,b=0), size = 1, se = FALSE, colour = "red")  

p.ch4redox

p.ch4theta<- ggplot(total.field2, aes(x=THETA, y=ch4.flux))+
  geom_point(size=10, color="#dc3522")+
  #scale_y_continuous(limits= c(-200,900), expand=c(0,0))+
  theme_bw()+
  labs(y=ch4label)+
  ylim(-25, 1000)+
  #labs(x="redox potential (mV)")+
  theme(axis.text.x = element_text(size=(20), colour="black"))+
  theme(axis.title.y= element_text(size=32), 
        axis.text.y = element_text(size=(24), colour="black"),
        axis.title.x= element_text(size = (32), colour="black"))+
  theme(legend.justification=c(1,1), 
        legend.position=c(1,1),
        legend.title=element_blank(),
        legend.text=element_text(color='black', size=24), 
        legend.background = element_rect( fill="#FFFFFF"), 
        legend.key=element_rect(color="#FFFFFF", fill="#FFFFFF") )+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_hline(aes(yintercept=0))
#stat_smooth(method = "nls", formula = , data=total.field, start = list(a=0,b=0), size = 1, se = FALSE, colour = "red")  

p.ch4theta


#NEed to reorganize redox data
redox.data <- total.field2[,c("id", "REDOX5", "REDOX20")]
theta.data <- total.field2[, c("id", "THETA")]

require(reshape2)
redox.data <- melt(redox.data)

redox.data <- merge(redox.data, theta.data, by = c("id"))

redox.data$depth[redox.data$variable == "REDOX5"] <- "5 cm"
redox.data$depth[redox.data$variable == "REDOX20"] <- "20 cm"

redox.data$depth <- as.factor(redox.data$depth)

p.redox <- ggplot(redox.data, aes(x=THETA, y=value, shape=depth, linetype=depth))+
  geom_point(size=4)+
     scale_shape_manual(values = c(1,16))+
  labs(x=vwclabel)+
  labs(y="Redox Potential (mV)")+
  geom_smooth( method=lm, se=FALSE, color = "black")+
     theme_bw()+
     theme(axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20))+
     theme(axis.line = element_line(colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank(),
           panel.background = element_blank())+ 
     theme(axis.title.y=element_text(margin=margin(0,20,0,0)))+
     theme(legend.justification=c(1,1), legend.position=c(1,1))+
     guides(fill=guide_legend(title=NULL))

p.redox


p.redox.vs <- ggplot(total.field2, aes(x=REDOX5, y=REDOX20))+
     geom_point(size=5, shape = 1)+
     xlab("Redox Potential at 5 cm (mV)")+
     labs(y="Redox Potential at 20 cm (mV)")+
     geom_smooth( method=lm, se=FALSE, color = "black")+
     xlim(0,800)+
     ylim(0,800)+
     theme_bw()+
     theme(axis.text.x = element_text(size=20),
           axis.text.y = element_text(size=20),
           axis.title.x = element_text(size=20),
           axis.title.y = element_text(size=20))+
     theme(axis.line = element_line(colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank(),
           panel.background = element_blank())+ 
     theme(axis.title.y=element_text(margin=margin(0,20,0,0)))

p.redox.vs

##################################
# looking at just transect A distance 10

andy.10 <- subset(field, TRANSECT == "A" | DISTANCE == 10)

#############

#more statisitcal analysis

co2.fit <- lm(FLUX ~ THETA, data = field2)
summary(co2.fit )

ch4.fit <- lm(ch4.flux ~ THETA, data = total.field2)
summary(ch4.fit)

r5.fit <- lm(REDOX5 ~ THETA, data = total.field2)
summary(r5.fit)

r20.fit <- lm(REDOX20 ~ THETA, data = total.field2)
summary(r20.fit)

ch4.r5.fit <- lm(ch4.flux ~ REDOX5, data = total.field2)
summary(ch4.r5.fit)

ch4.r20.fit <- lm(ch4.flux ~ REDOX20, data = total.field2)
summary(ch4.r20.fit)

co2.r5.fit <- lm(FLUX ~ REDOX5, data = total.field2)
summary(co2.r5.fit)

co2.r20.fit <- lm(FLUX~ REDOX20, data = total.field2)
summary(co2.r20.fit)
 
redox.fit <- lm(REDOX5~ REDOX20, data = total.field2)
summary(redox.fit)

cor.test(total.field2$REDOX5, total.field2$THETA)
cor.test(total.field2$REDOX20, total.field2$THETA)


plot(total.field2$REDOX5, total.field2$REDOX20)

require(Hmisc)

cor.test(total.field2$REDOX20, total.field2$FLUX)
plot(total.field2$REDOX20, total.field2$FLUX)
p.redox + geom_abline(intercept=898.175, slope = -10.013, linetype = "dashed")+
     geom_abline(intercept=1062.241, slope = -13.102)+
     scale_linetype_discrete(name="", 
                             breaks=c("A", "B", "C"), 
                             labels = c("easy", "medium", "hard"))

p.CO2_THETA +
     theme(axis.line = element_line(colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank(),
           panel.background = element_blank()) +
     geom_smooth(method=lm, color="black", se=FALSE)

p.CH4_THETA2 +
     theme(axis.line = element_line(colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.border = element_blank(),
           panel.background = element_blank())

