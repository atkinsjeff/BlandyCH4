#setwd("C:/Users/Jeff/Documents/R/DATA")
setwd("C:/Users/jeffatkins81/Google Drive/R/DATA")

#this loads the table header=true tells it that there is a header line that provides column names

methane <- read.csv("methane_raw_data_revised.csv", header=TRUE, fill=TRUE)

#structuring data
methane$distance <- as.factor(methane$distance)
methane$date <- as.Date(methane$date, format = "%m/%d/%Y")

#Methane standard curve from GC
volume <- c(0.1, 0.1, 0.25, 0.25, 0.3, 0.5, 0.7,1, 1,1,1)
area <- c(4848052,7597310,14249390,14571654,16513161,17666470,22118032,23536344,26729032,26925110,27679382)

ch4 <- data.frame(volume, area)


#coef(ch4.lm)

#24451.219 = molar density of methane in micromoles per liter
#0.05 is the percent of the standard volume that was methane (5% standard)
ch4$umols <- ((24451.219) * ((ch4$volume)*0.05))

#Calculationg molar equation for chromatograph area
ch4.lm <- lm(data=ch4,umols ~ area + 0)
summary(ch4.lm)

XX <- coef(ch4.lm)

plot(ch4$area, ch4$umols)


########################
methane <-na.omit(methane)
methane$umols <-methane$area * XX
methane_sort <- methane[order(methane$date, methane$transect, methane$distance, methane$minute),]

meth <- subset(methane_sort, select = -c(note))

#making a unique plot id based on converting date to julian day and using transect and distance
library(lubridate)
library(reshape)
meth$jd <- yday(meth$date)
meth$x <- substr(meth$transect,1,1)

meth$id <-paste(meth$jd,meth$x,as.character(meth$distance))
meth$id <- as.factor(meth$id)

#lm(data=meth, umols ~ minute, subset=id)



by(meth, meth[,"id"],
   function(x) lm(umols ~ minute, data = x))

                # trying lmList
library(nlme)

meth.list <- lmList(umols ~ minute | id,data=meth)
df.meth.list1 <-coef(meth.list)     #converting this to list
meth.summary <- summary(meth.list)
#by(meth, meth[,"id"],
#   function(x) plot(meth, tmp$minute, tmp$umols))

## now suppose we want to extract the coefficients by group
tmp <- with(meth,
            by(meth,id,
               function(x) lm(umols ~ minute, data = x)))
         
lm.fit.meth <- lapply(tmp, coef)
lm.sum.meth <- lapply(tmp, summary)

#trying to unpack the list from lm.fit.meth into a dataframe
df.meth <- data.frame(matrix(unlist(lm.fit.meth), byrow=F))
do.call(rbind.data.frame, lm.fit.meth)



df.meth.lm <- data.frame(t(sapply(lm.fit.meth,c)))
df.meth.lm <- cbind(id = rownames(df.meth.lm), df.meth.lm)
df.meth.lm

df.meth.lm <-rename(df.meth.lm, c("minute" = "slope"))

df.meth.lm$ch4.flux <- (df.meth.lm$slope / 0.070544) * 16.66666667

df.meth.lm.sum <- data.frame(t(sapply(lm.sum.meth,c)))

write.csv(df.meth.lm, file="meth_lm_output_revised.csv", row.names=TRUE)

#chamberarea <- 0.0705445 #meters squared this is the footprint of the chamber


library(ggplot2)
makeScatterplots <- function(dataframe,x.variable, y.variable, my.factor){
  print(ggplot(dataframe, aes_string(x=x.variable,y= y.variable, 
                                     group=my.factor, colour=my.factor)) + geom_point())  
}

makeScatterplots(meth, "minute","umols", "id")

makeScatterplots2 <- function(dataframe,x.variable, y.variable, my.factor){
  gg<-expand.grid(x.variable,y.variable,my.factor)
  gg<-data.frame(lapply(gg, as.character), stringsAsFactors=FALSE)
  apply(gg,1,function(x) ggplot(data,aes_string(x=x[1],y=x[2],color=x[3]))+geom_point())
}

makeScatterplots2(meth, "minute","umols", "id")


df1 <- subset(meth, meth$id=="198 a 0")
p.198.a.0<- ggplot(df1, aes(x=minute, y=umols))+
  geom_point()

p.198.a.0


df2 <- subset(meth, meth$id=="198 a 10")
p.198.a.10<- ggplot(df2, aes(x=minute, y=umols))+
  geom_point()

p.198.a.10


df3 <- subset(meth, meth$id=="198 a 20")
p.198.a.20<- ggplot(df3, aes(x=minute, y=umols))+
  geom_point()

p.198.a.20

df4 <- subset(meth, meth$id=="198 b 10")
p.198.b.10<- ggplot(df4, aes(x=minute, y=umols))+
  geom_point()

p.198.b.10

df5 <- subset(meth, meth$id=="206 a 0")
p.206.a.0<- ggplot(df5, aes(x=minute, y=umols))+
  geom_point()
p.206.a.0

df6 <- subset(meth, meth$id=="206 a 20")
p.206.a.20<- ggplot(df6, aes(x=minute, y=umols))+
  geom_point()
p.206.a.20


p.all<- ggplot(meth, aes(x=minute, y=umols))+
  geom_point()+
  geom_smooth(method=lm)+
  facet_grid(id~.)





