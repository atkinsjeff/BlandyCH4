# Working with met data
require(lubridate)
require(ggplot2)
#importing all the BLANDY data
met.data <- read.csv(file = "blandy_met_data.csv", header = TRUE)

met.data$date <- as.Date(met.data$date, format = "%m/%d/%Y")

x11()
plot(met.data$date, met.data$snow)

met.data$year <- lubridate::year(met.data$date)
df <- subset(met.data, year == 2014)

plot(df$date, df$max_temp)

#creating a time series of max temp
m.max.temp <- loess(max_temp ~ as.numeric(date), data = df)
summary(m.max.temp)


x11(width = 10, height = 3)
ggplot(df, aes(x = date, y = max_temp))+
     geom_point()+
     geom_smooth()+
     theme_minimal()+
     ylab("Maximum Temperature (C)")+
     xlab("")+
     ylim(c(-20,40))

x11(width = 10, height = 3)
ggplot(df, aes(x = date, y = min_temp))+
     geom_point()+
     geom_smooth()+
     theme_minimal()+
     ylab("Minimum Temperature (C)")+
     xlab("")+
     ylim(c(-20,40))

x11(width = 10, height = 3)
ggplot(df, aes(x = date, y = precip))+
     geom_bar(stat = "identity")+
     theme_minimal()+
     ylab("Precipitation (in)")+
     xlab("")

x11(width = 10, height = 3)
ggplot(df, aes(x = date, y = usgs_well))+
     geom_line()+
     theme_minimal()+
     ylab("USGS Well (ft)")+
     xlab("")+
     scale_y_reverse()

# x <- df$precip
# x <- na.omit(x)
# sum(x)
# 
# 
# 
radio <- read.csv(file = "radio.csv", header = TRUE)

#cleaning up radio met tower data
radio$date <- substring(radio$date, 2)

radio$date <- as.POSIXct(radio$date)
x11(width = 10, height = 3)
ggplot(radio, aes(x = date, y = soil_temp_t1))+
     geom_point()+
     geom_smooth()+
     theme_minimal()+
     ylab("soil temperature (C) @ 5 cm ")+
     xlab("")

x11(width = 10, height = 3)
ggplot(radio, aes(x = date, y = soil_temp_c1))+
     geom_point()+
     geom_smooth()+
     theme_minimal()+
     ylab("soil temperature (C) @ 12 cm ")+
     xlab("")

radio$soildiff <- radio$soil_temp_c1 - radio$soil_temp_t1

x11(width = 10, height = 3)
ggplot(radio, aes(x = date, y = soildiff))+
     geom_point()+
     geom_smooth()+
     theme_minimal()+
     ylab("Difference in temperature between soil depths")+
     xlab("")


