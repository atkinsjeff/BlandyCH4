require(magrittr)
require(tidyr)

df <-total.field3

df.0 <- subset(df, DISTANCE == 0)
df.10 <- subset(df, DISTANCE == 10)
df.20 <- subset(df, DISTANCE == 20)

x11()
ggplot(df.0, aes(x = jd, y = ch4.flux))+
     geom_point()

x11()
ggplot(df.10, aes(x = jd, y = ch4.flux))+
     geom_point()

x11()
ggplot(df.20, aes(x = jd, y = ch4.flux))+
     geom_point()

set.seed(20)

q <- seq(from=0, to=20, by=0.1)
Value to predict (y):
     
     y <- 500 + 0.4 * (q-10)^3
Some noise is generated and added to the real signal (y):
     
     noise <- rnorm(length(q), mean=10, sd=80)
noisy.y <- y + noise
Plot of the noisy signal:
     
     plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
lines(q,y,col='firebrick1',lwd=3)


# model for meth at 0 distance.
model.ch4.0 <- lm(ch4.flux ~ poly(jd, 3), data = df.0)
summary(model.ch4.0)

confint(model.ch4.0, level = 0.95)

plot(fitted(model.ch4.0), residuals(model.ch4.0))

predicted.intervals <- predict(model.ch4.0 ,df.0,interval='confidence',
                               level=0.99)
#Add lines to the existing plot:
x11()
plot(df.0$jd, df.0$ch4.flux, col='deepskyblue4',xlab='Julian Day', ylab = "Methane Flux (nmol)", main='Methane Flux at 0 m')
lines(df.0$jd, df.0$ch4.flux,col='firebrick1',lwd=3)
     lines(df.0$jd,predicted.intervals[,1],col='green',lwd=3)
lines(df.0$jd,predicted.intervals[,2],col='black',lwd=1)
lines(df.0$jd,predicted.intervals[,3],col='black',lwd=1)
     legend("bottomright",c("Observ.","Signal","Predicted"), 
            col=c("deepskyblue4","red","green"), lwd=3)

# model for meth at 10 distance.
     model.ch4.10 <- lm(ch4.flux ~ poly(jd, 3), data = df.10)
     summary(model.ch4.10)
     
     confint(model.ch4.10, level = 0.95)
     
     plot(fitted(model.ch4.10), residuals(model.ch4.10))
     
     predicted.intervals <- predict(model.ch4.10 ,df.10,interval='confidence',
                                    level=0.99)
     #Add lines to the existing plot:
     x11()
     plot(df.10$jd, df.10$ch4.flux, col='deepskyblue4',xlab='Julian Day', ylab = "Methane Flux (nmol)", main='Methane Flux at 10 m')
     lines(df.10$jd, df.10$ch4.flux,col='firebrick1',lwd=3)
     lines(df.10$jd,predicted.intervals[,1],col='green',lwd=3)
     lines(df.10$jd,predicted.intervals[,2],col='black',lwd=1)
     lines(df.10$jd,predicted.intervals[,3],col='black',lwd=1)
     legend("bottomright",c("Observ.","Signal","Predicted"), 
            col=c("deepskyblue4","red","green"), lwd=3)
     
     # model for meth at 20 distance.
     model.ch4.20 <- lm(ch4.flux ~ poly(jd, 3), data = df.20)
     summary(model.ch4.20)
     
     confint(model.ch4.20, level = 0.95)
     
     plot(fitted(model.ch4.20), residuals(model.ch4.20))
     
     predicted.intervals <- predict(model.ch4.20 ,df.20,interval='confidence',
                                    level=0.99)
     #Add lines to the existing plot:
     x11()
     plot(df.20$jd, df.20$ch4.flux, col='deepskyblue4',xlab='Julian Day', ylab = "Methane Flux (nmol)", main='Methane Flux at 20 m')
     lines(df.20$jd, df.20$ch4.flux,col='firebrick1',lwd=3)
     lines(df.20$jd,predicted.intervals[,1],col='green',lwd=3)
     lines(df.20$jd,predicted.intervals[,2],col='black',lwd=1)
     lines(df.20$jd,predicted.intervals[,3],col='black',lwd=1)
     legend("bottomright",c("Observ.","Signal","Predicted"), 
            col=c("deepskyblue4","red","green"), lwd=3)     
