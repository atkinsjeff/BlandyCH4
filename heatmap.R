#heatmap makin'

library(fields)
library(akima)

## space="Lab" helps when colors don't form a natural sequence
m <- outer(1:20,1:20,function(x,y) sin(sqrt(x*y)/3))
rgb.palette <- colorRampPalette(c("red",  "blue"),
                                space = "rgb")
Lab.palette <- colorRampPalette(c("red", " dark blue"),
                                space = "Lab")
filled.contour(m, col = rgb.palette(20))
filled.contour(m, col = Lab.palette(20))


x <- c(0,0,0,10,10,10,20,20,20)
y <- c(-5,-20,-50,-5,-20,-50,-5,-20,-50)
z.206 <- c(0.314,0.355,0.457,0.392,0.429,0.456,0.445,0.423,0.434)
z.226 <- c(0.205,0.344,0.457,0.457,0.434,0.458,0.428,0.431,0.442)
z.236 <- c(0.200,0.348,0.457,0.433,0.441,0.456,0.431,0.429,0.443)
z.250 <- c(0.200,0.346,0.457,0.200,0.402,0.455,0.428,0.441,0.446)

z.206 <- z.206*100
z.226 <- z.226*100
z.236 <- z.236*100
z.250 <- z.250*100

m.206 <- interp(x,y,z.206)
m.226 <- interp(x,y,z.226)
m.236 <- interp(x,y,z.236)
m.250 <- interp(x,y,z.250)
image.plot(m.206, zlim= c(20, 50), col = rev(tim.colors(64)))
image.plot(m.226, zlim= c(20, 50), col = rev(tim.colors(64)))
image.plot(m.236, zlim= c(20, 50), col = rev(tim.colors(64)))
image.plot(m.250, zlim= c(20, 50), col = rev(tim.colors(64)))

#neatly arranged multiplots

A<- matrix( 1:48, ncol=6)
# Note that matrix(c(A), ncol=6) == A
image.plot(1:8, 1:6, A)
# add labels to each box 
text( c( row(A)), c( col(A)), A)
# and the indices ...
text( c( row(A)), c( col(A))-.25,  
      paste( "(", c(row(A)), ",",c(col(A)),")", sep=""), col="grey")

# "columns" of A are horizontal and rows are ordered from bottom to top!
#
# matrix in its usual tabular form where the rows are y  and columns are x

image.plot( t( A[6:1,]), axes=FALSE)


#
#fat (5 characters wide) and short (50% of figure)  color bar on the bottom
image.plot( x,y,z,legend.width=5, legend.shrink=.5, horizontal=TRUE) 

set.panel()

# Here is quick but quirky way to add a common legend to several plots. 
# The idea is leave some room in the margin and then over plot in this margin

par(oma=c( 0,0,0,4)) # margin of 4 spaces width at right hand side
set.panel( 2,2) # 2X2 matrix of plots

# now draw all your plots using usual image command
for (  k in 1:4){
  image( matrix( rnorm(150), 10,15), zlim=c(-4,4), col=tim.colors())
}

par(oma=c( 0,0,0,1))# reset margin to be much smaller.
image.plot( legend.only=TRUE, zlim=c(-4,4)) 

# image.plot tricked into  plotting in margin of old setting 

set.panel() # reset plotting device