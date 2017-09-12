# a few usefull commands for installation and running in GNU/Linux (Ubuntu/Debian)
# java -version
# sudo apt-get install openjdk-6-jre icedtea6-plugin openjdk-6-jdk
# sudo update-java-alternatives -l
# sudo update-java-alternatives -s java-6-openjdk
# ### sudo apt-get install sun-java6-jdk # ne peut pas s'installer !?
# 
# sudo R CMD javareconf
# sudo R
# install.packages("iplots", dep=T)
# q()
# R
# help.start(br="firefox")


library(iplots)
# simple example with random data by MK
set.seed(5)
n<-100
x1<- round(abs(rnorm(n)*3)+1)
x4<- log(x1) + rnorm(n)/3
x3<- x1+x4 +  rnorm(n)
x2<- round((x4-1)^2 + rnorm(n)/3)
X <- data.frame(x1,x2, x3,x4)
#fix(X)
pairs(X)
plot(x1,x2,pch=".",cex=10)
sunflowerplot(x1,x2)

##########
### alternate, 20 countries
#######  Start here to share this code on r-developers mailing list
library(iplots)

set.seed(5)
n<-20
countries<- c("Angola", "Brazil", "CAR", "Denmark", "Ecuador", "France", "Gabon", "Haiti", "India", "Japan",
              "Kosovo", "Laos", "Mali", "Nepal", "Oman", "Portugal", "Qatar", "Rwanda", "Senegal", "Togo")
x1<- round(abs(rnorm(n)*3)+1)
x4<- log(x1) + rnorm(n)/3
x3<- x1 + x4 +  rnorm(n)
x2<- round((x4-1)^2 + rnorm(n)/3)
X <- data.frame(countries, x1, x2, x3, x4)
rm(x1, x2, x3, x4, countries)
##############################

attach(X)
ibar(countries) # then rotate with CTRL-R

## mosaic of iplots
# display.matrix() computes the position and size of iplots windows for creating a mosaic of iplots, by create a list with plot.width and plot.height to be used with iplot.location(x, y), and a list of vectors giving the top-left xloc(ation) and yloc(ation) of each window.
display.matrix <- function(columns, rows, # number of columns and rows in the final layout
  width = 1270, height=890, #size of the combined plot windows, in pixels
  xpos=0, ypos=0, # position of the top left window, in pixels
  borderwidth=3, # width of the border (window decoration), in pixels
  barheight=45 # height of title bar + menu bar, in pixels
) {
  plot.width <- width/columns - 2 * borderwidth
  plot.height <- height/rows - 2 * borderwidth - barheight
  # the first window starts at xpos, ypos; for each successive one, add two borders + the width of one plot
  xloc <- xpos + ((1:columns)-1) * (2 * borderwidth + plot.width)
  yloc <- ypos + ((1:rows)-1) * (2 * borderwidth + barheight + plot.height)
  list(plot.width = round(plot.width), plot.height = round(plot.height),
    xloc = round(xloc), yloc=round(yloc))
}

# Notes:
# iplot.location(x=0, y=0) #location of *window's corner* in pixels (includes window decoration)
# iplot.size(width=dm$plot.width, height=dm$plot.height)#size of *plot* in pixels (excludes window decoration)


iplot.move <- function (display.matrix, column, row) { # moves and resizes
  iplot.size(width=display.matrix$plot.width, height=display.matrix$plot.height)
  iplot.location(x=display.matrix$xloc[column], y=display.matrix$yloc[row])
}
  
dm <- display.matrix(4,4,ypos=30, barheight=38) # do not start on the O.S. menu bar (30 pix, on top on my desktop)

ibar(x1) # barchart
iplot.move(dm,1,1)
ihist(x4) # histogram
iplot.move(dm,4,4)
ihist(x3) # histogram
iplot.move(dm,3,3)
iplot(x1, x4) # scatter plot
iplot.move(dm,1,4)
iplot(x1, x3) # scatter plot
iplot.move(dm,1,3)
iplot(x3, x4) # scatter plot
iplot.move(dm,3,4)

iplot(jitter(x1,.015), jitter(x2,.015)) # scatter plot
iplot.move(dm,1,2)

iplot(x2,x4) # scatter plot
iplot.move(dm,2,4)
iplot(x2, x3) # scatter plot
iplot.move(dm,2,3)

ibar(x2) # barchart
iplot.move(dm,2,2)


ibox(X[c("x1","x2","x3", "x4")]) # boxplots 
iplot.move(dm,2,1)
ipcp(X[c("x1","x2","x3", "x4")]) # parallel coordinates
iplot.move(dm,3,2)




#width, height 	the width and height of the plotting window, in inches. 
#xpos, ypos integer: initial position of the top left corner of the window, in pixels. Negative values are from the opposite corner

#Example 3D diagram with openGL and Rcmdr
library(Rcmdr)
library(rgl, pos=4)
library(mgcv, pos=4)

scatter3d(X$x1, X$x4, X$x3, fit="smooth", residuals=TRUE, bg="black",
  axis.scales=TRUE, grid=TRUE, ellipsoid=FALSE, xlab="x1", ylab="x4",
  zlab="x3",  revolutions=0)

scatter3d(X$x3, X$x1, X$x4, surface=FALSE, residuals=TRUE, bg="black",
  axis.scales=TRUE, grid=FALSE, ellipsoid=FALSE, xlab="x3", ylab="x1",
  zlab="x4",groups=as.factor(X$x2))


## example from http://www.statmethods.net/advgraphs/interactive.html
library(iplots)
cyl.f <- factor(mtcars$cyl)
gear.f <- factor(mtcars$factor) 
attach(mtcars) 
ihist(mpg) # histogram 
ibar(carb) # barchart
iplot(mpg, wt) # scatter plot
ibox(mtcars[c("qsec","disp","hp")]) # boxplots 
ipcp(mtcars[c("mpg","wt","hp")]) # parallel coordinates
imosaic(cyl.f,gear.f) # mosaic plot
detach(mtcars)

