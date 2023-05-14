install.packages("ggplot2")
library(ggplot2)

#1
data(ChickWeight)
h <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet))
h + geom_smooth(alpha=.4, size=3)

h <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet))
h + geom_point(alpha=.3) + geom_smooth(alpha=.2, size=1)

h <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet))
h + geom_density()

#2
data(mtcars)
p <- qplot(wt, mpg, colour=hp, data=mtcars)
m <- mtcars[1:10,]
p %+% m

c <- ggplot(mtcars, aes(factor(cyl)))
c + geom_bar()
c + geom_bar(fill = "red")
c + geom_bar(colour ="red")
c + geom_bar(fill="white", colour="red")

k <- ggplot(mtcars, aes(factor(cyl), fill = factor(vs)))
k + geom_bar()


require(MASS)
# attach graphics library
require(lattice)

Cars93[1:3,]

dim(Cars93)
ncol(Cars93)
nrow(Cars93)
colnames(Cars93)


# Scatterplot of the data
# graphics setup
trellis.par.set(theme=col.whitebg())
# plotting
with(Cars93,xyplot(100/MPG.highway~Weight,
                   ylab="Gallons per 100 miles",
                   main="Cars(1993 Makes & Models)",pch=16))

# Modelling:  Cars and Fuel Economy
with(Cars93,xyplot(100/MPG.highway~Weight,
                   ylab="Gallons per 100 miles",pch=16,
                   main="Cars(1993 Makes & Models)",
                   panel =
                     function(x, y, ...) {
                       panel.xyplot(x, y, ...)
                       panel.lmline(x, y,col="red", ...)
                     }))

###########################
###########################
# by types

Cars93 <- transform(Cars93,GPM.highway = 100/MPG.highway,
                    WeightT=Weight/1000)
with(Cars93,xyplot(GPM.highway~Weight|Type, ylab="Gallons per 100 miles",
                   pch=16,main="Cars(1993 Makes & Models)"))

