# 10-07,09-2019
# STA 141A : Lectures 6,7
# Codes

#######################################################
## Statistically informed data visualization
#######################################################

####################
## A tour : mtcars
####################

data(mtcars)  # part of available data sets in R

# The data was extracted from the 1974 Motor Trend US magazine, and comprises
# fuel consumption and 10 aspects of automobile design and performance for
# 32 automobiles (1973-74 models).

# Reference:
# Henderson and Velleman (1981). Building multiple regression models interactively. Biometrics, 37, 391-411.


# A data frame with 32 observations on 11 variables.
#	  mpg 	Miles/(US) gallon
#   cyl 	Number of cylinders
#	  disp 	Displacement (cu.in.)
#	  hp 		Gross horsepower
#   drat 	Rear axle ratio
#	  wt 		Weight (1000 lbs)
# 	qsec 	1/4 mile time
# 	vs 		V/S
#	  am 		Transmission (0 = automatic, 1 = manual)
#	  gear 	Number of forward gears
# 	carb 	Number of carburetors

attach(mtcars)
mtcars
rownames(mtcars)

#######################################
## Basic scatterplot smoother : lowess
#####################################

install.packages(digest)
library(digest)

plot(wt,mpg, main = "Scatter plot of MPG vs. Weight",
     xlab= "Weight of car (in 1000 lbs)", ylab = "Miles per Gallon", pch = 10)

abline(lm(mpg ~ wt), col="red", lwd=2, lty = 1) # plots the least squares regression line of "mpg" on "wt"

lines(lowess(wt,mpg), col="blue", lwd=2, lty=2) # plots a lowess regrssion line of "mpg" on "wt"

##local weighted least squares!

############################################
## Enhanced plotting features : car package
#########################################

library(car)

## bivariate scatterplot

scatterplot(mpg ~ wt | cyl, data = mtcars, lwd=2,
            main = "Scatter plot of MPG vs Weight by # of Cylinders",
            xlab = "Weight of car (in 1000 lbs)", ylab = "Miles per Gallon",
            legend.plot = TRUE,
            legend.coords = "topleft",
            boxplot = "xy",
)

scatterplot(mpg ~ wt | cyl, data = mtcars, lwd=2,
            main = "Scatter plot of MPG vs Weight by # of Cylinders",
            xlab = "Weight of car (in 1000 lbs)", ylab = "Miles per Gallon",
            legend.plot = TRUE,
            legend.coords = "topleft",
            boxplot = "xy",
            span = 0.9,
            id.method = "identify",
)

#############
## scatterplot matrix

pairs(~ mpg + disp + drat + wt, data = mtcars, main = "Basic scatter plot matrix")  # using base R function pairs()

?mtcars

# using function scatterplotMatrix() in car package

scatterplotMatrix( ~ mpg + disp + drat + wt, data = mtcars,
                   spread=FALSE,
                   span = 0.8,
                   main = "Scatter plot matrix via car package")

scatterplotMatrix( ~ mpg + disp + drat + wt | cyl , data = mtcars,
                   spread=FALSE,
                   span = 0.8,
                   main = "Scatter plot matrix grouped by # cylinders")

scatterplotMatrix( ~ mpg + disp + drat + wt | cyl , data = mtcars,
                   spread=FALSE,
                   span = 0.8,
                   diagonal = "histogram",
                   main = "Scatter plot matrix grouped by # cylinders")

scatterplotMatrix( ~ mpg + disp + drat + wt | cyl , data = mtcars,
                   spread=FALSE,
                   span = 0.8,
                   diagonal = list(method="boxplot"),
                   main = "Scatter plot matrix grouped by # cylinders")

scatterplotMatrix( ~ mpg + disp + drat + wt | cyl , data = mtcars,
                   spread=FALSE,
                   span = 0.8, smooth=FALSE,
                   diagonal = "boxplot",
                   main = "Scatter plot matrix grouped by # cylinders")


####################################################
## Spinning 3D scatter plot  : Using package rgl
####################################################

install.packages("digest")

library(digest)

library(rgl)

plot3d(wt, disp, mpg, col="red", size=5)

z = 2 * volcano        # Exaggerate the relief of a volcano surface (data volcano is part of rgl package)
x =10 * (1:nrow(z))   # 10 meter spacing (S to N)
y = 10 * (1:ncol(z))   # 10 meter spacing (E to W)
persp3d(x, y, z, col = "green3") 
# perspective plot of surface z on the grid of x and y


# the resulting plot can be rotated by dragging with a mouse

##################################################
## Correlogram : Displaying pairwise correlations
####################################################

round(cor(mtcars),2)  # correlation matrix, rounded to 2 decimal places

library(corrgram)

corrgram(mtcars, order = TRUE,
         lower.panel = panel.shade, upper.panel = panel.pie,
         text.panel = panel.txt,
         main = "Correlogram of mtcars data")

# For lower as well as upper panels (i.e., below and above diagonal)
# blue color represents positive correlation
# red color represents negative correlation
# darker shade indicates stronger (positive or negative) correlation

# The pies above the diagonal contain the same information as the squares below the diagonal,
# where the strength of the correlation is displayed by the size of the pie slice

# "order = TRUE" ensures that the variables are reordered using principal component analysis

corrgram(mtcars, order = FALSE,
         lower.panel=panel.ellipse, upper.panel = panel.pts,
         text.panel = panel.txt,
         diag.panel = panel.minmax,
         main = "Correlogram of mtcars data")

##################################################
## Mosaic plot : Displaying contingency tables
####################################################
Titanic
ftable(Titanic)  # plot "flattened" table Titanic
library(vcd)
mosaic(Titanic, shade = TRUE, legend = TRUE)
mosaic(~ Class + Sex + Age + Survived, data = Titanic, shade = TRUE, legend = TRUE)
# order of variables (categorical) in the formula above determine the nesting of rectangles
mosaic(~ Class + Age + Survived, data = Titanic, shade = TRUE, legend = TRUE) 
# plot corresponding to a subset of variables

Titanic
is.data.frame(Titanic)
is.table(Titanic)
str(Titanic)
apply(Titanic,2,sum)
apply(Titanic,c(2,3),sum)
apply(Titanic,c(2,3),sum)
apply(Titanic,c(1,2,4),sum)
Titanic[Titanic$Age==`Child`]
