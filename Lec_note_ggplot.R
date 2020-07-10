# STA 141A : ggplot
# Codes

###########################################################
## Structured Graphics : Getting started with ggplot2
##########################################################

library(ggplot2)

qplot(wt, mpg, data = mtcars)  # scatterplot of mpg vs wt

qplot(wt, mpg, data = mtcars, color = I("red"))  # scatterplot of mpg vs wt

qplot(wt, mpg, data = mtcars, color = I("red"), size = I(2))  # scatterplot of mpg vs wt

qplot(wt, mpg, data = mtcars, colour = cyl, size = I(2))  ## color values according to cyl = # of cylinders

## color values according to cyl = # of cylinders (treated as a factor)
qplot(wt, mpg, data = mtcars, colour = factor(cyl), size = I(2))

## add a scatterplot smoother and corresponding confidence band per level of the factor cyl
qplot(wt, mpg, data = mtcars, colour = factor(cyl), geom = c("point","smooth"))

## same as above, but the smoother is added as an additional layer
qplot(wt, mpg, data = mtcars, colour = factor(cyl)) +  geom_smooth()

## add a scatterplot smoother (with a different bandwidth) for each level of cyl, and add corresponding confidence band
qplot(wt, mpg, data = mtcars, colour = factor(cyl)) +  geom_smooth(span=2)

## add a scatterplot smoother for the whole data
## but mark the points with different color for different levels of cyl
qplot(wt, mpg, data = mtcars, colour = cyl) +  geom_smooth(span=2)

## Saving a plot as an object

pl.mpg = qplot(displ, hwy, data=mpg, color=factor(cyl))
summary(pl.mpg)
print(pl.mpg)
save(pl.mpg, file="mpg_plot.Rdata")

############################
## Fitting regression line
###########################

## fit regression line for each level of cyl and plot corresponding confidence bands
qplot(wt, mpg, data = mtcars, colour = factor(cyl)) +  geom_smooth(method = "lm")


## make the regression line thicker and confidence band more transparent
## also, mark the points with different color for different levels of cyl
qplot(wt, mpg, data = mtcars, colour = cyl) +  geom_smooth(method = "lm",size=2,alpha = I(1/5))

## in addition, make the size of the dots proportional to the number of gears (levels of variable gear)
qplot(wt, mpg, data = mtcars, colour = cyl, size = gear)  +  geom_smooth(method = "lm",size=2,alpha = I(1/5))

## also, add labels as the names of the rows
mtcars$label=row.names(mtcars)

qplot(wt, mpg, label=label, data = mtcars, colour = cyl, size = gear) +  geom_smooth(method = "lm", alpha = I(1/2))  +  geom_text(size=3)

qplot(wt, mpg, label=label, data = mtcars) + geom_point(colour = cyl, size = gear) +  geom_smooth(method = "lm", alpha = I(1/5)) +  geom_text(size=2)

################################
##  Histograms and density plots
################################

## plot histogram of mpg
qplot(mpg, data = mtcars, geom="histogram")

## change the bin width of the histogram
qplot(mpg, data = mtcars, geom="histogram", binwidth=3)

## plot relative frequencies rather than counts by setting ..density.. as an argument
qplot(mpg, ..density.., data = mtcars, geom="histogram",binwidth=3)

## histogram subdivided according to the levels of factor(cyl)
qplot(mpg, data = mtcars, geom="histogram", binwidth=3, fill=factor(cyl))

## plot a kernel density estimate for mpg, bandwidth (bw) = 1.5, xlim sets the range of values
qplot(mpg, data = mtcars, geom="density", bw=1.5, xlim=c(5,50))

## plot the kernel density estimates for mpg for the strata created by levels of factor(cyl)
qplot(mpg, data = mtcars, geom="density", bw=1.5, colour=factor(cyl), xlim=c(5,50))

## plot the density estimates for different strata in separate panels using the argument facets
## the formulate facets = . ~ cyl determines a lattice of plots with
## number of columns corresponding to the number of distinct values of cyl
qplot(mpg, data = mtcars, geom="density", bw=1.5, xlim=c(5,50), facets = . ~ cyl)


###########################
## Saving a plot as an object

pl.mpg = qplot(displ, hwy, data=mpg, color=factor(cyl))
summary(pl.mpg)
print(pl.mpg)
save(pl.mpg, file="mpg_plot.Rdata")

###################################
## Layered graphics in ggplot2
###################################

# From now on, we use ggplot() function rather than qplot() to
# make each component of the graph clear

data(diamonds)
?diamonds
summary(diamonds)

pl.diam = ggplot(diamonds, aes(carat, price, color = cut))
summary(pl.diam)

pl.diam # no scatterplot is created!

pl.diam + geom_point()  # now the scatterplot appears
pl.diam + geom_point(alpha=I(1/2), shape='.', size=I(4))   # modify shape, size and transparency

## Layers can be stored as regular R objects

bestfit = geom_smooth(method="lm",se=F,color=alpha("green",0.5),size=2)
bestscatter = geom_point(alpha=I(1/2),shape='.',size=I(4))

pl.diam + bestscatter
pl.diam + bestscatter + bestfit

## change the scales on the x and y axes
pl.diam + bestscatter + bestfit +
  scale_x_continuous(limits=c(0,4.5)) + scale_y_continuous(limits=c(0,25000))

## also change the grid or tick marks on the x and y axes
pl.diam + bestscatter + bestfit +
  scale_x_continuous(limits=c(0,4.5),breaks=c(1.5,2.5,3.5)) +
  scale_y_continuous(limits=c(0,25000),breaks=c(10000,20000))

## plot the scatter plots corresponding different values of cut
pl.diam + bestscatter + facet_grid(cut ~ .)

# also fit regression line to each stratum
pl.diam + bestscatter + facet_grid(cut ~ .) + bestfit

# allow the x-scale of the different panels to be different
# default: scale = "free"; other options "free_x", "free_y"
pl.diam + bestscatter + facet_grid(cut ~ ., scale="free") + bestfit

## a different way of organizing the panels
pl.diam + bestscatter + facet_grid(. ~ cut) + bestfit + ylim(0,20000)

#################################################
# Variables can be plotted in a transformed scale
###################################################

pl.di = ggplot(diamonds, aes(carat, price))
pl.di = pl.di + geom_point() + scale_x_log10() + scale_y_log10()
pl.di + xlab("log10(carat)") + ylab("log10(price)")

# We can achieve the same by using qplot()
qplot(log10(carat),log10(price),data=diamonds)

################################################
## Different geometric objects used in a plot
################################################

dfx = data.frame(x=c(3,1,5,4),y=c(2,4,6,8),label=letters[1:4])
pl.dfx = ggplot(dfx,aes(x=x,y=y,label=label)) + xlab(NULL) + ylab(NULL)

par(mfrow=c(2,4))

pl.dfx + geom_bar(stat="identity")  + xlab("bar")# bar plot
pl.dfx + geom_point() + xlab("point")# scatter plot
pl.dfx + geom_line()  + xlab("line") # line plot (connecting successive points)
pl.dfx + geom_area() + xlab("area") # area plot
pl.dfx + geom_path() + xlab("path") # path plot (connecting successive points the way they are ordered in the data frame)
pl.dfx + geom_text() + xlab("text") # adding text (label) to points
pl.dfx + geom_tile() + xlab("tile") # tiles around points
pl.dfx + geom_polygon() + xlab("polygon") # polygon formed by connecting points


#####################################
## Displaying data distribution
################################

depth.di = ggplot(diamonds,aes(depth)) + xlim(58,68)

# plot relative density histograms of depth stratified by different values of cut
depth.di + geom_histogram(aes(y=..density..), binwidth=0.2) + facet_grid(cut ~ .)

depth.di + geom_histogram(aes(fill=cut), binwidth=0.2, position = "fill")

depth.di + geom_histogram(aes(fill=cut), binwidth=0.2, position = "stack")

depth.di + geom_histogram(aes(fill=cut), binwidth=0.2, position = "dodge")

########################
## boxplot has to have an y variable if we want to plot against variable cut
##########

depcut.di = ggplot(diamonds,aes(x=cut,y=depth))

depcut.di +  geom_boxplot()

depcut.di + aes(color=cut) + geom_boxplot()  # use different colors for different values of cut

### Stratifying further by another categorical variable clarity
depcut.di + aes(color=factor(clarity)) + geom_boxplot()

depcut.di +  aes(color=cut) + geom_boxplot() + facet_grid(clarity ~ .)
depcut.di +  aes(color=cut) + geom_boxplot() + facet_grid(. ~ clarity)
depcut.di +  aes(color=cut) + geom_boxplot() + facet_wrap(~ clarity)

##############################################
# Overriding grouping according to values of a variable
############################################

# now grouping according to values of clarity rather than cut
depcut.di + geom_boxplot(aes(group=clarity,color=clarity))

# however, the label on the x axis needs to be changed
depcut.di + geom_boxplot(aes(group=clarity,color=clarity)) + scale_x_discrete("clarity",breaks=c())

######
## Another example: Longitudinal data

library(nlme)
data(Oxboys) # heights of boys in Oxford

boysOx = ggplot(Oxboys, aes(Occasion,height))

boysOx + geom_boxplot() # boxplots grouped by Occasion

# overlay the plot with actual height trajectories
boysOx + geom_boxplot() + geom_line(aes(group=Subject),color="steelblue")



############################################
## Dealing with overplotting in a scatter plot
#############################################

td = ggplot(diamonds,aes(table,depth))
td + geom_point()   # ordinary scatter plot

td + geom_jitter()  # jitter plot
jit = position_jitter(width=0.5)
td + geom_jitter(position=jit)
td + geom_jitter(position=jit,color=alpha("black",1/20))

################################
## 2D histogram and hexagonal binning
#############################

di = ggplot(diamonds,aes(carat,price)) + xlim(1,3)

di + stat_bin2d(bins=20,na.rm=T)
di + stat_bin2d(bins=20,na.rm=T,color="yellow")
di + stat_bin2d(binwidth=c(0.02,200))
di + stat_bin2d(binwidth=c(0.02,200),aes(fill=..density..))


# use hexagonal binning instead of square bins
di + stat_binhex(bins=20,na.rm=T)

# change the shape of the hexagonal bins and change the color scale
di + stat_binhex(binwidth=c(0.05,500),na.rm=T) + scale_fill_gradient(low="blue",high="red")


#################
## kernel density plot
#################

# contour plot of the density
di + geom_density2d(na.rm=T)

# contour plot of the density together with the scatter plot
di + geom_point(na.rm=T) + stat_density2d(na.rm=T)

# surface plot of the density
di + stat_density2d(na.rm=T,aes(fill=..level..), geom="polygon")

# surface plot of the density with added transparency to the surface to make
# the points underneath visible; the points are themselves made transparent
di + geom_point(na.rm=T,size=1,alpha=I(1/10)) + stat_density2d(na.rm=T,aes(fill = ..level..,alpha=..level..), geom="polygon") + scale_fill_gradient(low="blue",high="red")

