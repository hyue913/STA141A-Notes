# 10-18-2019
# STA 141A : Lecture 11
# Codes
####################################
# Linear regression using R
#####################################
## Goal of this analysis is to build a predictive model for diamond price
library(ggplot2)
data("diamonds")
attach(diamonds)
str(diamonds)
################
## Three of the variables (cut, color, clarity) are "ordinal categorical"
## variables. We convert them to simple categorical variables and use
## these transformed variables in the subsequent analyses.

cut.uo = as.factor(as.vector(cut))
color.uo = as.factor(as.vector(color))
clarity.uo = as.factor(as.vector(clarity))
diamonds = cbind(diamonds,cut.uo,color.uo,clarity.uo)

pl.diam = ggplot(diamonds, aes(carat, price, color = cut))
pl.diam + geom_point(alpha=I(1/5))
pl.diam + geom_point(alpha=I(1/5)) + geom_smooth(method="lm")
pl.diam + geom_point(alpha=I(1/5)) + geom_smooth()

###############
## Notice that the least squares fit of the regression line are quite "off";
## Also, the variability of price (response variable) increases with
## carat (predictor variable)

# We use log transformations on price and carat to check
# if the relationship is more reasonable

pl.diam + geom_point(alpha=I(1/5)) + scale_x_log10() + scale_y_log10()

## facet (or, condition) with respect to cut
pl.diam + geom_point(alpha=I(1/5)) + scale_x_log10() + scale_y_log10() + facet_grid(cut ~ .)

## facet (or, condition) with respect to cut and clarity
pl.diam + geom_point(alpha=I(1/5)) + scale_x_log10() + scale_y_log10() + facet_grid(cut ~ clarity)

#####################################################
#  Building predictive model through linear regression
#######################################################

log_price = log10(price)  # to be treated as the response variable
log_carat = log10(carat)  # to be treated as one predictor variable

diamonds = cbind(diamonds,log_price,log_carat)

## plot the points, together with least squares fit for data grouped by cut
qplot(log_carat,log_price,data=diamonds,color=cut) + geom_smooth(method="lm")

##########################################################################################
## Question: How much of the variability in log10(price) is explained by log10(carat) alone ?
############################################################################################

price.lm1 = lm(log_price ~ log_carat, data=diamonds)
summary(price.lm1)
# High value of R-square

di.lm1 = cbind(diamonds,resid=price.lm1$resid)
# add the residuals as additional column of the data frame
pl.di.lm1 = ggplot(di.lm1, aes(log_carat,resid))

# residuals do not appear completely random
pl.di.lm1 + geom_point(alpha=I(1/5),shape='.')

# residuals show parrern when distinguished by cut
pl.di.lm1 + geom_point(alpha=I(1/5),shape='.') + aes(color=cut)
# Conclusion : cut appears quite informative

# residuals show parrern when distinguished by both cut and clarity
pl.di.lm1 + geom_point(alpha=I(1/5),shape='.') + aes(color=cut) + facet_wrap(~clarity)
# Conclusion : Consider including both cut and clarity as additional explanatory variable

########################################################################
# Question : How do the variables cut and clarity affect the variability in price ?
######################################################################

#####################################
## First attempt : Just a shift in the baseline (intercept) due
###                to different levels of cut
###################################

price.lm2 = lm(log_price ~ log_carat + cut.uo, data=diamonds)  # use the unordered factor
summary(price.lm2)
## All the explanatory variables are significant.
## This means, (i) log_carat is significant, and
## (ii) Intercepts of the regression lines for different levels of cut are different

contrasts(cut.uo)  # actual basis added to the design matrix

di.lm2 = cbind(diamonds,resid=price.lm2$resid)
# add the residuals as additional column of the data frame
pl.di.lm2 = ggplot(di.lm2, aes(log_carat,resid))

### residual plots shows pattern that indicates slopes could be different
pl.di.lm2 + geom_point(alpha=I(1/5),shape='.') + facet_grid(.~cut)


#####################################
## Second attempt : Both intercept and slopes may be different for different
##                  values of cut
###################################

## add interaction between log_carat (continuous) and cut.uo (categorical)

price.lm3 = lm(log_price ~ log_carat + cut.uo + log_carat * cut.uo, data=diamonds)
summary(price.lm3)

di.lm3 = cbind(diamonds,resid=price.lm3$resid)
# add the residuals as additional column of the data frame
pl.di.lm3 = ggplot(di.lm3, aes(log_carat,resid))

### residual plots show patterns that indicate that clarity has a role
pl.di.lm3 + geom_point(alpha=I(1/5),shape='.') + facet_grid(cut ~ clarity)


#####################################
## Third attempt : Both intercept and slopes may be different for different
##                 values of cut; Also, there is shift in intercept due to clarity
###################################

price.lm4 = lm(log_price ~ log_carat + cut.uo + clarity.uo
               + log_carat * cut.uo,  data=diamonds)
summary(price.lm4)

di.lm4 = cbind(diamonds,resid=price.lm4$resid)
# add the residuals as additional column of the data frame
pl.di.lm4 = ggplot(di.lm4, aes(log_carat,resid))

### residual plots show patterns that indicate that clarity has a role
pl.di.lm4 + geom_point(alpha=I(1/5),shape='.') + facet_grid(cut ~ clarity)

#####################################
## Fourth attempt : Add both cut and clarity and their interactions with log_carat
###################################

price.lm5 = lm(log_price ~ log_carat + cut.uo + clarity.uo
               + log_carat * cut.uo + log_carat * clarity.uo, data = diamonds)
summary(price.lm5)

di.lm5 = cbind(diamonds,resid=price.lm5$resid)
# add the residuals as additional column of the data frame
pl.di.lm5 = ggplot(di.lm5, aes(log_carat,resid))

### residual plots show patterns that indicate that clarity has a role
pl.di.lm5 + geom_point(alpha=I(1/5),shape='.') + facet_grid(cut ~ clarity)

##########################################################################
#######################################################################
## Question : What if we were working with a much smaller data set ?
#######################################################################

diamonds.sub = diamonds[1:5000, ]  # select the data with the first 5000 observations

price.lm6 = lm(log_price ~ log_carat + cut.uo + clarity.uo
               + log_carat * cut.uo + log_carat * clarity.uo, data = diamonds.sub)
summary(price.lm6)

di.lm6 = cbind(diamonds.sub,resid=price.lm6$resid)
# add the residuals as additional column of the data frame
pl.di.lm6 = ggplot(di.lm6, aes(log_carat,resid))

### residual plots show patterns that indicate that clarity has a role
pl.di.lm6 + geom_point(alpha=I(1/2)) + facet_grid(cut ~ clarity)

###########
## Also add variables 'depth' and 'table' to the predictive model

price.lm7 = lm(log_price ~ log_carat + depth + table + cut.uo + clarity.uo
               + log_carat * cut.uo + log_carat * clarity.uo, data = diamonds.sub)
summary(price.lm7)

di.lm7 = cbind(diamonds.sub,resid=price.lm7$resid)
# add the residuals as additional column of the data frame
pl.di.lm7 = ggplot(di.lm7, aes(log_carat,resid))

### residual plots show patterns that indicate that clarity has a role
pl.di.lm7 + geom_point(alpha=I(1/2)) + facet_grid(cut ~ clarity)

###########
## Drop 'table' (not significant)
## and the interaction between  'log_carat' and 'clarity.uo' (not significant)
## (implication: slope of log10(carat) does not change with the levels of clarity)

price.lm8 = lm(log_price ~ log_carat + depth + cut.uo + clarity.uo
               + log_carat * cut.uo, data = diamonds.sub)
summary(price.lm8)

di.lm8 = cbind(diamonds.sub,resid=price.lm8$resid)
# add the residuals as additional column of the data frame
pl.di.lm8 = ggplot(di.lm8, aes(log_carat,resid))

### residual plots show patterns that indicate that clarity has a role
pl.di.lm8 + geom_point(alpha=I(1/2)) + facet_grid(cut ~ clarity)
pl.di.lm8 + geom_point(alpha=I(1/2)) + facet_grid(cut ~ color)
# shows some dependence on 'color'

########################
## Add color as another predictor

price.lm9 = lm(log_price ~ log_carat + depth + cut.uo + clarity.uo + color.uo
               + log_carat * cut.uo, data = diamonds.sub)
summary(price.lm9)
## 'depth' is now not very significant

di.lm9 = cbind(diamonds.sub,resid=price.lm9$resid)
# add the residuals as additional column of the data frame
pl.di.lm9 = ggplot(di.lm9, aes(log_carat,resid))

### residual plots show patterns that indicate that clarity has a role
pl.di.lm9 + geom_point(alpha=I(1/2)) + facet_grid(cut ~ clarity)

#################################
## Predictive model : after dropping 'depth' from the previous model (price.lm9)
##################################

price.lm10 = lm(log_price ~ log_carat + cut.uo + clarity.uo + color.uo
                + log_carat * cut.uo, data = diamonds.sub)
summary(price.lm10)

di.lm10 = cbind(diamonds.sub,resid=price.lm10$resid)
pl.di.lm10 = ggplot(di.lm10, aes(log_carat,resid))
pl.di.lm10 + geom_point(alpha=I(1/2), aes(color=color)) + facet_grid(cut ~ clarity)

## Model : og_price ~ log_carat + cut.uo + clarity.uo + color.uo + log_carat * cut.uo
## Multiple R-squared : 0.9651

###################################################################
## There still appear to be clusters in the residuals
## according to the value of 'log_carat'
## It will be useful to divide the data according to the value of 'log_carat'

# histogram or density plot of 'log_carat' shows "bimodality" (two modes)
hist(diamonds.sub$log_carat,breaks=16)
plot(density(diamonds.sub$log_carat,bw=0.05))
rug(diamonds.sub$log_carat)

# divide the data according to size (i.e., 'log_carat')
diamonds.small = diamonds.sub[diamonds.sub$log_carat < -0.3, ]
diamonds.big = diamonds.sub[diamonds.sub$log_carat >= -0.3, ]

##################################
#### Small diamonds
#################################
price.small.lm10 = lm(log_price ~ log_carat + cut.uo + clarity.uo + color.uo
                      + log_carat * cut.uo, data = diamonds.small)
summary(price.small.lm10)
di.small.lm10 = cbind(diamonds.small,resid=price.small.lm10$resid)
pl.di.small.lm10 = ggplot(di.small.lm10, aes(log_carat,resid))
pl.di.small.lm10 + geom_point(alpha=I(1/2), aes(color=color)) + facet_grid(cut ~ clarity)

### updated model : after dropping 'cut' and its interactions with 'log_carat'
### and adding back 'depth' and 'table'

price.small.lm11 = lm(log_price ~ log_carat + depth + table +
                        clarity.uo + color.uo, data = diamonds.small)
summary(price.small.lm11)

# drop 'depth' and 'table' : still not significant
price.small.lm12 = lm(log_price ~ log_carat + clarity.uo + color.uo, data = diamonds.small)
summary(price.small.lm12)
di.small.lm12 = cbind(diamonds.small,resid=price.small.lm12$resid)
pl.di.small.lm12 = ggplot(di.small.lm12, aes(log_carat,resid))
pl.di.small.lm12 + geom_point(alpha=I(1/2), aes(color=color)) + facet_grid(clarity ~.)

## Proposed (simplistic) for Small Diamonds :
## log_price ~ log_carat +  clarity.uo + color.uo
## Multiple R-squared : 0.7251 (significant drop from that using all 10000 observations)

##############################
#### Big diamonds
##############################
price.big.lm10 = lm(log_price ~ log_carat + depth + table + cut.uo + clarity.uo + color.uo
                    + log_carat * cut.uo, data = diamonds.big)
summary(price.big.lm10)
di.big.lm10 = cbind(diamonds.big,resid=price.big.lm10$resid)
pl.di.big.lm10 = ggplot(di.big.lm10, aes(log_carat,resid))
pl.di.big.lm10 + geom_point(alpha=I(1/2), aes(color=color)) + facet_grid(cut ~ clarity)

### updated model : drop 'depth' and 'table' from the model since neither is significant

price.big.lm11 = lm(log_price ~ log_carat + cut.uo + clarity.uo + color.uo
                    + log_carat * cut.uo, data = diamonds.big)
summary(price.big.lm11)
di.big.lm11 = cbind(diamonds.big,resid=price.big.lm11$resid)
pl.di.big.lm11 = ggplot(di.big.lm11, aes(log_carat,resid))
pl.di.big.lm11 + geom_point(alpha=I(1/2), aes(color=color)) + facet_grid(cut ~ clarity)

## Proposed (simplistic) Model for Big Diamonds :
## log_price ~ log_carat + cut.uo + clarity.uo + color.uo + log_carat * cut.uo
## Multiple R-squared 0.3872  (only!!)

##################################
## Final predictive model : use the information on size more effectively
################################

size.ind = as.factor(diamonds.sub$log_carat >= - 0.3)  # indicator of big diamond
diamonds.sizeadj = cbind(diamonds.sub,size.ind)

price.lm11 = lm(log_price ~ log_carat + cut.uo + clarity.uo + color.uo
                + size.ind + log_carat * cut.uo, data = diamonds.sizeadj)
summary(price.lm11)

## drop interaction between 'log_carat' and 'cut', since it is not very significant

price.lm12 = lm(log_price ~ log_carat + cut.uo + clarity.uo + color.uo
                + size.ind + size.ind * log_carat, data = diamonds.sizeadj)
summary(price.lm12)
di.lm12 = cbind(diamonds.sizeadj,resid=price.lm12$resid)
pl.di.lm12 = ggplot(di.lm12, aes(log_carat,resid))
pl.di.lm12 + geom_point(alpha=I(1/2), aes(color=color)) + facet_grid(cut ~ clarity) + ylim(c(-0.2,0.2))

## FINAL Model for all diamonds (based on n = 5000)
## log_price ~ log_carat + cut.uo + clarity.uo + color.uo + size.ind + size.ind * log_carat
## Multiple R-squared: 0.9787

########################################################
## Conclusions:
##
## 1. Size (carat) of a diamond is a very significant factor in determining its price.
## 2. A linear regression model is effective for predicting the price.
## 3. Both intercept and slope may vary according to the size of the diamond.
## 4. Apart from carat, other important predictors for price are the factors
##    cut, clarity and color.
## 5. If you divide the data into groups according to the size of the diamond,
##    the within-group variations are harder to explain through a regression model.
## 6. The proposed regression model (based on first 5000 observations) can explain
#     about 98% variability in the log_price.
###################################################
