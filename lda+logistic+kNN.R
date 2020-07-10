#####################################
## 2-class classification problem
######################################

###############
## Data: Feture X is 2-dimensional
## For both class 1 and class 2, the distribution of X (bivariate) is Normal
## X |Y = 1   has N(mu1, Sigma)  distribution
## X |Y = 2   has N(mu2, Sigma)  distribution
###############################
n1 = 40
n2 = 40
m1 = 10
m2 = 10
n = n1 + n2  # number of observations in Training data set
m = m1 + m2  # number of observations in Test data set

## Model Parameters
mu1 = c(0,0)  # Population mean for class 1
mu2 = c(3,2)  # Population mean for class 2
Sigma = matrix(c(2,0.5,0.5,1),2,2)  # common population covariance matrix
Sigma
## Simulate data
Y = c(rep(1,n1+m1),rep(2,n2+m2)) # true class labels
Y
Sigma.eig = eigen(Sigma)  # spectral decmposition of Sigma1
Sigma.eig
set.seed(1000) # set the value of the seed for the random number generator
X = matrix(0,n+m,2)
X
X[1:(n1+m1),] = matrix(1,(n1+m1),1) %*% t(as.vector(mu1))  + matrix(rnorm(2*(n1+m1)),n1+m1,2) %*% diag(sqrt(Sigma.eig$val)) %*% t(Sigma.eig$vec)
X[1:(n1+m1),]
X[(n1+m1+1):(n+m),] =  matrix(1,(n2+m2),1) %*% t(as.vector(mu2))  + matrix(rnorm(2*(n2+m2)),n2+m2,2) %*% diag(sqrt(Sigma.eig$val)) %*% t(Sigma.eig$vec)
X[(n1+m1+1):(n+m),]
X = round(X,3)
X
dfxy = data.frame(X1=X[,1],X2=X[,2],class=as.factor(Y))   # combined data
dfxy
## Training data : consisting of n1 observations from class 1 and
# n2 observations from class 2)
dfxy.train = dfxy[c(1:n1,(n1+m1+1):(n1+m1+n2)), ]
dfxy.train
## Test data : consisting of the remaining m1 observations from class 1
## and m2 observations from class 2
dfxy.test = dfxy[c((n1+1):(n1+m1),(n1+m1+n2+1):(n+m)), ]
dfxy.test
## Plotting the data
plot(dfxy[dfxy$class==1,1:2],col=2,xlim=range(dfxy[,1]),ylim=range(dfxy[,2]))
points(dfxy[dfxy$class==2,1:2],col=4)
####################

#################################################################
## Linear Discriminant Analysis for 2-class classification problem
##################################################################

library(MASS)
# Use linear discriminant analysis to the training data.
xy.lda = lda(class ~ X1 + X2, dfxy.train)
# overall summary of the procedure
xy.lda
# Predict for test data.
xy.lda.pred = predict(xy.lda, dfxy.test)
xy.lda.pred
# Create the confusion matrix by tabulating true classes against predicted classes.
xy.lda.conf = table(true = dfxy.test$class, predicted = xy.lda.pred$class)
xy.lda.conf
#####################################################
# Computing the classification boundary for LDA
#
# True Discriminant function for class j is
# delta_j(x) = solve(Sigma) %*% mu_j - 0.5 * t(mu_j) %*%  solve(Sigma) %*% mu_j + log(pi_j)
#
# Classification boundary is the set of X such that delta_1(X) - delta_2(X) = 0
# This can be expressed as    beta0 + t(beta1) %*% X = 0
# or,  beta0 + beta1[1] * X1 + beta1[2] * X2  = 0     where  X = (X1,X2)
########################################################################

pi1_hat = n1/(n1+n2)  # estimated prior probability of class 1
pi2_hat = n2/(n1+n2)  # estimated prior probability of class 2

mu1_hat = as.vector(xy.lda$means[1,]) # sample mean of class 1
mu2_hat = as.vector(xy.lda$means[2,]) # sample mean of class 2

# pooled sample covariance matrix as estimate of Sigma
Sigma_hat = ((n1-1) * cov(dfxy.train[dfxy.train$class==1,1:2])
             + (n2-1) * cov(dfxy.train[dfxy.train$class==2,1:2]))/(n1+n2-2)

# Estimated discriminant vector (for class 1 with respect to class 2)
# or, the slope of the (estimated) function delta_1(x) - delta_2(x)
beta1.lda = solve(Sigma_hat) %*% (mu1_hat - mu2_hat)
# Intercept term of the (estimated) function delta_1(x) - delta_2(x)
beta0.lda = log(pi1_hat/pi2_hat) - 0.5 * t(mu1_hat) %*% solve(Sigma_hat) %*% mu1_hat + 0.5 * t(mu2_hat) %*% solve(Sigma_hat) %*% mu2_hat

## compute (estimated) delta_1(x) - delta_2(x) for test data
xy.disc.pred = as.vector(beta0.lda) + as.matrix(dfxy.test[,1:2]) %*% beta1.lda
# predicted class label based on the value of the discriminant function
xy.lda.pred.alt = ifelse(xy.disc.pred > 0, 1, 2)
# confusion matrix computed from the discriminant function
xy.lda.alt.conf = table(true = dfxy.test$class, predicted = xy.lda.pred.alt)

## Plotting the data together with the estimated class boundary
plot(dfxy[dfxy$class==1,1:2],col=2,xlim=range(dfxy[,1]),ylim=range(dfxy[,2]))
points(dfxy[dfxy$class==2,1:2],col=4)

## To plot the classification boundary,
# express:  beta0.lda + beta1.lda[1] * X1 + beta1[2] * X2 = 0
# as:  intercept.lda + slope.lda * X1 = X2
intercept.lda = - as.vector(beta0.lda)/beta1.lda[2]
slope.lda  = - beta1.lda[1]/beta1.lda[2]
abline(intercept.lda, slope.lda)  # plots the clssification boundary

################################################
## Logistic Regression for 2-class classification
################################################

## use family = binomial with the glm function for a two-class logistic rgression
xy.glm = glm(class ~ X1+X2,dfxy.train, family = binomial)
# overall summary
summary(xy.glm)

# Predict for test data. Use type = "response" to get class probabilities.
xy.glm.pred.prob = predict(xy.glm, dfxy.test, type = "response")
xy.glm.pred.prob
# Convert predictions to class lables (1 or 2, for category 1 or 2, respectively).
xy.glm.pred = (xy.glm.pred.prob > 0.5) + 1
xy.glm.pred
# Create the confusion matrix by tabulating true classes against predicted classes.
xy.glm.conf = table(true = dfxy.test$class, predicted = xy.glm.pred)

xy.glm.conf

#  Use knn() from package class for k-nearest neighbors.
library(class)

# Use the kNN classifier with k = 7
xy.knn = knn(
  train = dfxy.train[c("X1","X2")], # training data for features used in classification
  test = dfxy.test[c("X1","X2")], # test data data for features used in classification
  cl = dfxy.train$class, # vector of class labels for training data
  k = 7)

## true and predicted class lables, togther with features, for the test data
cbind(dfxy.test,xy.knn)

xy.knn.conf = table(true = dfxy.test$class, predicted = xy.knn)

install.packages("caret")
library(caret)

trControl <- trainControl(method  = "cv",
                          number  = 5)
trControl

install.packages("e1071")
fit <- train(class ~ X1+X2,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:50),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = dfxy.train
)
fit
