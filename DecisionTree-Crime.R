# This model was created for a homework assignment using a small crime dataset on statsci.org.
# The goal is to use decision trees and regression to predict crime rates in a city

#Function to install packages and load them if they are not already installed
PackagesPrep <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

# Run function
PackagesPrep( c("tree" , "DAAG" ) )

# Clear Environment
rm(list = ls())

# Reading in the data
data <- as.data.frame(read.table("http://www.statsci.org/data/general/uscrime.txt", stringsAsFactors = FALSE, header = TRUE))

#
# check to make sure the data is read correctly
#

head(data)

##      M So   Ed  Po1  Po2    LF   M.F Pop   NW    U1  U2 Wealth Ineq     Prob    Time Crime
## 1 15.1  1  9.1  5.8  5.6 0.510  95.0  33 30.1 0.108 4.1   3940 26.1 0.084602 26.2011   791
## 2 14.3  0 11.3 10.3  9.5 0.583 101.2  13 10.2 0.096 3.6   5570 19.4 0.029599 25.2999  1635
## 3 14.2  1  8.9  4.5  4.4 0.533  96.9  18 21.9 0.094 3.3   3180 25.0 0.083401 24.3006   578
## 4 13.6  0 12.1 14.9 14.1 0.577  99.4 157  8.0 0.102 3.9   6730 16.7 0.015801 29.9012  1969
## 5 14.1  0 12.1 10.9 10.1 0.591  98.5  18  3.0 0.091 2.0   5780 17.4 0.041399 21.2998  1234
## 6 12.1  0 11.0 11.8 11.5 0.547  96.4  25  4.4 0.084 2.9   6890 12.6 0.034201 20.9995   682
#
# Crime is response, other variables are predictors





############################# Regression Tree ############################# 

# Setting the random number generator seed
set.seed(1)

# Fit a regression tree function to the crime data
tree.data <- tree(Crime~., data = data)
summary(tree.data)

## Regression tree:
##   tree(formula = Crime ~ ., data = data)
## Variables actually used in tree construction:
##   [1] "Po1" "Pop" "LF"  "NW" 
## Number of terminal nodes:  7 
## Residual mean deviance:  47400 = 1900000 / 40 
## Distribution of residuals:
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   -574     -98      -2       0     111     490 

# Most of the variables were excluded as they didn't provide enough information gain by being included.


# Plot the regression tree
plot(tree.data)
text(tree.data)

# Try pruning the tree
termnodes <- 5
prune.data <- prune.tree(tree.data, best = termnodes)

# Plot the pruned tree
plot(prune.data)
text(prune.data)

############################# Calculate R^2 values #############################

# Calculate SSres of the regression model.
yhat <- predict(tree.data)
yhatprune <- predict(prune.data)
SSres <- sum((yhat-data$Crime)^2)
SSresprune <- sum((yhatprune-data$Crime)^2)


# Plots of actual vs. predicted crime values
plot(data$Crime, yhat)
abline(0,1)

plot(data$Crime, yhatprune)
abline(0,1)


# Plots of the residuals
plot(data$Crime, scale(yhat - data$Crime))
abline(0,0)

plot(data$Crime, scale(yhatprune - data$Crime))
abline(0,0)


# Calculate SStot and R-squared of this model on the training data
SStot <- sum((data$Crime - mean(data$Crime))^2)

R2 <- 1 - SSres/SStot
R2
## 0.724

R2prune<- 1 - SSresprune/SStot
R2prune
## 0.669

# Couple things to point out here - the pruned r^2 value is lower than the unpruned which makes me think that there is still value in the amount of branching done in the unpruned tree.
# So going forward, I will use the unpruned tree.
# Second, we did not perform cross validation of any kind here, so we should check to see what the sum of squared error looks like.


############################# Checking squared error ############################# 

#
# Here's the sum of squared errors for trees of each size, from 7 nodes down to 1 node
# from 7 nodes down to 1 node (1 leaf)

prune.tree(tree.data)$size
prune.tree(tree.data)$dev

## [1] 7 6 5 4 3 2 1
## [1] 1895722 2013257 2276670 2632631 3364043 4383406 6880928

# Now, compare with the sum of squared errors in cross-validation:
cv.data <- cv.tree(tree.data) # cross-validation
cv.data$size
cv.data$dev

## [1] 7 6 5 4 3 2 1
## [1] 7359435 7369732 7271228 7492308 8612002 7413281 8088892

# These errors are much larger than the non cross validated trees. Meaning that our model is simply very much so overfitting this data.
# Now based on this homework assignment, we need to perform regression on the leaves of the tree; however, the number of data points are too small to create meaningful models from.

# As a workaround, let's create a 1-branch tree with 2 leaves:

prune.data <- prune.tree(tree.data,best=2)

p <- predict(prune.data)
1 - sum((p - data$Crime)^2)/SStot

## [1] 0.363
#
# This is the observed R-squared on the training data.
# Now let's try cross-validation

cv.data <- cv.tree(prune.data)
cv.data$size
cv.data$dev

# Again, the error is very high, but instead of using the average value of each leaf, let's try using a regression model for each leaf.

############################# Create regression leaves ############################# 

# Separate rows of data in each leaf
d1 <- data[which(prune.data$where == 2),]
d2 <- data[which(prune.data$where == 3),]

# First leaf:
m1 <- lm(Crime~.,data=d1)
summary(m1)

# But only four factors are even marginally significant, so refine:
m1b <- lm(Crime~Ed+Pop+Prob+Time,data=d1)
summary(m1b)

# But now only two factors are even marginally significant, so refine:
m1c <- lm(Crime~Pop+Time,data=d1)
summary(m1c)

# And now just one factor is significant:
m1d <- lm(Crime~Pop,data=d1)
summary(m1d)

# R-squared on training data is 0.296, now estimate with leave-one-out cross-validation (since we have few data points):
c1d <- cv.lm(d1,m1d,m=nrow(d1))
1 - attr(c1d,"ms")*nrow(d1)/sum((d1$Crime - mean(d1$Crime))^2)
## [1] 0.181

# But what if we used PCA for this instead, especially because we have so few data points?
p1 <- prcomp(~.,data=d1[,1:15],scale.=TRUE)

# let's use the first two components
mp1 <- lm(V1~.,data=as.data.frame(cbind(d1[,16],p1$x[,1:2])))
summary(mp1)

# Only one of these two components is significant (the second), so let's just use it:
mp2 <- lm(V1~.,data=as.data.frame(cbind(d1[,16],p1$x[,2])))
summary(mp2)
## Multiple R-squared:  0.383,     Adjusted R-squared:  0.354 

# Now, cross-validation:
cm2 <- cv.lm(as.data.frame(cbind(d1[,16],p1$x[,2])),mp2,m=nrow(d1))
1 - attr(cm2,"ms")*nrow(d1)/sum((d1$Crime - mean(d1$Crime))^2)
## [1] 0.304

# So, this is appears to have been a true improvement to the model.


# Now, let's try the second leaf.
m2 <- lm(Crime~.,data=d2)
summary(m2)

# Only one factor is significant:
m2b <- lm(Crime~Ineq,data=d2)
summary(m2b)

# None of the factors are significant!

# Let's see what happens when we use PCA. 
p2 <- prcomp(~.,data=d2[,1:15],scale.=TRUE)

# let's use the first two components
mp2 <- lm(V1~.,data=as.data.frame(cbind(d2[,16],p2$x[,1:2])))
summary(mp2)

# None of these factors are significant either!
#
# So, what it means is that we can get a reasonable model for the first leaf (Po1 < 7.65) using PCA + regression,
# but for the second leaf (Po1 > 7.65) we don't have a good model.
#
# So, it shows that for cities with Po1 > 7.65, we need to work more on finding good predictive factors, and/or finding a good model.
# The challenge of this homework assignment was working with such small amounts of data. I would be interested to see how this performs in another case where larger amounts of data is available.
