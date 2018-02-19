# This model was created for a homework assignment using a small crime dataset on statsci.org.
# The goal is to use linear regression to predict crime rates in a city with provided characteristics given in the test dataframe

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

#  Run function
PackagesPrep( c("DAAG" ) )


# Clear environment
rm(list = ls())


# Setting the random number generator seed

set.seed(1)

# Reading in the data
data <- read.table("http://www.statsci.org/data/general/uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#
# check to make sure the data is read correctly
#

head(data)

# M So   Ed  Po1  Po2    LF   M.F Pop   NW    U1  U2 Wealth Ineq     Prob    Time Crime
# 1 15.1  1  9.1  5.8  5.6 0.510  95.0  33 30.1 0.108 4.1   3940 26.1 0.084602 26.2011   791
# 2 14.3  0 11.3 10.3  9.5 0.583 101.2  13 10.2 0.096 3.6   5570 19.4 0.029599 25.2999  1635
# 3 14.2  1  8.9  4.5  4.4 0.533  96.9  18 21.9 0.094 3.3   3180 25.0 0.083401 24.3006   578
# 4 13.6  0 12.1 14.9 14.1 0.577  99.4 157  8.0 0.102 3.9   6730 16.7 0.015801 29.9012  1969
# 5 14.1  0 12.1 10.9 10.1 0.591  98.5  18  3.0 0.091 2.0   5780 17.4 0.041399 21.2998  1234
# 6 12.1  0 11.0 11.8 11.5 0.547  96.4  25  4.4 0.084 2.9   6890 12.6 0.034201 20.9995   682
#
# Crime is response, other variables are predictors
#

########################### Linear Regression ########################### 

#Create the test datapoint manually using dataframe
test <-data.frame(M = 14.0,So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.040,Time = 39.0)

model <- lm( Crime ~ ., data = data)

#Summary of the model
summary(model)

## Residuals:
##   Min      1Q  Median      3Q     Max 
## -395.74  -98.09   -6.69  112.99  512.67 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -5.984e+03  1.628e+03  -3.675 0.000893 ***
## M            8.783e+01  4.171e+01   2.106 0.043443 *  
## So          -3.803e+00  1.488e+02  -0.026 0.979765    
## Ed           1.883e+02  6.209e+01   3.033 0.004861 ** 
## Po1          1.928e+02  1.061e+02   1.817 0.078892 .  
## Po2         -1.094e+02  1.175e+02  -0.931 0.358830    
## LF          -6.638e+02  1.470e+03  -0.452 0.654654    
## M.F          1.741e+01  2.035e+01   0.855 0.398995    
## Pop         -7.330e-01  1.290e+00  -0.568 0.573845    
## NW           4.204e+00  6.481e+00   0.649 0.521279    
## U1          -5.827e+03  4.210e+03  -1.384 0.176238    
## U2           1.678e+02  8.234e+01   2.038 0.050161 .  
## Wealth       9.617e-02  1.037e-01   0.928 0.360754    
## Ineq         7.067e+01  2.272e+01   3.111 0.003983 ** 
## Prob        -4.855e+03  2.272e+03  -2.137 0.040627 *  
## Time        -3.479e+00  7.165e+00  -0.486 0.630708    
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 209.1 on 31 degrees of freedom
## Multiple R-squared:  0.8031,	Adjusted R-squared:  0.7078 
## F-statistic: 8.429 on 15 and 31 DF,  p-value: 3.539e-07


# Based on these results, we can see there are many variables that are not significant. Let's try again using variables significant at the 0.1 level.

model2 <- lm( Crime ~  M + Ed + Po1 + U2 + Ineq + Prob, data = data)

#Summary of the model
summary(model2)

## Call:
##   lm(formula = Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data = data)
## 
## Residuals:
##   Min      1Q  Median      3Q     Max 
## -470.68  -78.41  -19.68  133.12  556.23 
##
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -5040.50     899.84  -5.602 1.72e-06 ***
##   M             105.02      33.30   3.154  0.00305 ** 
##   Ed            196.47      44.75   4.390 8.07e-05 ***
##   Po1           115.02      13.75   8.363 2.56e-10 ***
##   U2             89.37      40.91   2.185  0.03483 *  
##  Ineq           67.65      13.94   4.855 1.88e-05 ***
##  Prob        -3801.84    1528.10  -2.488  0.01711 *  
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 200.7 on 40 degrees of freedom
## Multiple R-squared:  0.7659,	Adjusted R-squared:  0.7307 
## F-statistic: 21.81 on 6 and 40 DF,  p-value: 3.418e-11

#All variables are still considered significant.

#Check prediction
pred_model2 <- predict(model2, test)
pred_model2 ## 1304.245 

#This seems like a reasonable prediction.

# While this dataset is very small at only 47 observations, I will still use cross validation to see how results compare.

########################### Cross Validated ########################### 

# The DAAG package has cross-validation functions

# do 5-fold cross-validation

c <- cv.lm(data,model2,m=5) # 5 fold cross validation
c

# For a comparison of the models, let's calculate R^2 to see how each model performed
# R-squared = 1 - SSEresiduals/SSEtotal

# total sum of squared differences between data and its mean
SStot <- sum((data$Crime - mean(data$Crime))^2)

# model2 and cross-validation, calculated SEres
SSres_model2 <- sum(model2$residuals^2)
SSres_c <- attr(c,"ms")*nrow(data) # mean squared error, times number of data points, gives sum of squared errors


########################### Calculate R-squareds for model2 and cross-validation ########################### 

1 - SSres_model2/SStot # model2 without insignificant factors
## 0.766
1 - SSres_c/SStot # cross-validated
## 0.638

# While the R^2 is higher for model2, the amount of data we are working with is exceptionally small 
# so it wouldn't surprise me if there is some overfitting going on.
# Based on the cross validated R^2 being much lower, I think that supports that hypothesis.

