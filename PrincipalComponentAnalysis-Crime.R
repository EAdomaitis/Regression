# This model was created for a homework assignment using a small crime dataset on statsci.org.
# The goal is to perform linear regression using the top 4 principal components to predict crime rates.

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
#rm(list = ls())


# Setting the random number generator seed

set.seed(1)

# Reading in the data
data <- read.table("http://www.statsci.org/data/general/uscrime.txt", stringsAsFactors = FALSE, header = TRUE)
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




############################ Calculating Principal Components ############################ 

# Run PCA on matrix of scaled predictors
pca <- prcomp(data[,1:15], scale. = TRUE)
summary(pca)

## Importance of components:
##                          PC1   PC2   PC3    PC4    PC5    PC6    PC7    PC8    PC9   PC10   PC11    PC12    PC13   PC14    PC15
## Standard deviation     2.453 1.674 1.416 1.0781 0.9789 0.7438 0.5673 0.5544 0.4849 0.4471 0.4191 0.35804 0.26333 0.2418 0.06793
## Proportion of Variance 0.401 0.187 0.134 0.0775 0.0639 0.0369 0.0214 0.0205 0.0157 0.0133 0.0117 0.00855 0.00462 0.0039 0.00031
## Cumulative Proportion  0.401 0.588 0.722 0.7992 0.8631 0.9000 0.9214 0.9419 0.9576 0.9709 0.9826 0.99117 0.99579 0.9997 1.00000

# Getting the first 4 principal components
#------------------

# Method 1: direct from prcomp output
PCs <- pca$x[,1:4]

# Method 2: calculated from prcomp output
data.scale <- as.data.frame(scale(data[,1:15]))
data.mat = as.matrix(data.scale)
PCs2 <- data.mat %*% pca$rotation[,1:4]


############################ Performing Linear Regression ############################ 
# Build linear regression model with the first 4 principal components

PCcrime <- cbind(PCs, data[,16])
model <- lm(V5~., data = as.data.frame(PCcrime))
summary(model)

## Call:
## lm(formula = V5 ~ ., data = as.data.frame(PCcrime))
## 
## Residuals:
##   Min     1Q Median     3Q    Max 
## -557.8 -210.9  -29.1  197.3  810.3 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    905.1       49.1   18.44   <2e-16 ***
## PC1             65.2       20.2    3.23   0.0024 ** 
## PC2            -70.1       29.6   -2.36   0.0227 *  
## PC3             25.2       35.0    0.72   0.4760    
## PC4             69.4       46.0    1.51   0.1387    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 336 on 42 degrees of freedom
## Multiple R-squared:  0.309,	Adjusted R-squared:  0.243 
## F-statistic:  4.7 on 4 and 42 DF,  p-value: 0.00318

# This model has R^2 = 0.309 and R^2_adj = 0.243. See below for followup.


############################ Calculating Coefficients of the model ############################ 

#Scaled coefficients

# Coefficients for this linear regression model
beta0 <- model$coefficients[1]
beta0

## (Intercept) 
## 905 

betas <- model$coefficients[2:5]
betas

##  PC1   PC2   PC3   PC4 
## 65.2 -70.1  25.2  69.4 

# Transform the PC coefficients into coefficients for the original variables

alphas <- pca$rotation[,1:4] %*% betas
t(alphas)

##          M   So   Ed  Po1  Po2  LF   M.F  Pop   NW    U1   U2 Wealth  Ineq Prob  Time
## [1,] -21.3 10.2 14.4 63.5 64.6 -14 -24.4 39.8 15.4 -27.2 1.43   38.6 -27.5  3.3 -6.61

# These are the alphas using scaled data. Now, we have to convert back to the original data.


# When scaling, this function subtracts the mean and divides by the standard deviation, for each variable.
#
# So, alpha * (x - mean)/sd = originalAlpha * x:
# (1) originalAlpha = alpha/sd
# (2) we have to modify the constant term a0 by alpha*mean/sd

originalAlpha <- alphas/sapply(data[,1:15],sd)
originalBeta0 <- beta0 - sum(alphas*sapply(data[,1:15],mean)/sapply(data[,1:15],sd))

# Here are the coefficients for unscaled data:
t(originalAlpha)
# M   So   Ed  Po1  Po2   LF   M.F  Pop  NW    U1   U2 Wealth Ineq Prob   Time
# [1,] -16.9 21.3 12.8 21.4 23.1 -347 -8.29 1.05 1.5 -1510 1.69   0.04 -6.9  145 -0.933

originalBeta0
## 1667



# And now calculate R^2 and R^2_adj
SSE = sum((estimates - data[,16])^2)
SStot = sum((data[,16] - mean(data[,16]))^2)
1 - SSE/SStot
## 0.309

R2 <- 1 - SSE/SStot
R2 - (1 - R2)*4/(nrow(data)-4-1)

## 0.243

# Here are the estimates from this model:
estimates <- as.matrix(data[,1:15]) %*% originalAlpha + originalBeta0
estimates

#Comparison of results to actual and linear regression estimates. Note this will not run unless you've run the LinearRegression-Crime models and have them stored in your global environment
#cbind( c$Crime, c$cvpred,round(((c$cvpred/c$Crime)-1)*100,digits = 2), estimates, round(((estimates/c$Crime)-1)*100,digits = 2))

#     Actual  CVpred %errorCVpred PCAest %errorPCAest
# [1,]  791  762  -3.66  726  -8.17
# [2,] 1635 1368 -16.35  927 -43.30
# [3,]  578  390 -32.54  630   9.06
# [4,] 1969 1858  -5.64 1368 -30.52
# [5,] 1234 1337   8.34 1014 -17.84
# [6,]  682  707   3.66 1216  78.31
# [7,]  963  694 -27.95  982   2.01
# [8,] 1555 1282 -17.56 1107 -28.81
# [9,]  856  657 -23.21  788  -7.90
# [10,]  705  777  10.19  758   7.55
# [11,] 1674 1019 -39.10 1236 -26.19
# [12,]  849  660 -22.24  914   7.63
# [13,]  511  842  64.81  806  57.75
# [14,]  664  712   7.20  824  24.06
# [15,]  798  805   0.84  664 -16.79
# [16,]  946  986   4.20  845 -10.67
# [17,]  539  469 -12.94  774  43.64
# [18,]  929  672 -27.69 1192  28.30
# [19,]  750 1316  75.45 1286  71.45
# [20,] 1225 1211  -1.16 1089 -11.08
# [21,]  742  836  12.73  825  11.21
# [22,]  439  767  74.63  612  39.36
# [23,] 1216  871 -28.39  926 -23.84
# [24,]  968  880  -9.12  758 -21.65
# [25,]  523  671  28.23  788  50.73
# [26,] 1993 1896  -4.88 1183 -40.66
# [27,]  342  334  -2.30  900 163.09
# [28,] 1216 1253   3.01  781 -35.80
# [29,] 1043 1693  62.32 1535  47.15
# [30,]  696  631  -9.31  743   6.80
# [31,]  373  424  13.62  580  55.49
# [32,]  754  778   3.13 1046  38.67
# [33,] 1072  850 -20.70  790 -26.35
# [34,]  923 1032  11.80 1007   9.14
# [35,]  653  777  18.92 1067  63.46
# [36,] 1272 1163  -8.59  982 -22.77
# [37,]  831 1115  34.19  646 -22.26
# [38,]  566  511  -9.69  611   7.90
# [39,]  826  810  -1.90  739 -10.51
# [40,] 1151 1187   3.13  922 -19.90
# [41,]  880  813  -7.65  844  -4.12
# [42,]  542  302 -44.26  757  39.64
# [43,]  823 1091  32.61  845   2.65
# [44,] 1030 1191  15.68  965  -6.28
# [45,]  455  612  34.49  610  34.11
# [46,]  508  839  65.13 1051 106.97
# [47,]  849  998  17.53  878   3.43

#You can see that PCA predictions were generally much further from the correct population. One explanation for this could be that PCA does not perform well with 
#Binary data and since one of the factors contains binary information, that may have caused it to produce more inaccurate results.