library(BVAR)
library(fbi)
library(stats)

source("/Users/prld/git/NoTeX/#TS/MacroE_ML/td1/functions.R")
source("/Users/prld/git/NoTeX/#TS/MacroE_ML/td1/factors_em_2.R")

## File name of desired FRED-MD vintage ##
filepath <- "https://files.stlouisfed.org/files/htdocs/fred-md/monthly/2015-04.csv"  ##
data <- fredmd(filepath, date_start = as.Date("1960-01-01"), date_end = NULL, transform = TRUE)       ##
#data <- fredmd(filepath, date_start = NULL, date_end = NULL, transform = TRUE)       ##
# The data are already transformed

# =========================================================================

# Type of transformation performed on each series before factors are
# estimated
#   0 --> no transformation
#   1 --> demean only
#   2 --> demean and standardize
#   3 --> recursively demean and then standardize
DEMEAN <- 2

# Information criterion used to select the number of factors; for more details,
# see auxiliary function factors_em()
#   1 --> information criterion PC_p1
#   2 --> information criterion PC_p2
#   3 --> information criterion PC_p3
jj <- 2

# Maximum number of factors to be estimated; if set to 99, the number of
# factors selected is forced to equal 8
kmax <- 99

# Select the variables to predict
# 6   : Industrial production (IP)
# ??? : Consumer Price Index  (CPI)
nn <- c(6,114);
#nn <- 6

HH = 12; # number of step ahead to forecast

# =========================================================================
# PART 1: LOAD AND LABEL DATA

# Variable names
series <- colnames(data[,2:length(data)])

# Raw data
rawdata <- data[2:nrow(data),2:length(data)]

# Month/year of the final observation
final_date <- tail(data$date, 1)
dates <- data$date

# T = number of months in the sample
T <- length(dates)
# Number of time points to use in the estimation of the parameter: Rolling scheme
Jwind = 120;

# Starting dates for the out-of-sample evaluation
#start_date = "1970-01-01";
start_date = "1974-01-01"; #end of training sample / forecast from this date

# =========================================================================
# PART 2: PROCESS DATA

# 1. Prepare Missing Data
#yt <- prepare_missing(rawdata, tcode)
yt <- rawdata

# 2. Reduce Sample  to usable dates: remove first two months because some
# series have been first differenced
#yt <- yt[2:nrow(yt), ]
dates <- dates[2:length(dates)]
dim_yt <- dim(yt)
TT <- dim_yt[1]
NN <- dim_yt[2]

# 3. Remove Outliers
result <- remove_outliers(yt)

# =========================================================================
start_sample <- which(dates == start_date)  # index of start_date

if (Jwind > start_sample) {
  stop("the rolling window cannot be larger than the first evaluation sample")
}

j0 <- start_sample - Jwind + 1

x_temp <- yt[j0:start_sample, ]   # The available data at the beginning of the out-of-sample evaluation exercise
# Not the complete matrix X with the complete set of data but only in-sample-data = subsample
x<- remove_outliers(x_temp)       # Remove outliers specific to this particular sample

#x[, nn] <- x_temp[, nn]  # Select the variables to predict



# Performs the out-of-sample forecasting exercise
true <- matrix(NA, nrow = TT - tail(HH, 1) - start_sample, ncol = 2)  #collect true value of the series
RW <- matrix(NA, nrow = TT - tail(HH, 1) - start_sample, ncol = 2)    #forecast made by random walk (to compare method)
PC <- matrix(NA, nrow = TT - tail(HH, 1) - start_sample, ncol = 2)    #forecast made by diffusion index = pca

# training sample
for (j in start_sample:(TT - tail(HH, 1))) {

  ## Displays the dates at the beginning of each year
  if (grepl("-01-01", dates[j]) == 1) {
    cat('--------------\n')
    cat('now running\n')
    cat(paste(dates[j], collapse = ' '), '\n')
    #cat('elapsed minutes: ', toc() / 60, '\n')
  }

  ## Define the beginning of the estimation sample
  j0 <- j - Jwind + 1  # Starting period for the in-sample

  x_temp <- yt[j0:j, ]  # The available data at each time point of the evaluation exercise
  x<- remove_outliers(x_temp)

  for (k in seq_along(nn)) {  # Loop across series to forecast

    for (h in HH) {  # Loop across the number of steps ahead, ici H=12, une seule valeur

      ## Normalization constants
      if (nn[k] %in% c(1, 6, 25)) {
        const <- 12
      } else {
        const <- h
      }

      ## Compute the true value to be predicted
      # mean over 12 values (smooth) y(t+1) to y(t+12)
      temp <- mean(yt[(j + 1):(j + h), nn[k]])  #nn[k] donne colonne pour la variables qu'on regarde
      true[j + h, k] <- temp * const

      ## Compute the Constant growth forecast
      # Apply moving average filter
      Y <- stats::filter(x$INDPRO, filter = rep(1/h, h), sides = 1)
      # Discard the initial h elements
      Y2 <- Y[(h + 1):length(Y)]
      temp <- mean(Y2)
      RW[j + h, k] <- temp * const

      ## Computes the Factor-based forecasts
      x_pred <- x[,-6]   # dataset without series number 6 = the one we predict !!
      result_factors <- factors_em_2(x_pred, kmax, jj, DEMEAN)
      #result_factors <- factors_em_2(x, kmax, jj, DEMEAN)
      # Regressors
      Z <- cbind(1, result_factors$Fhat)
      # Compute the dependent variable to be predicted
      # Apply moving average filter: Y = (y_{+1}+...+y_{+h})/h
      Y <- stats::filter(x$INDPRO, filter = rep(1/h, h), sides = 1)
      # Compute the forecasts
      Z_trimmed <- Z[1:(nrow(Z)-h), ]
      gamma <- solve(t(Z_trimmed) %*% Z_trimmed) %*% t(Z_trimmed) %*% Y[(h+1):length(Y)] #LS coefficient
      pred <- tail(Z, 1) %*% gamma

      PC[j + h, k] <- const * pred
    }
  }
}

# Assuming true, RW, and PC are lists or data frames and ind_first, ind_last, h, and k are defined
# Make sure to adjust variable names if needed

# Compute MSFE_RW and MSFE_PC
true_NA <- na.omit(true)
Index_NA <- which(is.na(true[,1]))
#dates_sub <- dates[j0:end]
#dates_NA <- dates_sub[,-Index_NA]
RW_NA <- na.omit(RW)
PC_NA <- na.omit(PC)
MSFE_RW <- mean((true_NA - RW_NA)^2)
MSFE_PC <- mean((true_NA - PC_NA)^2)

plot(1:44, true_NA,
     type = 'l', col = "black")

points(1:44, RW_NA, col="red", pch="*")
lines(1:44, PC_NA, col="blue",lty=2)
