transform_data <- function(x2, DEMEAN) {
  
  # =========================================================================
  # DESCRIPTION
  # This function transforms a given set of series based upon the input variable DEMEAN.
  #  The following transformations are possible:
  #
  #   1) No transformation.
  #
  #   2) Each series is demeaned only (i.e. each series is rescaled to have a mean of 0).
  #
  #   3) Each series is demeaned and standardized (i.e. each series is rescaled to have a mean of 0 and a standard
  #       deviation of 1).
  #
  #   4) Each series is recursively demeaned and then standardized. For a given series x(t), where t=1,...,T,
  #     the recursively demeaned series x'(t) is calculated as x'(t) = x(t) - mean(x(1:t)). After the recursively
  #     demeaned series x'(t) is calculated, it is standardized by dividing x'(t) by the standard deviation of the
  #     original series x. Note that this transformation does not rescale the original series to have a specified mean
  #     or standard deviation.
  #
  # -------------------------------------------------------------------------
  # INPUTS
  #           x2 = set of series to be transformed (one series per column); no missing values;
  #           DEMEAN  = an integer indicating the type of transformation performed on each series in x2; it can take
  #                     on the following values:
  #                           0 (no transformation)
  #                           1 (demean only)
  #                           2 (demean and standardize)
  #                           3 (recursively demean and then standardize)
  #
  # OUTPUTS
  #           x22     = transformed dataset
  #           mut     = matrix containing the values subtracted from x2
  #                     during the transformation
  #           sdt     = matrix containing the values that x2 was divided by
  #                     during the transformation
  #
  # =========================================================================
  # FUNCTION
  # Number of observations in each series (i.e. number of rows in x2)
  T <- nrow(x2)
  
  # Number of series (i.e. number of columns in x2)
  N <- ncol(x2)
  
  # Perform transformation based on type determined by 'DEMEAN'
  if (DEMEAN == 0){
    # No transformation
    mut <- matrix(0, nrow = T, ncol = N)
    sdt <- matrix(1, nrow = T, ncol = N)
    x22 <- x2
  } else if (DEMEAN == 1){
    # Each series is demeaned only
    mut <- matrix(rep(mean(x2), each = T), nrow = T, ncol = N)
    sdt <- matrix(1, nrow = T, ncol = N)
    x22 <- x2 - mut
  } else if (DEMEAN == 2){
    # Each series is demeaned and standardized
    mut <- matrix(rep(colMeans(x2), T), nrow = T, ncol = N, byrow = TRUE)
    sdt <- matrix(rep(sapply(x2,sd), T), nrow = T, ncol = N, byrow = TRUE)         # sd divides by T-1
    x22 <- (x2 - mut) / sdt
  } else if (DEMEAN == 3){
    # Each series is recursively demeaned and then standardized
    mut <- matrix(NA, nrow = T, ncol = N)
    for (t in 1:T) {
      mut[t, ] <- colMeans(x2[1:t, , drop = FALSE])
    }
    sdt <- matrix(rep(sd(x2),T), nrow = T, ncol = N)
    x22 <- (x2 - mut) / sdt
  }
  
  # Return the results
  return(list(x22 = x22, mut = mut, sdt = sdt))
}



remove_outliers <- function(X) {
  # =========================================================================
  # DESCRIPTION:
  # This function takes a set of series aligned in the columns of a matrix
  # and replaces outliers with the value NA.
  #
  # -------------------------------------------------------------------------
  # INPUT:
  #           X   = dataset (one series per column)
  #
  # OUTPUT:
  #           Y   = dataset with outliers replaced with NA
  #
  # -------------------------------------------------------------------------
  # NOTES:
  #           1) Outlier definition: a data point x of a series X(:,i) is
  #           considered an outlier if abs(x-median)>10*interquartile_range.
  #
  #           2) This function ignores values of NaN and thus is capable of
  #           replacing outliers for series that have missing values.
  #
  # =========================================================================
  # Error checking
  if (!inherits(X, "fredmd"))
    stop("Object must be of class 'fredmd'")
  #             - inherits(object, "fredmd"): The inherits function is used to check if an object inherits from a
  #               particular class.
  #             - !inherits(object, "fredmd"): The ! operator negates the result of the inherits function. So, this
  #               condition is true if the object does not inherit from the class "fredmd".
  #             - stop("Object must be of class 'fredmd'"): If the condition is true, the stop function is called.
  #               The stop function is used to generate an error message and halt the execution of the program.
  #             So, we are checking if the object is not of class "fredmd" and, if so, it stops the execution.
  # =========================================================================
  # Calculate median of each series
  #             We use the 'apply' function to calculate the median for each
  #             column (i.e., margin 2) of a matrix 'X'
  #             -'X': This is the matrix or data frame for which you want to calculate column-wise medians.
  #             -'2': This argument specifies that the operation should be applied to each column.
  #                 In R, 1 would mean rows, and 2 means columns.
  #             - stats::median: This is the function that will be applied to each column (or row) of the matrix.
  #               In this case, it's the median function from the stats package.
  #             - na.rm = TRUE: This argument specifies whether to remove missing values (NA) before applying
  #               the function. In this case, na.rm = TRUE means that any missing values in each column will be
  #               ignored when calculating the median.
  #  So, overall, this command is calculating the median of each column in the matrix X, ignoring any missing values
  #  in each column. The result will be a vector of median values, one for each column of the matrix.
  median_X <- apply(X, 2, stats::median, na.rm = TRUE)
  # median_X <- matrix(median_X)
  # median_X <- t(median_X)
  
  # Repeat median of each series over all data points in the series
  median_X_mat <- matrix(rep(median_X, nrow(X)), nrow = nrow(X),
                         ncol = ncol(X), byrow = TRUE)
  #             - rep(median_X, nrow(X)): The rep function is used to replicate the value median_X a certain number
  #               of times. In this case, it is replicated nrow(X) times, where nrow(X) is the number of rows in the
  #               matrix X. This creates a vector of repeated median values.
  #             - matrix(...): This function is then used to convert the repeated median vector into a matrix.
  #                 nrow = nrow(X): Specifies the number of rows in the resulting matrix, which is the same as the
  #                                 number of rows in the original matrix X.
  #                 ncol = ncol(X): Specifies the number of columns in the resulting matrix, which is the same as
  #                                 the number of columns in the original matrix X.
  #                 byrow = TRUE: This argument specifies that the values in the repeated median vector should be
  #                               filled into the matrix by rows. If byrow = FALSE (or not specified), the filling would be done by columns.
  # So, overall, this command creates a matrix where each row contains the same repeated median value, and the
  # number of rows and columns in the new matrix is the same as the number of rows and columns in the original
  # matrix.
  
  # Calculate quartiles
  Q <- apply(X, 2, stats::quantile, probs = c(0.25, 0.75), na.rm = TRUE)
  
  # Calculate interquartile range (IQR) of each series
  IQR <- Q[2,] - Q[1,]
  
  # Repeat IQR of each series over all data points in the series
  IQR_mat <- matrix(rep(IQR, nrow(X)), nrow = nrow(X),
                    ncol = ncol(X), byrow = TRUE)
  
  # Determine outliers
  Z <- abs(X - median_X_mat)
  outlier <- (Z > (10 * IQR_mat))
  
  # Replace outliers with NaN
  Y <- X
  Y[outlier] <- NA
  
  # Cleaned data
  outdata <- Y
  # We set the class attribute of the object outdata to be a combination of two classes: "data.frame" and "fredmd".
  # In R, an object can belong to multiple classes. The class function is used to query or set the class of an object.
  # In this case, it's setting the class of outdata to be both "data.frame" and "fredmd".
  class(outdata) <- c("data.frame", "fredmd")
  return(outdata)
  
  # Print the number of outliers
  print("Number of outliers:", quote = FALSE)     # quote = FALSE: The quote argument in the print function controls
  # whether or not to print quotation marks around the character
  # string. When quote = FALSE, it means that the string will be
  # printed without quotation marks.
  print(sum(outlier, na.rm = TRUE), quote = FALSE)  # na.rm = TRUE: The na.rm argument is set to TRUE, which means
  # that any missing values (NA) in the vector or matrix will be
  # removed before calculating the sum.
  # If na.rm is set to FALSE or not specified, the presence of any
  # missing values would result in the sum being reported as NA.
}

# ================================================================================================================
# ================================================================================================================
# ================================================================================================================

baing <- function(X, kmax, jj) {
  # =========================================================================
  # DESCRIPTION
  # This function determines the number of factors to be selected for a given
  # dataset using one of three information criteria specified by the user.
  # The user also specifies the maximum number of factors to be selected.
  #
  # -------------------------------------------------------------------------
  # INPUTS
  #           X       = dataset (one series per column)
  #           kmax    = an integer indicating the maximum number of factors
  #                     to be estimated
  #           jj      = an integer indicating the information criterion used
  #                     for selecting the number of factors; it can take on
  #                     the following values:
  #                           1 (information criterion IC_p1)
  #                           2 (information criterion IC_p2)
  #                           3 (information criterion IC_p3)
  #
  # OUTPUTS
  #           ic1     = number of factors selected
  #           chat    = values of X predicted by the factors
  #           Fhat    = factors
  #           eigval  = eivenvalues of X'*X (or X*X' if N>T)
  # -------------------------------------------------------------------------
  #
  # PART 1: SETUP
  
  # Number of observations per series (i.e. number of rows)
  T <- nrow(X)
  
  # Number of series (i.e. number of columns)
  N <- ncol(X)
  
  # Total number of observations
  NT <- N * T
  
  # Number of rows + columns
  NT1 <- N + T
  
  # PART 2: OVERFITTING PENALTY
  # Determine penalty for overfitting based on the selected information criterion.
  
  # Allocate memory for overfitting penalty
  #CT <- numeric(kmax)
  
  # Array containing possible number of factors that can be selected (1 to kmax)
  ii <- 1:kmax
  
  # The smaller of N and T
  GCT <- min(N, T)
  
  # Calculate penalty based on criterion determined by jj.
  if (jj == 1){
    # Criterion PC_p1
    CT <- log(NT / NT1) * ii * NT1 / NT
  } else if (jj == 2){
    # Criterion PC_p2
    CT <- (NT1 / NT) * log(min(N, T)) * ii
  } else if (jj == 3){
    # Criterion PC_p3
    CT <- ii * log(GCT) / GCT
  }
  
  # PART 3: SELECT NUMBER OF FACTORS
  # Perform principal component analysis on the dataset and select the number
  # of factors that minimizes the specified information criterion.
  
  # RUN PRINCIPAL COMPONENT ANALYSIS
  
  # Get components, loadings, and eigenvalues
  XX <-as.matrix(X)
  if (T < N) {
    # Singular value decomposition
    svd_result <- svd(XX %*% t(XX))
    
    # Components
    Fhat0 <- sqrt(T) * svd_result$u
    
    # Loadings
    Lambda0 <- t(XX) %*% Fhat0 / T
    
  } else {
    # Singular value decomposition
    svd_result <- svd(t(XX) %*% XX)       # Alternatively, you can use "eigen(t(XX) %*% XX)" and then "svd_result$vectors"
    
    # Loadings
    Lambda0 <- sqrt(N) * svd_result$u
    
    # Components
    Fhat0 <- XX %*% Lambda0 / N
    
  }
  
  # SELECT NUMBER OF FACTORS
  
  # Preallocate memory
  Sigma <- numeric(kmax + 1)  # sum of squared residuals divided by NT
  # "numeric()": it creates numeric vectors or matrices.
  #It initializes the vector or matrix with numeric values (defaulting to zeros).
  IC1 <- matrix(0, nrow = 1, ncol = kmax + 1)  # information criterion value
  
  # Loop through all possibilities for the number of factors
  for (i in kmax:1) {
    
    # Identify factors as first i components
    Fhat <- Fhat0[, 1:i]
    
    # Identify factor loadings as first i loadings
    lambda <- Lambda0[, 1:i]
    
    # Predict X using i factors
    chat <- Fhat %*% t(lambda)
    
    # Residuals from predicting X using the factors
    ehat <- X - chat
    
    # Sum of squared residuals divided by NT
    Sigma[i] <- mean(colSums(ehat^2/T))
    
    # Value of the information criterion when using i factors
    IC1[i] <- log(Sigma[i]) + CT[i]
  }
  
  # Sum of squared residuals when using no factors to predict X (i.e.
  # fitted values are set to 0)
  Sigma[kmax + 1] <- mean(colSums(X^2) / T)
  
  # Value of the information criterion when using no factors
  IC1[, kmax + 1] <- log(Sigma[kmax + 1])
  
  # Number of factors that minimizes the information criterion
  ic1 <- minindc(t(IC1))
  
  # Set ic1 = 0 if ic1 > kmax (i.e. no factors are selected if the value of the
  # information criterion is minimized when no factors are used)
  ic1 <- ifelse(ic1 > kmax, 0, ic1)
  
  # PART 4: SAVE OTHER OUTPUT
  
  # Factors and loadings when the number of factors set to kmax
  Fhat <- Fhat0[, 1:kmax]  # factors
  Lambda <- Lambda0[, 1:kmax]  # factor loadings
  
  # Predict X using kmax factors
  chat <- Fhat %*% t(Lambda)
  
  # Get the eigenvalues corresponding to X'*X (or X*X' if N > T)
  eigval <- svd_result$d
  
  # Return the results
  return(list(ic1 = ic1, chat = chat, Fhat = Fhat, eigval = eigval))
}


#########################
## SUBFUNCTION minindc ##
#########################
minindc <- function(x) {
  
  # DESCRIPTION
  # This function finds the index of the minimum value for each column of a given matrix. The function assumes that
  # the minimum value of each column occurs only once within that column. The function returns an error if this is
  # not the case.
  apply(x, 2, which.min)    # "2": it specifies that the function ("which.min" in this case) should be applied to
  # each column. If it were 1, the function would be applied to each row.
  # "which.min": it returns the index of the first minimum value in a vector.
}

# ================================================================================================================
# ================================================================================================================
# ================================================================================================================

mrsq <- function(Fhat, lamhat, ve2, series) {
  # =========================================================================
  # DESCRIPTION
  # This function computes the R-squared and marginal R-squared from
  # estimated factors and factor loadings.
  #
  # -------------------------------------------------------------------------
  # INPUTS
  #           Fhat    = estimated factors (one factor per column)
  #           lamhat  = factor loadings (one factor per column)
  #           ve2     = eigenvalues of covariance matrix
  #           series  = series names
  #
  # OUTPUTS
  #           R2      = R-squared for each series for each factor
  #           mR2     = marginal R-squared for each series for each factor
  #           mR2_F   = marginal R-squared for each factor
  #           R2_T    = total variation explained by all factors
  #           t10_s   = top 10 series that load most heavily on each factor
  #           t10_mR2 = marginal R-squared corresponding to top 10 series
  #                     that load most heavily on each factor
  #
  # N = number of series, ic = number of factors
  N <- nrow(lamhat)
  ic <- ncol(lamhat)
  
  # Preallocate memory for output
  R2 <- matrix(NA, nrow = N, ncol = ic)
  mR2 <- matrix(NA, nrow = N, ncol = ic)
  t10_s <- matrix(NA, nrow = 10, ncol = ic)
  t10_mR2 <- matrix(NA, nrow = 10, ncol = ic)
  
  # Compute R-squared and marginal R-squared for each series for each factor
  for (i in 1:ic) {
    R2[, i] <- t(apply(Fhat[, 1:i] %*% t(lamhat[, 1:i]), 2, var))
    mR2[, i] <- apply(Fhat[, i] %*% t(lamhat[, i]), 2, var)
  }
  
  # Compute marginal R-squared for each factor
  mR2_F <- ve2 / sum(ve2)
  mR2_F <- mR2_F[1:ic]
  
  # Compute total variation explained by all factors
  R2_T <- sum(mR2_F)
  
  # Sort series by marginal R-squared for each factor
  ind <- apply(mR2, 2, order, decreasing = TRUE)
  
  # Get top 10 series that load most heavily on each factor and the
  # corresponding marginal R-squared values
  for (i in 1:ic) {
    t10_s[, i] <- series[ind[1:10, i]]
    t10_mR2[, i] <- mR2[ind[1:10, i], i]
  }
  
  return(list(R2 = R2, mR2 = mR2, mR2_F = mR2_F, R2_T = R2_T, t10_s = t10_s, t10_mR2 = t10_mR2))
}

# ================================================================================================================
# ================================================================================================================
# ================================================================================================================

factors_em <- function(x, kmax, jj, DEMEAN) {
  # PART 1: CHECKS
  
  # Check that x is not missing values for an entire row
  if (any(rowSums(is.na(x)) == ncol(x))) {
    stop("Input x contains entire row of missing values.")
  }
  
  # Check that x is not missing values for an entire column
  if (any(colSums(is.na(x)) == nrow(x))) {
    stop("Input x contains entire column of missing values.")
  }
  
  # Check that kmax is an integer between 1 and the number of columns of x, or 99
  if (!((kmax <= ncol(x) && kmax >= 1 && floor(kmax) == kmax) || kmax == 99)) {
    stop("Input kmax is specified incorrectly.")
  }
  
  # Check that jj is one of 1, 2, 3
  if (!(jj %in% c(1, 2, 3))) {
    stop("Input jj is specified incorrectly.")
  }
  
  # Check that DEMEAN is one of 0, 1, 2, 3
  if (!(DEMEAN %in% 0:3)) {
    stop("Input DEMEAN is specified incorrectly.")
  }
  
  # PART 2: SETUP
  
  # Maximum number of iterations for the EM algorithm
  maxit <- 50
  
  # Number of observations per series in x (i.e. number of rows)
  T <- nrow(x)
  
  # Number of series in x (i.e. number of columns)
  N <- ncol(x)
  
  # Set error to arbitrarily high number
  err <- 999
  
  # Set iteration counter to 0
  it <- 0
  
  # Locate missing values in x
  x1 <- is.na(x)
  
  # PART 3: INITIALIZE EM ALGORITHM
  # Fill in missing values for each series with the unconditional mean of that series.
  # Demean and standardize the updated dataset. Estimate factors using the demeaned and standardized dataset,
  # and use these factors to predict the original dataset.
  
  # Get unconditional mean of the non-missing values of each series
  mut <- matrix(rep(colMeans(x, na.rm = TRUE), T), nrow = nrow(x), ncol = N, byrow = TRUE)
  
  # Replace missing values with unconditional mean
  x2 <- x
  x2[is.na(x)] <- mut[is.na(x)]         # we replace the NA values in the vector x2 with the corresponding non-NA
  # values from the vector mut.
  
  # Demean and standardize data
  x3 <- transform_data(x2, DEMEAN)
  
  # If input 'kmax' is not set to 99, use subfunction baing() to determine
  # the number of factors to estimate. Otherwise, set number of factors equal
  # to 8
  if (kmax != 99) {
    icstar <- baing(x3$x22, kmax, jj)$ic1
  } else {
    icstar <- 8
  }
  
  # Run principal components on updated dataset
  pc_result <- pc2(x3$x22, icstar)
  chat0 <- pc_result$chat
  Fhat <- pc_result$fhat
  lamhat <- pc_result$lambda
  ve2 <- pc_result$ss
  
  # PART 4: PERFORM EM ALGORITHM
  # Update missing values using values predicted by the latest set of factors.
  # Demean and standardize the updated dataset. Estimate a new set of factors using the updated dataset.
  # Repeat the process until the factor estimates do not change.
  
  # Run while error is large and have yet to exceed maximum number of iterations
  while (err > 0.000001 && it < maxit) {
    # INCREASE ITERATION COUNTER
    it <- it + 1
    
    # Display iteration counter, error, and number of factors
    cat(sprintf('Iteration %d: obj %10f IC %d \n', it, err, icstar))
    # The "cat" and "sprintf" functions in R are used for formatting and printing text to the
    # console.
    # "%d": This is a placeholder for an integer.
    # "%10f": This is a placeholder for a floating-point number, with a width of
    # 10 characters, including the decimal point.
    # \n: This is a newline character, which moves the cursor to the next line.
    
    # UPDATE MISSING VALUES
    for (t in 1:T) {
      for (j in 1:N) {
        if (x1[t, j] == 1) {
          x2[t, j] <- chat0[t, j] * x3$sdt[t, j] + x3$mut[t, j]
        } else {
          x2[t, j] <- x[t, j]
        }
      }
    }
    
    # ESTIMATE FACTORS
    # Demean and standardize the new data and recalculate mut and sdt using subfunction "transform_data()"
    #   x3  = transformed dataset
    #   mut = matrix containing the values subtracted from x2 during the transformation
    #   sdt = matrix containing the values that x2 was divided by during the transformation
    x3 <- transform_data(x2, DEMEAN)
    
    # Determine number of factors to estimate for the new dataset using subfunction "baing()"
    # (or set to 8 if kmax equals 99)
    if (kmax != 99) {
      icstar <- baing(x3$x22, kmax, jj)$ic1
    } else {
      icstar <- 8
    }
    
    # Run principal components on the new dataset using subfunction "pc2()"
    #   chat   = values of x22 predicted by the factors
    #   Fhat   = factors scaled by (1/sqrt(N)) where N is the number of
    #            series
    #   lamhat = factor loadings scaled by number of series
    #   ve2    = eigenvalues of x3'*x3
    pc_result <- pc2(x3$x22, icstar)
    chat <- pc_result$chat
    
    # CALCULATE NEW ERROR VALUE
    # Caclulate difference between the predicted values of the new dataset and the predicted values of the previous
    # dataset
    diff <- chat - chat0
    
    # The error value is equal to the sum of the squared differences between "chat" and "chat0" divided by the sum
    # of the squared values of "chat0"
    v1 <- as.vector(diff)
    v2 <- as.vector(chat0)
    err <- sum(v1^2) / sum(v2^2)
    
    # Set chat0 equal to the current chat
    chat0 <- chat
  }
  
  # Produce warning if maximum number of iterations is reached
  if (it == maxit) {
    warning('Maximum number of iterations reached in EM algorithm')
  }
  # Final Output:
  Fhat <- pc_result$fhat
  lamhat <- pc_result$lambda
  ve2 <- pc_result$ss
  
  # FINAL DIFFERENCE
  # Calculate the difference between the initial dataset and the values predicted by the final set of factors
  ehat <- x - chat * x3$sdt - x3$mut
  
  
  return(list(ehat = ehat, Fhat = Fhat, lamhat = lamhat, ve2 = ve2, x2 = x2))
}

#########################
#### SUBFUNCTION pc2 ####
#########################
pc2 <- function(X, nfac) {
  
  # Number of series in X (i.e. number of columns)
  N <- ncol(X)
  
  # Singular value decomposition: X'*X = U*S*V'
  XX <- as.matrix(X)
  svd_result <- svd(t(XX) %*% XX)
  
  # Factor loadings scaled by sqrt(N)
  lambda <- svd_result$u[, 1:nfac] * sqrt(N)
  
  # Factors scaled by 1/sqrt(N) (note that lambda is scaled by sqrt(N))
  fhat <- XX %*% lambda / N
  
  # Estimate initial dataset X using the factors (note that U'=inv(U))
  chat <- fhat %*% t(lambda)
  
  # Identify eigenvalues of X'*X
  ss <- diag(svd_result$d)
  
  # Return the results
  return(list(chat = chat, fhat = fhat, lambda = lambda, ss = ss))
}
