factors_em_2 <- function(x, kmax, jj, DEMEAN) {
  # PART 1: CHECKS

  # Check that x is not missing values for an entire row
  if (any(rowSums(is.na(x)) == ncol(x))) {
    stop("Input x contains entire row of missing values.")
  }

  # Check that x is not missing values for an entire column
  if (any(colSums(is.na(x)) == nrow(x))) {
    #stop("Input x contains entire column of missing values.")
    cat("Input x contains entire column of missing values.")
    Index_missing <- which(colSums(is.na(x)) == nrow(x))      # It counts how many 'NA' are in a column. If the
                                                              # number of NA in a colum is equal to the number of rows
                                                              # then, the condition is met.
    x_new <- subset(x,select = -Index_missing)
  } else {x_new <- x}

  # Check that kmax is an integer between 1 and the number of columns of x, or 99
  if (!((kmax <= ncol(x_new) && kmax >= 1 && floor(kmax) == kmax) || kmax == 99)) {
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

  # Number of observations per series in x_new (i.e. number of rows)
  T <- nrow(x_new)


  # Set error to arbitrarily high number
  err <- 999

  # Set iteration counter to 0
  it <- 0

  # Locate missing values in x_new
  x1 <- is.na(x_new)

  # PART 3: INITIALIZE EM ALGORITHM
  # Fill in missing values for each series with the unconditional mean of that series.
  # Demean and standardize the updated dataset. Estimate factors using the demeaned and standardized dataset,
  # and use these factors to predict the original dataset.

  # Get unconditional mean of the non-missing values of each series
  mut <- matrix(rep(colMeans(x_new, na.rm = TRUE), T), nrow = nrow(x_new), ncol = ncol(x_new), byrow = TRUE)

  # Replace missing values with unconditional mean
  x2 <- x_new
  x2[is.na(x2)] <- mut[is.na(x2)]         # we replace the NA values in the vector x2 with the corresponding non-NA
                                        # values from the vector mut.
  # Check whether there are entire columns of zeros
  Index_zeros <- which(colSums(x2==0) == nrow(x2))
  x2_new <- subset(x2,select = -Index_zeros)
  x1_new <- subset(x1,select = -Index_zeros)
  #II_down <- sum(Index_missing < Index_zeros)
  #II_up <- sum(Index_missing > Index_zeros)
  #Index_zeros_x <- which(colSums(x==0|is.na(x)) == nrow(x))
  #Index_zeros_x <- as.matrix(Index_zeros_x,nrow=length(Index_zeros_x))
  #x_new <- subset(x,select = -Index_zeros_x[II_down+1,])
  x_new <- subset(x_new,select = -Index_zeros)

  # Number of series in x2_new (i.e. number of columns)
  N <- ncol(x2_new)

  # Demean and standardize data
  x3 <- transform_data(x2_new, DEMEAN)

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
        if (x1_new[t, j] == 1) {
          x2_new[t, j] <- chat0[t, j] * x3$sdt[t, j] + x3$mut[t, j]
        } else {
          x2_new[t, j] <- x_new[t, j]
        }
      }
    }

    # ESTIMATE FACTORS
    # Demean and standardize the new data and recalculate mut and sdt using subfunction "transform_data()"
    #   x3  = transformed dataset
    #   mut = matrix containing the values subtracted from x2 during the transformation
    #   sdt = matrix containing the values that x2 was divided by during the transformation
    x3 <- transform_data(x2_new, DEMEAN)
    X3_x22 <- as.matrix(x3$x22)
    if (any(colSums(is.na(x3$x22)) == nrow(x3$x22))) {
      cat("Input x3$x22 contains entire column of missing values.")
      Index_missing <- which(colSums(is.na(x3$x22)) == nrow(x3$x22))      # It counts how many 'NA' are in a column. If the
      # number of NA in a colum is equal to the number of rows
      # then, the condition is met.
      x3_x22_new <- subset(x3$x22,select = -Index_missing)
    } else {x3_x22_new <- x3$x22}

    # Determine number of factors to estimate for the new dataset using subfunction "baing()"
    # (or set to 8 if kmax equals 99)
    if (kmax != 99) {
      icstar <- baing(x3_x22_new, kmax, jj)$ic1
    } else {
      icstar <- 8
    }

    # Run principal components on the new dataset using subfunction "pc2()"
    #   chat   = values of x22 predicted by the factors
    #   Fhat   = factors scaled by (1/sqrt(N)) where N is the number of
    #            series
    #   lamhat = factor loadings scaled by number of series
    #   ve2    = eigenvalues of x3'*x3
    pc_result <- pc2(x3_x22_new, icstar)
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
  ehat <- x_new - chat * x3$sdt - x3$mut


  return(list(ehat = ehat, Fhat = Fhat, lamhat = lamhat, ve2 = ve2, x2 = x2))
}

#----------------------------------------------------------------------------------------------------------------
################################################
################# SUBFUNCTIONS #################
################################################

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

#########################
## SUBFUNCTION minindc ##
#########################
minindc <- function(x) {
  apply(x, 2, which.min)
}


