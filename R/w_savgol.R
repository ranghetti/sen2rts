#' @title Savitzky-Golay filter for not equally-spaced weighted data
#' @description Smooth values using a Savitzky-Golay filter.
#'  This function is suitable for not equally-spaced and/or weighted data.
#' @param y List of floats representing the "y" values.
#' @param x List of floats representing the "x" values of the data.
#'  Must have same length as `y`.
#'  If missing, points are assumed to be equally-spaced
#' @param q List of floats representing the relative weight of "y" values. 
#'  Must have same length as `y`.
#'  If missing, all points are assumed to have the same weight.
#' @param window Window length of datapoints.
#'  Must be odd and smaller than `x`. Default is 7.
#' @param polynom The polynomial order to be used. 
#'  Must be smaller than the `window` size. Default is 3.
#' @return The smoothed "y" values.
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @note This is an R adaptation of Python function at
#'  \url{https://dsp.stackexchange.com/questions/1676/savitzky-golay-smoothing-filter-for-not-equally-spaced-data},
#'  with the addition of weights following 
#'  \url{https://en.wikipedia.org/wiki/Savitzky-Golay_filter}.
#' @export

w_savgol <- function(y, x, q, window = 7, polynom = 3) {
  
  ## Check parameters ----
  
  if (missing(x)) {
    x <- seq_along(y)
  }
  
  if (missing(q)) {
    q <- rep(1, length(y))
  }
  
  if (any(length(x) != length(y), length(y) != length(q))) {
    print_message(
      type = "error",
      '"x", "y" and "q" must be of the same length.'
    )
  }
  
  if (length(x) < window) {
    print_message(
      type = "error",
      'The data size must be larger than the window size.'
    )
  }

  if (!inherits(window, c("integer", "numeric")) || window %% 1 != 0) {
    print_message(
      type = "error",
      '"window" must be an integer.'
    )
  }

  if (window %% 2 == 0) {
    print_message(
      type = "error",
      'The "window" must be an odd integer.'
    )
  }

  if (!inherits(polynom, c("integer", "numeric")) || polynom %% 1 != 0) {
    print_message(
      type = "error",
      '"polynom" must be an integer.'
    )
  }
  
  if (polynom >= window) {
    print_message(
      type = "error",
      '"polynom" must be less than "window".'
    )
  }

  half_window <- trunc(window / 2)
  polynom <- polynom + 1
  
  ## Smooth ----
  
  # Initialise variables
  A <- matrix(nrow=window, ncol=polynom)     # Matrix
  tA <- matrix(ncol=window, nrow=polynom)    # Transposed matrix
  t <- numeric(window)                # Local x variables
  y_smoothed <- rep(NA, length(y))
  
  # Start smoothing
  for (i in seq(half_window+1, length(x)-half_window)) {
    
    # Center a window of x values on x[i]
    for (j in seq(window)) {
      t[j] <- x[i + j - 1 - half_window] - x[i]
    }
    
    # Diagonal matrix of weights
    w <- q[seq(i-half_window, i+half_window)] # vector of weights
    w <- w * length(w) / sum(w) # normalise
    W <- diag(w) # diagonal matrix

    # Create the initial matrix A and its transposed form tA
    for (j in seq(window)) {
      r <- 1
      for (k in seq(polynom)) {
        A[j, k] <- r
        tA[k, j] <- r
        r <- r * t[j]
      }
    }
    
    # Multiply the three matrices
    AA <- tA %*% W %*% A
    
    # Invert the product of the matrices
    tAA <- tryCatch(
      solve(AA),
      error = function(e) {
        if (requireNamespace("MASS", quietly = TRUE)) {
          MASS::ginv(AA)
        } else {
          print_message(
            type = "error",
            "Package 'MASS' is required to invert matrices with no univoc solution; ",
            "please install it.\nDetails:\n",
            e$message,"."
          )
        }
      }
    )
    
    
    # Calculate the pseudoinverse of the design matrixmappl
    coeffs <- tAA %*% tA %*% W
    
    # Calculate c0 which is also the y value for y[i]
    y_smoothed[i] <- 0
    for (j in seq(window)) {
      y_smoothed[i] <- y_smoothed[i] + coeffs[1, j] * y[i + j - 1 - half_window]
    }

    # If at the end or beginning, store all coefficients for the polynom
    if (i == half_window+1) {
      first_coeffs <- rep(0, polynom)
      for (j in seq(window)) {
        for (k in seq(polynom)) {
          first_coeffs[k] <- first_coeffs[k] + coeffs[k, j] * y[j]
        }
      }
    } else if (i == length(x) - half_window - 1) {
      last_coeffs <- rep(0, polynom)
      for (j in seq(window)) {
        for (k in seq(polynom)) {
          last_coeffs[k] <- last_coeffs[k] + coeffs[k, j] * y[length(y) - window + j]
        }
      } 
    }

  }
  
  # Interpolate the result at the left border
  for (i in seq(half_window)) {
    y_smoothed[i] <- 0
    x_i <- 1
    for (j in seq(polynom)) {
      y_smoothed[i] <- y_smoothed[i] + first_coeffs[j] * x_i
      x_i <- x_i * (x[i] - x[half_window+1])
    }
  }
  
  # Interpolate the result at the right border
  for (i in seq(length(x)-half_window+1, length(x))) {
    y_smoothed[i] <- 0
    x_i <- 1
    for (j in seq(polynom)) {
      y_smoothed[i] <- y_smoothed[i] + last_coeffs[j] * x_i
      x_i <- x_i * (x[i] - x[length(x) - half_window])
    }
  }
  
  y_smoothed
  
}
