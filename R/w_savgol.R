#' @title Savitzky-Golay smoothing filter for not equally spaced data
#' @description TODO
#' @param x List of floats representing the "x" values of the data.
#' @param y List of floats representing the "y" values. Must have same length as `x`.
#' @param q List of floats representing the relative weight of "y" values. Must have same length as `y`.
#' @param window Window length of datapoints. Must be odd and smaller than `x`.
#' @param polynom The order of polynom used. Must be smaller than the `window` size.
#' @return The smoothed "y" values.
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @note This is an R adaptation of Python function at
#'  https://dsp.stackexchange.com/questions/1676/savitzky-golay-smoothing-filter-for-not-equally-spaced-data
#' @export

w_savgol <- function(x, y, q, window, polynom) {
  
  ## Check parameters ----
  
  if (any(length(x) != length(y), length(y) != length(q))) {
    print_message(
      type = "error",
      '"x", "y" and "q" must be of the same size.'
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
  
  # Initialise variables
  A <- matrix(nrow=window, ncol=polynom)     # Matrix
  tA <- matrix(ncol=window, nrow=polynom)    # Transposed matrix
  t <- numeric(window)                # Local x variables
  y_smoothed <- rep(NA, length(y))
  
  # Start smoothing
  for (i in seq(half_window+1, length(x) - half_window, 1)) {
    
    # Center a window of x values on x[i]
    for (j in seq(1, window, 1)) {
      t[j] <- x[i + j - 1 - half_window] - x[i]
    }
    
    # Diagonal matrix of weights
    w <- q[seq(i-half_window, i+half_window)] # vector of weights
    w <- w * length(w) / sum(w) # normalise
    W <- diag(w) # diagonal matrix

    # Create the initial matrix A and its transposed form tA
    for (j in seq(1, window, 1)) {
      r <- 1
      for (k in seq(1, polynom, 1)) {
        A[j, k] <- r
        tA[k, j] <- r
        r <- r * t[j]
      }
    }
    
    # Multiply the three matrices
    tAA <- tA %*% W %*% A
    
    # Invert the product of the matrices
    tAA <- solve(tAA)
    
    # Calculate the pseudoinverse of the design matrixmappl
    coeffs <- tAA %*% tA %*% W
    
    # Calculate c0 which is also the y value for y[i]
    y_smoothed[i] <- 0
    for (j in seq(1, window, 1)) {
      y_smoothed[i] <- y_smoothed[i] + coeffs[1, j] * y[i + j - 1 - half_window]
    }

    # If at the end or beginning, store all coefficients for the polynom
    if (i == half_window+1) {
      first_coeffs <- rep(0, polynom)
      for (j in seq(1, window, 1)) {
        for (k in seq(polynom)) {
          first_coeffs[k] <- first_coeffs[k] + coeffs[k, j] * y[j]
        }
      }
    } else if (i == length(x) - half_window - 1) {
      last_coeffs <- rep(0, polynom)
      for (j in seq(1, window, 1)) {
        for (k in seq(polynom)) {
          last_coeffs[k] <- last_coeffs[k] + coeffs[k, j] * y[length(y) - window + j]
        }
      } 
    }

  }
  
  # Interpolate the result at the left border
  for (i in seq(1, half_window, 1)) {
    y_smoothed[i] <- 0
    x_i <- 1
    for (j in seq(1, polynom, 1)) {
      y_smoothed[i] <- y_smoothed[i] + first_coeffs[j] * x_i
      x_i <- x_i * (x[i] - x[half_window+1])
    }
  }
  
  # Interpolate the result at the right border
  for (i in seq(length(x) - half_window + 1, length(x), 1)) {
    y_smoothed[i] <- 0
    x_i <- 1
    for (j in seq(1, polynom, 1)) {
      y_smoothed[i] <- y_smoothed[i] + last_coeffs[j] * x_i
      x_i <- x_i * (x[i] - x[length(x) - half_window])
    }
  }
  
  y_smoothed
  
}
