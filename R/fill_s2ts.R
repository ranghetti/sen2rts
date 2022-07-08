#' @title Fill not equally-spaced time series
#' @description Fill temporal gaps in a time series smoothed with function
#'  [`smooth_s2ts()`] to obtain homogeneous values (daily or with a regular 
#'  time step).
#' @param ts Time series in `s2ts` format (generated using `smooth_s2ts()`).
#' @param frequency (optional) One of the followings:
#'  - `daily`: daily frequency (default);
#'  - `dop` (Days Of Passage): values are returned corresponding to 
#'      the theoretic Sentinel-2 dates of passage.
#' @param method (optional) Argument passed to `spline()` or `approx()`.
#' @param max_na_days (optional) maximum number of consecutive days with missing
#'  values which can be filled (in case of longer time windows with missing data,
#'  NA are returned).
#'  Default is to fit everything (unless this could lead to errors in case of
#'  long NT time windows, currently it is the only way to get subsequent functions 
#'  working).
#' @param max_extrapolation (optional) Numeric: maximum allowed extrapolation
#'  out of original range (relative value).
#'  Default is 0.1 (+10%). Set to Inf in order not to set any constraint.
#' @return The output time series in tabular format (see `extract_ts()`).
#' @importFrom stats spline approx
#' @importFrom methods as
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @export
#' @examples
#' #' # Load input data
#' data("ts_smoothed")
#' 
#' # Gap filling using default parameters (daily)
#' ts_filled <- fill_s2ts(ts_smoothed)
#' ts_filled # standard print
#' head(as.data.frame(ts_filled)) # see content
#' plot(ts_filled)
#' 
#' # Generate a regular time series using the minimum number of required records
#' ts_filled_2 <- fill_s2ts(ts_smoothed, frequency = "dop")
#' print(ts_filled_2, topn = 5) # standard print


fill_s2ts <- function(
  ts,
  frequency = "daily",
  method = "fmm",
  max_na_days = Inf,
  max_extrapolation = 0.1
) {
  
  # Avoid check notes for data.table related variables
  id <- orbit <- sensor <- interpolated <- value <- NULL

  ## Define Greater Common Divisor
  gcd <- function(x) {
    if (length(x) == 2) {
      ifelse(x[2]==0, x[1], gcd(x[2], x[1] %% x[2]))
    } else {
      gcd(c(x[1], gcd(x[2:length(x)])))
    }
  }
  
  ## Check arguments
  # TODO
  
  ## Check s2ts format
  # (must contain date, id, orbit, sensor, value, opt. quality)
  if (!inherits(ts, "s2ts")) {
    print_message(
      type = "error",
      "Argument 'ts' is not in the right format."
    )
  }
  # TODO
  ts_dt <- as.data.table(ts)
  
  ts_list <- list()
  for (sel_id in unique(ts_dt$id)) { # cycle on IDs
    
    # Create filled datatable
    ts_dop <- sen2r::s2_dop(
      s2_orbits = ts_dt[id == sel_id, unique(orbit)], 
      timewindow = ts_dt[id == sel_id, range(date)],
      mission = ts_dt[id == sel_id, unique(sensor)]
    )
    if (nrow(ts_dop) > 0) {
    
    # Define output dates
    ts_dt_out0 <- data.table(
      "date" = if (frequency == "dop") {
        ts_dop$date
      } else if (frequency == "daily") {
        seq(min(ts_dop$date), max(ts_dop$date), by = 1)
      } else if (frequency == "gcd") {
        diffdate <- gcd(as.integer(diff(ts_dop$date)))
        seq(min(ts_dop$date), max(ts_dop$date), by = diffdate)
      }
    )
    # ts_dt_out1 <- merge(ts_dt_out0, ts_dop, by = "date", all = TRUE)
    
    # ts_dt_out2 <- merge(
    #   ts_dt[id == sel_id,], ts_dt_out1, 
    #   by.x = c("date", "sensor", "orbit"), by.y = c("date", "mission", "orbit"), 
    #   all = TRUE
    # )
    ts_dt_out2 <- merge(
      ts_dt[id == sel_id,], ts_dt_out0, 
      by = "date", 
      all = TRUE
    )
    ts_dt_out2[,interpolated := is.na(value)]
    ts_dt_out2[,id := sel_id]
    
    # Interpolate (without extrapolating)
    valid_dates <- ts_dt[id == sel_id,][!is.na(value), date]
    valid_yrange <- ts_dt[id == sel_id, range(value, na.rm=TRUE)]
    valid_xrange <- as.Date(character(0))
    for (i in seq(length(valid_dates)-1)) {
      if (diff(valid_dates[i:(i+1)]) <= max_na_days) {
        valid_xrange <- c(valid_xrange, seq(valid_dates[i], valid_dates[i+1], 1))
      }
    }
    valid_xrange <- unique(valid_xrange)
    fill_fun <- if (method %in% c("linear", "constant")) {approx} else {spline}
    sel_spline <- do.call(
      fill_fun, 
      list(
        ts_dt[id == sel_id & date %in% valid_xrange, date],
        ts_dt[id == sel_id & date %in% valid_xrange, value],
        xout = ts_dt_out2[date %in% valid_xrange, date],
        method = method
      )
    )
    # sel_spline$x <- as.Date(sel_spline$x, origin = "1970-01-01")
    ts_dt_out2[date %in% valid_xrange, value := sel_spline$y]
    
    # Coerce to original min/max ranges
    if (max_extrapolation < Inf) {
      ts_dt_out2[,value := sapply(value, max, valid_yrange[1] - diff(valid_yrange) * max_extrapolation), by = id]
      ts_dt_out2[,value := sapply(value, min, valid_yrange[2] + diff(valid_yrange) * max_extrapolation), by = id]
    }
    
    ts_list[[sel_id]] <- ts_dt_out2

    } # enf of length > 0 IF cycle
  } # end of id FOR cycle
  
  ts_out <- as(rbindlist(ts_list), "s2ts")
  attr(ts_out, "gen_by") <- "fill_s2ts"
  ts_out
  
  
}