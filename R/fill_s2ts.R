#' @title Fill not equally-spaced time series
#' @description TODO
#' @param ts Time series in `s2ts` format (generated using `smooth_s2ts()`).
#' @param frequency (optional) One of the followings:
#'  - `dop` (Days Of Passage -- default): values are returned corresponding to 
#'      the theoretic Sentinel-2 dates of passage;
#'  - `daily`: daily frequency.
#' @param method (optional) Argument passed to `spline()`.
#' @param max_na_days (optional) maximum number of consecutive days with missing
#'  values which can be filled (in case of longer time windows with missing data,
#'  NA are returned).
#' @return The output time series in tabular format (see `extract_ts()`).
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @export

fill_s2ts <- function(
  ts,
  frequency = "dop",
  method = "fmm",
  max_na_days = 30
) {
  
  ## Define Greater Common Divisor
  gcd <- function(x) {
    if (length(x) == 2) {
      ifelse(x[2]==0, x[1], gcd2(x[2], x[1] %% x[2]))
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
    ts_dt_out1 <- merge(ts_dt_out0, ts_dop, by = "date", all = TRUE)
    
    
    ts_dt_out2 <- merge(
      ts_dt[id == sel_id,], ts_dt_out1, 
      by.x = c("date", "sensor", "orbit"), by.y = c("date", "mission", "orbit"), 
      all = TRUE
    )
    ts_dt_out2[,interpolated := is.na(value)]
    ts_dt_out2[,id := sel_id]
    
    # Interpolate (without extrapolating)
    valid_dates <- ts_dt[id == sel_id,][!is.na(value), date]
    valid_range <- as.Date(character(0))
    for (i in seq(length(valid_dates)-1)) {
      if (diff(valid_dates[i:(i+1)]) <= max_na_days) {
        valid_range <- c(valid_range, seq(valid_dates[i], valid_dates[i+1], 1))
      }
    }
    valid_range <- unique(valid_range)
    sel_spline <- spline(
      ts_dt[id == sel_id & date %in% valid_range, date],
      ts_dt[id == sel_id & date %in% valid_range, value],
      xout = ts_dt_out2[date %in% valid_range, date],
      method = method
    )
    sel_spline$x <- as.Date(sel_spline$x, origin = "1970-01-01")
    ts_dt_out2[date %in% valid_range, value := sel_spline$y]
    
    ts_list[[sel_id]] <- ts_dt_out2
    
  } # end of id FOR cycle
  
  ts_out <- as(rbindlist(ts_list), "s2ts")
  attr(ts_out, "gen_by") <- "fill_s2ts"
  ts_out
  
  
}