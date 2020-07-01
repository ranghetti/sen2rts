#' @title Fill not equally-spaced time series
#' @description TODO
#' @param ts Time series in `s2ts` format (generated using `smooth_s2ts()`).
#' @param frequency (optional) One of the followings:
#'  - `dop` (Days Of Passage -- default): values are returned corresponding to 
#'      the theoretic Sentinel-2 dates of passage;
#'  - `daily`: daily frequency.
#' @return The output time series in tabular format (see `extract_ts()`).
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @export

fill_s2ts <- function(
  ts,
  frequency = "dop"
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
    ts_dt_out2[,interpolated := is.na(id)]
    ts_dt_out2[interpolated == TRUE, id := sel_id]
    
    # Interpolate
    sel_spline <- spline(
      ts_dt[id == sel_id, date],
      ts_dt[id == sel_id, value],
      xout = ts_dt_out2$date
    )
    ts_dt_out2$value <- sel_spline$y
    
    ts_list[[sel_id]] <- ts_dt_out2
    
  } # end of id FOR cycle
  
  ts_out <- as(rbindlist(ts_list), "s2ts")
  attr(ts_out, "gen_by") <- "fill_s2ts"
  ts_out
  
  
}