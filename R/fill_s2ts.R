#' @title Fill not equally-spaced time series
#' @description TODO
#' @param s2ts Time series generated using `smooth_s2ts()`.
#' @param frequency (optional) One of the followings:
#'  - `dop` (Days Of Passage -- default): values are returned corresponding to 
#'      the theoretic Sentinel-2 dates of passage;
#'  - `daily`: daily frequency.
#' @return The output time series in tabular format (see `extract_ts()`).
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @export

fill_s2ts <- function(
  s2ts,
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
  # TODO
  
  s2ts_out_list <- list()
  for (sel_id in unique(s2ts$id)) { # cycle on IDs
    
    # Create filled datatable
    s2ts_dop <- sen2r::s2_dop(
      s2_orbits = s2ts[id == sel_id, unique(orbit)], 
      timewindow = s2ts[id == sel_id, range(date)],
      mission = s2ts[id == sel_id, unique(sensor)]
    )
    
    # Define output dates
    s2ts_out0 <- data.table(
      "date" = if (frequency == "dop") {
        s2ts_dop$date
      } else if (frequency == "daily") {
        seq(min(s2ts_dop$date), max(s2ts_dop$date), by = 1)
      } else if (frequency == "gcd") {
        diffdate <- gcd(as.integer(diff(s2ts_dop$date)))
        seq(min(s2ts_dop$date), max(s2ts_dop$date), by = diffdate)
      }
    )
    s2ts_out1 <- merge(s2ts_out0, s2ts_dop, by = "date", all = TRUE)
    
    
    s2ts_out2 <- merge(
      s2ts[id == sel_id,], s2ts_out1, 
      by.x = c("date", "sensor", "orbit"), by.y = c("date", "mission", "orbit"), 
      all = TRUE
    )
    s2ts_out2[,interpolated := is.na(id)]
    s2ts_out2[interpolated == TRUE, id := sel_id]
    
    # Interpolate
    sel_spline <- spline(
      s2ts[id == sel_id, date],
      s2ts[id == sel_id, value],
      xout = s2ts_out2$date
    )
    s2ts_out2$value <- sel_spline$y
    
    s2ts_out_list[[sel_id]] <- s2ts_out2
    
  } # end of id FOR cycle
  s2ts_out <- rbindlist(s2ts_out_list)
  
}