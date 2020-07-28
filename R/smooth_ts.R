#' @title Filter and smooth time series from sen2r archives
#' @description TODO
#' @param ts Time series in `s2ts` format (generated using `extract_s2ts()`).
#' @param min_qa (optional) minimum 0-1 quality value
#'  (points with `qa < min_qa` are not used, while `qa` values
#'  in the range `min_qa` to 1 are reshaped into the range 0 to 1).
#'  Default is 0.5.
#' @param noise_dir Direction of points generally containing noise.
#'  If `"low"`, higher values are generally maintained (this is the case of
#'  most vegetation indices like NDVI);
#'  if `"high"`, lower values are generally maintained;
#'  if `"undefined"` (default), no assumptions are done.
#' @param spike Relative "y" difference for spike determination (default is 0.25).
#' @param spike_window Maximum number of values for spike identification
#'  (it must be an odd number). Default is 3.
#' @param sg_window Argument `window` of function `w_savgol()`.
#' @param sg_polynom Argument `polynom` of function `w_savgol()`.
#' @param sg_n (optional) Positive integer: number of applications of the 
#'  Savitzky-Golay filter. The minimum value is 1 (a single application
#'  using the weights included in `ts`).
#'  Within each additional application, the weights included in `ts` are
#'  multiplicated for the relative ranks of the difference between original
#'  and smoothed values (if `noise_dir = "low"`) or between smoothed and 
#'  original values (if `noise_dir = "high"`).
#'  If `noise_dir = "undefined"`, this argument is coerced to 1.
#' @param max_extrapolation (optional) Numeric: maximum allowed extrapolation
#'  out of original range (relative value).
#'  Default is 0.1 (+10%). Set to Inf in order not to set any constraint.
#' @return The output time series in `s2ts` format.
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @export

smooth_s2ts <- function(
  ts,
  min_qa = 0.5,
  noise_dir = "low",
  spike = 0.25,
  spike_window = 5,
  sg_window = 9,
  sg_polynom = 2,
  sg_n = 3,
  max_extrapolation = 0.1
) {
  
  ## Check arguments
  # TODO
  
  ## Check ts format
  # (must contain date, id, orbit, sensor, value, opt. qa)
  if (!inherits(ts, "s2ts")) {
    print_message(
      type = "error",
      "Argument 'ts' is not in the right format."
    )
  }
  # TODO
  ts_dt <- ts_dt_full <- as.data.table(ts)[order(id, date),]
  
  # Check sg_n
  if (any(
    sg_n < 1,
    sg_n > 1 & !noise_dir %in% c("low", "high")
  )) {
    sg_n <- 1
  }
  
  ## Build relative TS
  ts_dt[,relval := (value - min(value, na.rm=TRUE)) / diff(range(value, na.rm=TRUE))]
  
  ## Remove spikes
  ts_dt$spike <- FALSE #initialisation
  for (sel_id in unique(ts_dt$id)) { # cycle on IDs
    ## Convert inputs
    shw <- trunc(spike_window/2) # spike half window
    sel_id_rows <- ts_dt[,which(id == sel_id)]
    for (j in seq(sel_id_rows[1]+shw, sel_id_rows[length(sel_id_rows)]-shw)) {
      val <- ts_dt[seq(j-shw, j+shw), relval]
      if (all(
        noise_dir %in% c("undefined", "high"),
        any(val[shw+1] - val[seq(1,shw)] > spike),
        any(val[shw+1] - val[seq(shw+2,2*shw+1)] > spike)
      )) {
        ts_dt[j, spike := TRUE]
      }
      if (all(
        noise_dir %in% c("undefined", "low"),
        any(val[seq(1,shw)] - val[shw+1] > spike),
        any(val[seq(shw+2,2*shw+1)] - val[shw+1] > spike)
      )) {
        ts_dt[j, spike := TRUE]
      }
    }
  } # end of id FOR cycle
  ts_dt <- ts_dt[spike == FALSE,]
  ts_dt$spike <- NULL
  
  ## Exclude low-quality values and reshape others
  if (!is.null(ts_dt$qa)) {
    
    # Recompute low-quality values
    ts_dt$value0 <- ts_dt$value
    for (sel_id in unique(ts_dt$id)) { # cycle on IDs
      valid_range <- range(ts_dt[id == sel_id & qa > min_qa, date])
      ts_dt_interp <- approx(
        ts_dt[id == sel_id & qa > min_qa, date],
        ts_dt[id == sel_id & qa > min_qa, value],
        xout = ts_dt[id == sel_id & date >= valid_range[1] & date <= valid_range[2], date]
      )
      ts_dt[id == sel_id & date >= valid_range[1] & date <= valid_range[2],
            value0 := ts_dt_interp$y]
    }
    ts_dt <- ts_dt[,qa0 := qa]
  } else {
    ts_dt <- ts_dt[,value0 := value]
    ts_dt <- ts_dt[,qa0 := 1]
  }
  
  # Compute Savitzky-Golay
  ts_dt$value_smoothed <- numeric()
  for (sel_id in unique(ts_dt$id)) { # cycle on IDs
    qa <- ts_dt[id == sel_id, qa0]
    value <- value_sg <- ts_dt[id == sel_id, value0]
    for (i in seq_len(sg_n)) {
      qa <- (rank(value-value_sg) - 1) / (ts_dt[,sum(id == sel_id)] - 1) * qa
      value_sg <- w_savgol(
        value, 
        x = ts_dt[id == sel_id, as.numeric(date)], 
        q = qa, 
        window = sg_window, 
        polynom = sg_polynom
      )
    }
    ts_dt[id == sel_id, value_smoothed := value_sg]
  } # end of id FOR cycle
  
  # Coerce values to original ranges
  if (max_extrapolation < Inf) {
    ts_dt[,value_smoothed := sapply(value_smoothed, max, min(value, na.rm = TRUE) - diff(range(value, na.rm = TRUE)) * max_extrapolation), by = id]
    ts_dt[,value_smoothed := sapply(value_smoothed, min, max(value, na.rm = TRUE) + diff(range(value, na.rm = TRUE)) * max_extrapolation), by = id]
  }
  
  # Restore non-smoothed values
  ts_dt <- merge(ts_dt, ts_dt_full, by = names(ts_dt_full), all = TRUE)
  
  ## Return output
  ts_dt$rawval <- ts_dt$value
  ts_dt$value <- ts_dt$value_smoothed
  ts_dt$value_smoothed <- ts_dt$relval <- ts_dt$qa0 <- ts_dt$value0 <- NULL
  ts_out <- as(ts_dt, "s2ts")
  attr(ts_out, "gen_by") <- "smooth_s2ts"
  ts_out
  
}
