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
#'  Set to NA to skip spike removal.
#' @param spike_window Maximum number of values for spike identification
#'  (it must be an odd number). Default is 3.
#' @param sg_daywindow Half-size of the time window to be used to interpolate 
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
#' @importFrom stats approx
#' @importFrom methods as
#' @export
#' @examples
#' # Load input data
#' data("ts_raw")
#' 
#' # Smoothing time series using default parameters
#' ts_smoothed <- smooth_s2ts(ts_raw)
#' print(ts_smoothed, topn = 5) # standard print
#' head(as.data.frame(ts_smoothed)) # see content
#' plot(ts_smoothed)
#' 
#' # Apply a more pronounced smoothing
#' ts_smoothed_2 <- smooth_s2ts(
#'   ts_raw,
#'   min_qa = 0.5, # exclude values with qa < 0.5
#'   sg_daywindow = 30, # larger moving window
#'   sg_polynom = 3, # cubic interpolation
#'   sg_n = 5 # apply the SG filter 5 times
#' )
#' plot(ts_smoothed_2)


smooth_s2ts <- function(
  ts,
  min_qa = 0.2,
  noise_dir = "low",
  spike = 0.25,
  spike_window = 5,
  sg_daywindow = 15,
  sg_polynom = 2,
  sg_n = 3,
  max_extrapolation = 0.1
) {
  
  # Avoid check notes for data.table related variables
  id <- relval <- value0 <- qa0 <- orbit <- sensor <- value_smoothed <- NULL
  
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
    sg_n < 1#,
    # sg_n > 1 & !noise_dir %in% c("low", "high")
  )) {
    sg_n <- 1
  }
  
  ## Build relative TS
  ts_dt[,relval := (value - min(value, na.rm=TRUE)) / diff(range(value, na.rm=TRUE)), by = id]
  
  ## Exclude low-quality values and reshape others
  ts_dt <- ts_dt[,value0 := value]
  if (!is.null(ts_dt$qa)) {
    ts_dt <- ts_dt[qa > min_qa,]
    ts_dt <- ts_dt[,qa0 := qa]
  } else {
    ts_dt <- ts_dt[,qa0 := 1]
  }
  # if (!is.null(ts_dt$qa)) {
  #   # Recompute low-quality values
  #   ts_dt$value0 <- ts_dt$value
  #   for (sel_id in unique(ts_dt$id)) { # cycle on IDs
  #     valid_range <- range(ts_dt[id == sel_id & qa > min_qa, date])
  #     ts_dt_interp <- approx(
  #       ts_dt[id == sel_id & qa > min_qa, date],
  #       ts_dt[id == sel_id & qa > min_qa, value],
  #       xout = ts_dt[id == sel_id & date >= valid_range[1] & date <= valid_range[2], date]
  #     )
  #     ts_dt[id == sel_id & date >= valid_range[1] & date <= valid_range[2],
  #           value0 := ts_dt_interp$y]
  #   }
  #   ts_dt <- ts_dt[,qa0 := qa]
  # } else {
  #   ts_dt <- ts_dt[,value0 := value]
  #   ts_dt <- ts_dt[,qa0 := 1]
  # }
  
  ## Remove spikes
  if (!is.na(spike)) {
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
  }
  
  # Reshape data to use a day-dependent window
  # (this step is required to pass a "more or less time weighted")
  if (length(unique(ts_dt$orbit)) > 5) {
    print_message(
      type = "warning",
      "Using more than 5 orbits may deal to unattended results."
    )
  }
  ts_dt_teor <- rbindlist(
    sapply(unique(ts_dt$id), function(i) {
      s2_dop_simpl(
        s2_orbits = unique(ts_dt[id==i,]$orbit),
        timewindow = range(ts_dt[id==i,]$date),
        mission = unique(ts_dt[id==i,]$sensor)
      )
    }, USE.NAMES = TRUE, simplify = FALSE),
    idcol = "id"
  )
  setnames(ts_dt_teor, "mission", "sensor")
  ts_dt_filled <- merge(
    ts_dt,
    ts_dt_teor,
    by = c("id", "date", "sensor", "orbit"),
    all = TRUE
  )
  # Recompute low-quality and missing values
  for (sel_id in unique(ts_dt_filled$id)) { # cycle on IDs
    valid_range <- range(ts_dt_filled[id == sel_id & !is.na(value), date])
    ts_dt_interp <- approx(
      ts_dt_filled[id == sel_id & !is.na(value), date],
      ts_dt_filled[id == sel_id & !is.na(value), value],
      xout = ts_dt_filled[id == sel_id & date >= valid_range[1] & date <= valid_range[2], date]
    )
    ts_dt_filled[id == sel_id & date >= valid_range[1] & date <= valid_range[2],
                 value0 := ts_dt_interp$y]
  }
  # ts_dt_filled[is.na(value), c("value0","qa0"):=list(0,0)]
  ts_dt_filled <- ts_dt_filled[is.na(value), "qa0" := 1e-2]
  
  # Compute Savitzky-Golay
  ts_dt_filled$value_smoothed <- numeric()
  for (sel_id in unique(ts_dt_filled$id)) { # cycle on IDs
    sg_window <- ceiling(
      sg_daywindow / 10 * 
        ts_dt_filled[id == sel_id, length(unique(orbit))*length(unique(sensor))]
    ) * 2 + 1
    qa <- ts_dt_filled[id == sel_id, qa0]
    value <- value_sg <- ts_dt_filled[id == sel_id, value0]
    for (i in seq_len(sg_n)) {
      qa <- (rank(value-value_sg) - 1) / (ts_dt_filled[,sum(id == sel_id)] - 1) * qa
      value_sg <- w_savgol(
        value, 
        x = ts_dt_filled[id == sel_id, as.numeric(date)], 
        q = qa, 
        window = sg_window, 
        polynom = sg_polynom
      )
    }
    ts_dt_filled[id == sel_id, value_smoothed := value_sg]
  } # end of id FOR cycle
  ts_dt <- ts_dt_filled[
    ts_dt_filled[,paste(id, date, sensor, orbit)] %in% 
      ts_dt[,paste(id, date, sensor, orbit)],
  ]
  
  
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
