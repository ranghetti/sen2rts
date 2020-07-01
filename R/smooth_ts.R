#' @title Filter and smooth time series from sen2r archives
#' @description TODO
#' @param ts Time series in `s2ts` format (generated using `extract_s2ts()`).
#' @param min_qa (optional) minimum 0-1 quality value
#'  (points with `qa < min_qa` are not used, while `qa` values
#'  in the range `min_qa` to 1 are reshaped ito the range 0 to 1).
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
#' @param keep_max (optional) Logical: if TRUE, the maximum (if `noise_dir = "low"`)
#'  or minimum (if `noise_dir = "high"`) value between the input and the smoothed
#'  is finally keeped; if FALSE (default) smoothed values are always keeped.
#'  This argument is not used if `noise_dir = "undefined"`.
#' @return The output time series in `s2ts` format.
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @export

smooth_s2ts <- function(
  ts,
  min_qa = 0.5,
  noise_dir = "undefined",
  spike = 0.25,
  spike_window = 3,
  sg_window = 9,
  sg_polynom = 3,
  keep_max = FALSE
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
  ts_dt <- as.data.table(ts)
  ts_dt <- ts_dt[order(id, date),]

  ## Build relative TS
  ts_dt[,relval := (value - min(value)) / diff(range(value))]
  
  ## Exclude low-quality values and reshape others
  if (!is.null(ts_dt$qa)) {
    ts_dt <- ts_dt[qa > min_qa,]
    ts_dt <- ts_dt[,qa2 := (qa - min_qa) / (1 - min_qa)]
  } else {
    ts_dt$qa2 <- 1
  }
  
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
  
  # Compute Savitzky-Golay
  ts_dt$value_sg <- numeric()
  for (sel_id in unique(ts_dt$id)) { # cycle on IDs
    ts_dt[id == sel_id, value_sg := w_savgol(
      value, 
      x = as.numeric(date), 
      q = qa2, 
      window = sg_window, 
      polynom = sg_polynom
    )]
  } # end of id FOR cycle
  
  ## Take the maximum between SG and local
  ts_dt[,value_sm := ifelse(keep_max == TRUE & value > value_sg, value, value_sg)]
  
  ## Return output
  ts_dt$value <- ts_dt$value_sm
  ts_dt$value_sg <- ts_dt$relval <- ts_dt$qa2 <- ts_dt$value_sm <- NULL
  ts_out <- as(ts_dt, "s2ts")
  attr(ts_out, "gen_by") <- "smooth_s2ts"
  ts_out
  
}
