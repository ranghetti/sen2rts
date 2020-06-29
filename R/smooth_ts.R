#' @title Filter and smooth time series from sen2r archives
#' @description TODO
#' @param s2ts Time series generated using `extract_s2ts()`.
#' @param min_q (optional) minimum 0-1 quality value
#'  (points with `quality < min_q` are not used, while `quality` values
#'  in the range `min_q` to 1 are reshaped ito the range 0 to 1).
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
#' @return The output time series in tabular format (see `extract_ts()`).
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @export

smooth_s2ts <- function(
  s2ts,
  min_q = 0.5,
  noise_dir = "undefined",
  spike = 0.25,
  spike_window = 3,
  sg_window = 9,
  sg_polynom = 3,
  keep_max = FALSE
) {
  
  ## Check arguments
  # TODO

  ## Check s2ts format
  # (must contain date, id, orbit, sensor, value, opt. quality)
  # TODO
  s2ts_in <- s2ts
  
  # define quality column if missing
  if (is.null(s2ts$quality)) {s2ts$quality <- 1}
  s2ts <- s2ts[order(id,date),]

  ## Build relative TS
  s2ts[,relval := (value - min(value)) / diff(range(value))]
  
  ## Exclude low-quality values and reshape others
  s2ts <- s2ts[quality > min_q,]
  s2ts <- s2ts[,quality := (quality - min_q) / (1 - min_q)]
  
  ## Remove spikes
  s2ts$spike <- FALSE #initialisation
  for (sel_id in unique(s2ts$id)) { # cycle on IDs
    ## Convert inputs
    shw <- trunc(spike_window/2) # spike half window
    sel_id_rows <- s2ts[,which(id == sel_id)]
    for (j in seq(sel_id_rows[1]+shw, sel_id_rows[length(sel_id_rows)]-shw)) {
      val <- s2ts[seq(j-shw, j+shw), relval]
      if (all(
        noise_dir %in% c("undefined", "high"),
        any(val[shw+1] - val[seq(1,shw)] > spike),
        any(val[shw+1] - val[seq(shw+2,2*shw+1)] > spike)
      )) {
        s2ts[j, spike := TRUE]
      }
      if (all(
        noise_dir %in% c("undefined", "low"),
        any(val[seq(1,shw)] - val[shw+1] > spike),
        any(val[seq(shw+2,2*shw+1)] - val[shw+1] > spike)
      )) {
        s2ts[j, spike := TRUE]
      }
    }
  } # end of id FOR cycle
  s2ts <- s2ts[spike == FALSE,]
  s2ts$spike <- NULL
  
  # Compute Savitzky-Golay
  s2ts$value_sg <- numeric()
  for (sel_id in unique(s2ts$id)) { # cycle on IDs
    s2ts[id == sel_id, value_sg := w_savgol(
      value, 
      x = as.numeric(date), 
      q = quality, 
      window = sg_window, 
      polynom = sg_polynom
    )]
  } # end of id FOR cycle
  
  ## Take the maximum between SG and local
  s2ts[,value_sm := ifelse(keep_max == TRUE & value > value_sg, value, value_sg)]
  s2ts$value_sg <- NULL
  s2ts$relval <- NULL
  
  
  ## Return output
  s2ts$value <- s2ts$value_sm
  s2ts$value_sm <- NULL
  # s2ts[,list(date,id,value=value_out,quality)]
  # s2ts[match(s2ts_in[,paste(date,id)], s2ts[,paste(date,id)]), value_sg]
  s2ts
  
}
