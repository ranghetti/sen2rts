#' @title Cut seasons
#' @description Cut Sentinel-2 time series into separate seasons,
#'  detecting dates of cuts and of peaks.
#' @param ts Time series in `s2ts` format (generated using `fill_s2ts()`).
#' @param min_win (optional) Minimum time window between two consecutive 
#'  maxima / minima to consider a separate season.
#' @param min_relh (optional) Minimum relative difference between the maximum
#'  and each of the two minima to consider a separate season.
#' @param max_relval (optional) Maximum relative value to consider a 
#'  season breakpoint.
#' @return The output time series in tabular format (see `extract_ts()`),
#'  with a new column `pheno` in which records detected as season breakpoints
#'  are marked with `"cut_seas"`, while season peaks with `"peak"`.
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @import data.table
#' @importFrom stats quantile
#' @importFrom methods as
#' @export

cut_seasons <- function(
  ts,
  min_win = 60,
  min_relh = 0.25,
  max_relval = 0.3
) {
  
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
  ts_dt <- as.data.table(ts)[order(id,date),]
  ts_dt[,uid := seq_len(nrow(ts_dt))]
  
  # Compute relative values
  ts_dt[,relval := (value - min(value, na.rm=TRUE)) / diff(range(value, na.rm=TRUE)), by = id]
  
  ## Retrieve local minima/maxima
  
  # Retrieve all maxima
  ts_dt[, peak0_l:= c(NA,diff(relval)) > 0 & c(diff(relval), NA) <= 0, by = id]
  ts_dt[, peak0_r:= c(NA,diff(relval)) >= 0 & c(diff(relval), NA) < 0, by = id]
  ts_dt[, peak0_p:= c(NA,diff(relval)) >= 0 & c(diff(relval), NA) <= 0, by = id]
  ts_dt[,peak0 := FALSE]
  for (peak_l_uid in ts_dt[peak0_l == TRUE, uid]) {
    peak_r_uid <- ts_dt[uid >= peak_l_uid & peak0_r == TRUE,][1,uid]
    if (ts_dt[seq(peak_l_uid,peak_r_uid), all(peak0_p)]) {
      ts_dt[uid == quantile(seq(peak_l_uid,peak_r_uid), 0.5, type = 1), peak0 := TRUE]
    }
  }
  ts_dt[,c("peak0_l", "peak0_r", "peak0_p") := NULL]
  ts_dt[,c("peak1", "cut1") := list(FALSE,FALSE)]
  
  # Filter maxima
  for (sel_id in unique(ts_dt$id)) {
    
    # Remove maxima with less than min_win days
    sel_ts_whichmax <- ts_dt[, which(id == sel_id & peak0)]
    for (i in sel_ts_whichmax[-1]) {
      ii <- which(sel_ts_whichmax == i)
      if (all(
        length(ii) > 0,
        ts_dt[sel_ts_whichmax[c(ii-1,ii)],diff(date)] < min_win
      )) {
        if (ts_dt[sel_ts_whichmax[ii-1],relval] > ts_dt[i,relval]) {
          ts_dt[i, peak0 := FALSE]
          sel_ts_whichmax <- sel_ts_whichmax[-ii]
        } else {
          ts_dt[sel_ts_whichmax[ii-1], peak0 := FALSE]
          sel_ts_whichmax <- sel_ts_whichmax[-(ii-1)]
        }
      }
    }
    
    # Remove maxima with less than min_h
    sel_ts_uidmax <- ts_dt[id == sel_id & peak0,][order(relval, decreasing = TRUE), uid]
    for (i in sel_ts_uidmax) {
      # compute ID of adjacent confirmed maxima
      suppressWarnings(uid_win <- c(
        ts_dt[id == sel_id & uid < i & peak1, max(uid)],
        ts_dt[id == sel_id & uid > i & peak1, min(uid)]
      ))
      # compute ID of minimum values within this window
      uid_mins <- c(
        ts_dt[id == sel_id & uid < i & uid > uid_win[1],][relval==min(relval), max(uid)],
        ts_dt[id == sel_id & uid > i & uid < uid_win[2],][relval==min(relval), min(uid)]
      )
      # check that the difference with all the minima is > min_relh
      # and that all the minima are <= max_relval
      if (all(
        ts_dt[i, relval] - ts_dt[uid_mins, relval] >= min_relh,
        ts_dt[uid_mins, relval] <= max_relval
      )) {
        ts_dt[uid == i, peak1 := TRUE]
        ts_dt[uid %in% uid_mins, cut1 := TRUE]
      }
    }
    
    # Remove "false seasons"
    for (i in ts_dt[id == sel_id & cut1 == TRUE, uid]) {
      j <- ts_dt[id == sel_id & uid > i & cut1 == TRUE,][1, uid]
      if (!is.na(j) && ts_dt[seq(i,j), !any(peak1)]) {
        ts_dt[uid %in% c(i,j), cut1 := FALSE]
        ts_dt[uid == quantile(seq(i,j), 0.5, type = 1), cut1 := TRUE]
      }
    }
  }
  
  ## Return output
  ts_dt[,pheno := as.character(NA)]
  ts_dt[peak1 == TRUE, pheno := "peak"]
  ts_dt[cut1 == TRUE, pheno := "cut_seas"]
  ts_dt[,c("uid", "relval", "peak0", "peak1", "cut1") := NULL]
  
  ts_out <- as(ts_dt, "s2ts")
  attr(ts_out, "gen_by") <- "cut_seasons"
  ts_out
  
}
