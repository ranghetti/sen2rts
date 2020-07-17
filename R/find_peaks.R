



find_peaks <- function(
  ts,
  min_win = 60, # Minimum time window between two consecutive maxima / minima
  min_relh = 0.25 # Minimum relative difference between the maximum and each of the two minima
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
  ts_dt <- as.data.table(ts)
  ts_dt[,uid := seq_len(nrow(ts_dt))]
  
  # Compute reltive values
  ts_dt[,relval := (value - min(value, na.rm=TRUE)) / diff(range(value, na.rm=TRUE)), by = id]

  ## Retrieve local minima/maxima
  
  # Retrieve all maxima
  ts_dt[, peak0:= c(NA,diff(relval)) > 0 & c(diff(relval), NA) < 0, by = id]
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
        ts_dt[id == sel_id & uid < i & uid > uid_win[1],][relval==min(relval), uid],
        ts_dt[id == sel_id & uid > i & uid < uid_win[2],][relval==min(relval), uid]
      )
      # check that the difference with all the minima is > min_relh
      if (all(ts_dt[i, relval] - ts_dt[uid_mins, relval] >= min_relh)) {
        ts_dt[uid == i, peak1 := TRUE]
        ts_dt[uid %in% uid_mins, cut1 := TRUE]
      }
    }

  }

  ## Return output
  ts_dt[,pheno := as.character(NA)]
  ts_dt[peak1 == TRUE, pheno := "peak"]
  ts_dt[cut1 == TRUE, pheno := "cut_seas"]
  ts_dt[,c("uid", "relval", "peak0", "peak1", "cut1") := NULL]
  
  ts_out <- as(ts_dt, "s2ts")
  attr(ts_out, "gen_by") <- "find_peaks"
  ts_out
  
}