



find_peaks <- function(
  ts,
  min_win = 15, # Minimum time window between two consecutive maxima / minima
  min_relh = 0.5, # Minimum relative difference between the maximum and each of the two minima
  min_mineffect = 0.15, # Minimum effect of minima (according to #REF)
  min_ymax = -Inf,
  max_ymin = Inf
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
  # ts_dt$max0 <- logical()
  
  ## Retrieve local minima/maxima
  
  # Retrieve all maxima
  ts_dt[, max0:= c(NA,diff(relval)) > 0 & c(diff(relval), NA) < 0 & value >= min_ymax, by = id]
  ts_dt[, min0:= c(NA,diff(relval)) < 0 & c(diff(relval), NA) > 0, by = id]
  ts_dt[,c("max1", "min1") := list(FALSE,FALSE)]
  
#   # Remove values under/over max_ymin/min_ymax
#   for (sel_id in unique(ts_dt$id)) {
#     for (i in ts_dt[id == sel_id & max0 & value <= min_ymax, uid]) {
#       suppressWarnings(uid_win <- c( # compute ID of adjacent confirmed minima
#         ts_dt[id == sel_id & uid < i & min0, max(uid)],
#         ts_dt[id == sel_id & uid > i & min0, min(uid)]
#       ))
# browser()
#       ts_dt[uid == ts_dt[uid %in% uid_win,][relval == max(relval, na.rm=TRUE), uid], min0 := FALSE]
#     }
#     for (i in ts_dt[id == sel_id & min0 & value >= max_ymin, uid]) {
#       suppressWarnings(uid_win <- c( # compute ID of adjacent confirmed maxima
#         ts_dt[id == sel_id & uid < i & max0, max(uid)],
#         ts_dt[id == sel_id & uid > i & max0, min(uid)]
#       ))
#       ts_dt[uid == ts_dt[uid %in% uid_win,][relval == min(relval, na.rm=TRUE), uid], max0 := FALSE]
#     }
#   }
  
  # ## Compute intensity of peaks
  # # TODO find a faster method
  # for (sel_id in unique(ts_dt$id)) {
  #   sel_ts_whichmax <- ts_dt[,which(id == sel_id & max0)]
  #   sel_ts_whichmin <- ts_dt[,which(id == sel_id & min0)]
  #   for (i in sel_ts_whichmax) {
  #     which_min_1 <- sel_ts_whichmin[sum(sel_ts_whichmin < i)]
  #     which_min_2 <- sel_ts_whichmin[which(sel_ts_whichmin > i)[1]]
  #     if (length(c(which_min_1,which_min_2)) == 2) {
  #       ts_dt[
  #         i, max0_h := ts_dt[i, relval] - ts_dt[which_min_1, relval] - 
  #           as.numeric(ts_dt[i, date] - ts_dt[which_min_1, date]) /
  #           as.numeric(ts_dt[which_min_2, date] - ts_dt[which_min_1, date]) *
  #           (ts_dt[which_min_2, relval] - ts_dt[which_min_1, relval])
  #         ]
  #     } else {
  #       ts_dt[
  #         i, max0_h := ts_dt[i, relval] - 
  #           ts_dt[c(which_min_1,which_min_2), relval]
  #         ]
  #     }
  #   }
  # }
  # 
  
  # Filter maxima
  for (sel_id in unique(ts_dt$id)) {
    
    # Remove maxima with less than min_win days
    sel_ts_whichmax <- ts_dt[, which(id == sel_id & max0)]
    for (i in sel_ts_whichmax[-1]) {
      ii <- which(sel_ts_whichmax == i)
      if (all(
        length(ii) > 0,
        ts_dt[sel_ts_whichmax[c(ii-1,ii)],diff(date)] < min_win#,
        # !anyNA(c(ts_dt[sel_ts_whichmax[ii-1],max0_h], ts_dt[i,max0_h]))
      )) {
        # if (ts_dt[sel_ts_whichmax[ii-1],max0_h] > ts_dt[i,max0_h]) {
        # ts_dt[seq(sel_ts_whichmax[ii-1],i), min0 := FALSE]
        if (ts_dt[sel_ts_whichmax[ii-1],relval] > ts_dt[i,relval]) {
          ts_dt[i, max0 := FALSE]
          sel_ts_whichmax <- sel_ts_whichmax[-ii]
        } else {
          ts_dt[sel_ts_whichmax[ii-1], max0 := FALSE]
          sel_ts_whichmax <- sel_ts_whichmax[-(ii-1)]
        }
      }
    }

    # Remove maxima with less than min_h
    sel_ts_uidmax <- ts_dt[id == sel_id & max0,][order(relval, decreasing = TRUE), uid]
    for (i in sel_ts_uidmax) {
      # compute ID of adjacent confirmed maxima
      suppressWarnings(uid_win <- c(
        ts_dt[id == sel_id & uid < i & max1, max(uid)],
        ts_dt[id == sel_id & uid > i & max1, min(uid)]
      ))
      # compute ID of minimum values within this window
      uid_mins <- c(
        ts_dt[id == sel_id & uid < i & uid > uid_win[1],][relval==min(relval), uid],
        ts_dt[id == sel_id & uid > i & uid < uid_win[2],][relval==min(relval), uid]
      )
      # # commpute the base level of relval in correspondence of the maximum date
      # relval_basemax <- ts_dt[uid == uid_mins[1], relval] +
      #   diff(ts_dt[match(c(uid_mins[1],i), uid), as.numeric(date)]) /
      #   diff(ts_dt[match(uid_mins, uid), as.numeric(date)]) *
      #   diff(ts_dt[match(uid_mins, uid), relval])
      # check that the difference with all the minima is > min_relh
      if (all(ts_dt[i, relval] - ts_dt[uid_mins, relval] >= min_relh)) {
      # if (ts_dt[uid == i, relval] - relval_basemax >= min_relh) {
        ts_dt[uid == i, max1 := TRUE]
        ts_dt[uid %in% uid_mins, min1 := TRUE]
      }
    }

#     # Remove minima with unrelevant effect
#     sel_ts_uidmin <- ts_dt[id == sel_id & min0,][order(relval), uid]
#     ts_dt[,c("max2", "min2") := list(max0,min0)]
#     for (i in sel_ts_uidmin) {
#       # compute ID of adjacent confirmed maxima
#       suppressWarnings(uid_win <- c(
#         ts_dt[id == sel_id & uid < i & max2, max(uid)],
#         ts_dt[id == sel_id & uid > i & max2, min(uid)]
#       ))
#       # compute ratio
#       area_A <- (sum(ts_dt[match(uid_win, uid), relval])) * diff(ts_dt[match(uid_win, uid), as.numeric(date)]) / 2
#       area_B <- (sum(ts_dt[match(uid_win, uid), relval]) - 2 * ts_dt[uid == i, relval]) *
#         diff(ts_dt[match(uid_win, uid), as.numeric(date)]) / 2 -
#         diff(ts_dt[match(c(i,uid_win[1]),uid), relval]) * diff(ts_dt[match(c(uid_win[1],i),uid),as.numeric(date)]) / 2 -
#         diff(ts_dt[match(c(i,uid_win[2]),uid), relval]) * diff(ts_dt[match(c(i,uid_win[2]),uid),as.numeric(date)]) / 2
#       
# if (ts_dt[i,date] == as.Date("2019-08-08")) {browser()}
#       # check if the ratio is > min_mineffect
#       if (area_B / area_A < min_mineffect) {
#         ts_dt[uid == i, min2 := FALSE]
#         ts_dt[uid == ts_dt[uid %in% uid_win,][relval == min(relval), uid], max2 := FALSE]
#       }
#       
#     }

  }

browser()
  
  
  
  # seas_list <- list()
  # for (sel_id in unique(ts_dt$id)) { # cycle on IDs
  #   
  #   ## Retrieve local minima/maxima
  #   
  #   # Retrieve all minima/maxima
  #   sel_id_nrow <- ts_dt[,which(id == sel_id)]
  #   ts_dt[sel_id_nrow, max0:= c(NA,diff(ts_dt[sel_id_nrow,relval])) > 0 & c(diff(ts_dt[sel_id_nrow,relval]), NA) < 0]
  #   ts_dt[sel_id_nrow, min0:= c(NA,diff(ts_dt[sel_id_nrow,relval])) < 0 & c(diff(ts_dt[sel_id_nrow,relval]), NA) > 0]
  # 
  #   # Remove maxima with less than 15 days
  #   for (i in ts_dt[sel_id_nrow,][,which(max0 == TRUE)])
  #   ts_dt[sel_id_nrow,][max0 == TRUE,diff(date)] 
  #   
  #   
  #   
  #   ## Filter maxima
  #   
  #   
  #   
  # } # end of id FOR cycle
  
  
  ## Return output
  
  
}