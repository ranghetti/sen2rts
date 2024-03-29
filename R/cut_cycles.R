#' @title Cut cycles
#' @description Cut Sentinel-2 time series into separate cycles,
#'  detecting dates of cuts and peaks.
#' @param ts Time series in `s2ts` format (generated using `fill_s2ts()`).
#' @param n_cycles (optional) Maximum number of cycles to be detected in one year
#'  (default: Inf, meaning that all the identified cycles are kept).
#'  A cycle overlapping the new year's day (argument `newyearday`)
#'  is assigned to the year in which the date of maximum value falls.
#' @param min_win (optional) Minimum time window between two consecutive 
#'  maxima / minima to consider a separate cycle.
#' @param min_peakvalue (optional) Minimum value to consider a cycle peak.
#' @param max_dropvalue (optional) Maximum value to consider a cycle drop
#'  (breakpoint).
#' @param max_groundvalue (optional) Maximum value to identify a ground plain
#'  (window without cycles).
#' @param ground_buffer (optional) n. of days of beginning / ending of grounds 
#'  to be included in previous / next seasons.
#' @param value_type (optional) Character: if `"relative"` (default), values
#'  set with arguments `min_peakval` and `max_dropval` are relative
#'  values (normalised to 0-1 range among IDs);
#'  if `"absolute"`, absolute values are considered.
#' @param min_relh (optional) Numeric: minimum relative difference between the 
#'  maximum and each of the two minima to consider a separate cycle.
#'  Default is 0.15.
#' @param relevance (optional) Numeric: threshold used to consider
#'  local minima as relevant, according to Meroni et al. (2021)
#'  (see for reference). 
#'  This is an alternative criterion with respect to `min_relh`
#'  (nevertheless they can be used together, too).
#'  Default is 0, meaning that this criterion is not used by default.
#' @param newyearday (optional) day to be considered as new year's day, used
#'  to assign cycles to the proper year.
#'  It can be an object of class `Date` (in which case the year is ignored) 
#'  or a character value in the form `mm-dd`.
#'  In case it is July 1 or higher, cycles whose maximum value is falling in
#'  the last  part of the year are assigned to the subsequent year; 
#'  otherwise, cycles whose maximum value is falling in the first part of the
#'  year are assigned  to the previous year. Default is January 1
#'  (all cycles are assigned the the year in which their maximum value falls).
#' @param weight_metric (optional) Criterion used to assign a weight value
#'  to each seasons (used by subsequent functions:
#'  `"integral"` (default: sum of values among the cycle),
#'  `"length"` (length of the cycle) or 
#'  `"maxval"` (maximum value reached in the cycle).
#' @return A data table with the following fields:
#'  - `id`: the time series ID (see `s2ts`);
#'  - `year`: the year assigned to each cycle;
#'  - `cycle`: the cycle ID (progressive integer within each year);
#'  - `begin`: the date of the begin of the cycle;
#'  - `end`: the date of the end of the cycle;
#'  - `maxval`: the date of the maximum value of the cycle;
#'  - `weight`: the value of the metric used for ranking seasons.
#' @note The steps used to discriminate seasons are partially based on the method
#'  exposed in Meroni et al. (2021) (\doi{10.1016/j.rse.2020.112232}).
#'  The methodology will be documented in future.
#' @author Luigi Ranghetti, PhD (2021) \email{luigi@@ranghetti.info}
#' @import data.table
#' @importFrom stats quantile
#' @export
#' @examples 
#' # Load input data
#' data("ts_filled")
#' 
#' # Cut seasons with standard parameters
#' dt_cycles <- cut_cycles(ts_filled)
#' dt_cycles
#' # Plot the TS highlighting the extracted cycles
#' plot(ts_filled, pheno = dt_cycles, plot_dates = TRUE)
#' 
#' # Cut cycles considering separate cycles only if the maximum NDVI is > 0.7
#' dt_cycles_2 <- cut_cycles(
#'   ts_filled,
#'   min_win = 120, # exclude cycles shorter than 4 months
#'   min_peakvalue = 0.7, # exclude cycles with NDVI of peak < 0.7
#'   value_type = "absolute", # 0.7 is the absolute NDVI value, not relative
#'   newyearday = "10-01" # consider a year from 1st October to 30th September
#' )
#' plot(ts_filled, pheno = dt_cycles_2)


cut_cycles <- function(
  ts,
  n_cycles = Inf,
  min_win = 60,
  min_peakvalue = 0.1,
  max_dropvalue = 0.6,
  max_groundvalue = 0.2,
  ground_buffer = 10,
  value_type = "relative",
  min_relh = 0.15,
  relevance = 0,
  newyearday = "01-01",
  weight_metric = "integral"
) {
  
  # Avoid check notes for data.table related variables
  id <- uid <- relval <- value <-
    peak0_l <- peak0_r <- peak0_p <- peak0 <- 
    cut0_l <- cut0_r <- cut0_p <- cut0 <-
    ground <- cutground <- 
    peak1 <- cut1 <- peak2 <- cut2 <- peak3 <- cut3 <- 
    s1 <- y1 <- maxval <- maxyear <- weight <- 
    begin <- end <- cycle <- newyear <- 
    NULL
  
  ## Check arguments
  # TODO
  
  # Check newyearday
  if (inherits(newyearday, "character")) {
    if (grepl("^[0-1][0-9]?\\-[0-3][0-9]?$", newyearday)) {
      newyearday <- as.Date(paste0("2021-",newyearday))
    } else {
      newyearday <- as.Date(newyearday)
    }
  }
  
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
  
  # Compute relative values, if needed
  if (value_type == "relative") {
    ts_dt[,relval := (value - min(value, na.rm=TRUE)) / diff(range(value, na.rm=TRUE)), by = id]
  } else {
    ts_dt[,relval := value]
  }
  
  ## Retrieve local minima/maxima

  # Retrieve all maxima
  ids <- unique(ts_dt$id) # perform once
  ts_dt[, peak0_l:= c(NA,diff(relval)) > 0 & c(diff(relval), NA) <= 0, by = id]
  ts_dt[, peak0_r:= c(NA,diff(relval)) >= 0 & c(diff(relval), NA) < 0, by = id]
  ts_dt[, peak0_p:= c(NA,diff(relval)) >= 0 & c(diff(relval), NA) <= 0, by = id]
  ts_dt[,peak0 := FALSE]
  ts_dt[peak0_l == TRUE & peak0_r == TRUE, peak0 := TRUE]
  for (peak_l_uid in ts_dt[peak0_l == TRUE & peak0_r == FALSE, uid]) {
    peak_r_uid <- ts_dt[uid >= peak_l_uid & peak0_r == TRUE,][1,uid]
    if (ts_dt[seq(peak_l_uid,peak_r_uid), all(peak0_p)]) {
      ts_dt[uid == quantile(seq(peak_l_uid,peak_r_uid), 0.5, type = 1), peak0 := TRUE]
    }
  }
  ts_dt[,c("peak0_l", "peak0_r", "peak0_p") := NULL]
  
  # Retrieve all minima
  ts_dt[, cut0_l:= c(-Inf,diff(relval)) < 0 & c(diff(relval), Inf) >= 0, by = id]
  ts_dt[, cut0_r:= c(-Inf,diff(relval)) <= 0 & c(diff(relval), Inf) > 0, by = id]
  ts_dt[, cut0_p:= c(-Inf,diff(relval)) <= 0 & c(diff(relval), Inf) >= 0, by = id]
  ts_dt[,cut0 := FALSE]
  ts_dt[cut0_l == TRUE & cut0_r == TRUE, cut0 := TRUE]
  for (cut_l_uid in ts_dt[cut0_l == TRUE & cut0_r == FALSE, uid]) {
    cut_r_uid <- ts_dt[uid >= cut_l_uid & cut0_r == TRUE,][1,uid]
    if (ts_dt[seq(cut_l_uid,cut_r_uid), all(cut0_p)]) {
      ts_dt[uid == quantile(seq(cut_l_uid,cut_r_uid), 0.5, type = 1), cut0 := TRUE]
    }
  }
  ts_dt[,c("cut0_l", "cut0_r", "cut0_p") := NULL]
  ts_dt[,c("peak1", "cut1", "peak2", "cut2") := list(peak0, cut0, FALSE, FALSE)]
  
  # Identify baselines
  ts_dt[, ground := relval <= max_groundvalue]
  ts_dt[, cutground := c(NA, diff(ground)), by = id]
  ts_dt[is.na(cutground), cutground := 0]
  for (sel_id in unique(ts_dt$id)) {
    for (u in ts_dt[id == sel_id & cutground < 0, uid]) {
      date_r <- ts_dt[uid == u, date]
      date_l <- ts_dt[id == sel_id & date <= date_r - ground_buffer, max(date)]
      ts_dt[id == sel_id & date >= date_l & date < date_r, ground := FALSE]
      ts_dt[id == sel_id & date == date_l, cutground := -1]
      ts_dt[id == sel_id & date == date_r, cutground := 0]
    }
    for (u in ts_dt[id == sel_id & cutground > 0, uid]) {
      date_l <- ts_dt[uid == u, date]
      date_r <- ts_dt[id == sel_id & date >= date_l + ground_buffer - 1, min(date)]
      ts_dt[id == sel_id & date >= date_l & date <= date_r, ground := FALSE]
      ts_dt[id == sel_id & date == date_l, cutground := 0]
      ts_dt[id == sel_id & date == date_r, cutground := 1]
    }
  }
    
  # Remove maxima corresponding to removed minima
  clean_maxmin_ts(ts_dt, "peak1", "cut1", check_peaks = TRUE, check_cuts = FALSE, ids = ids)
  ts_dt[, c("peak2", "cut2") := list(peak1, cut1)]
  
  # Remove maxima with less than min_peakvalue
  ts_dt[peak2 & relval < min_peakvalue, peak2 := FALSE]
  # Remove minima with more than max_dropvalue
  ts_dt[cut2 & relval > max_dropvalue, cut2 := FALSE]
  # Clean maxima/minima
  clean_maxmin_ts(ts_dt, "peak2", "cut2", check_peaks = TRUE, check_cuts = TRUE)
  
  # Remove minima with less than relevance
  if (relevance > 0) {
    for (sel_id in unique(ts_dt$id)) {
      sel_ts_uidmin <- ts_dt[id == sel_id & cut2, uid]
      for (i in sel_ts_uidmin) {
        # compute ID of adjacent confirmed maxima
        suppressWarnings(uid_win <- c(
          ts_dt[id == sel_id & uid < i & peak2, max(uid)],
          ts_dt[id == sel_id & uid > i & peak2, min(uid)]
        ))
        if (all(!is.infinite(uid_win))) {
          # compute areas to compare
          area_den <- ts_dt[
            id == sel_id & date >= ts_dt[uid == uid_win[1], date] & date <= ts_dt[uid == uid_win[2], date], 
            sum(relval)
          ]
          area_num <- ts_dt[
            match(c(uid_win,i), uid), 
            mean(relval[c(1,2)]) * as.integer(diff(date[c(1,2)])) -
              mean(relval[c(1,3)]) * as.integer(diff(date[c(1,3)])) -
              mean(relval[c(2,3)]) * as.integer(diff(date[c(3,2)]))
          ]
          # area_num <- ts_dt[
          #   match(c(uid_win,i), uid), 
          #   (mean(relval[c(1,2)]) - relval[3]) * as.integer(diff(date[c(1,2)])) - # area del trapezio
          #     diff(relval[c(3,1)]) * as.integer(diff(date[c(1,3)])) / 2 - # area del primo triangolo
          #     diff(relval[c(3,2)]) * as.integer(diff(date[c(3,2)])) / 2 # area del secondo triangolo
          # ]
          # check that the area is higher than relevance
          if (area_num/area_den < relevance) {
            ts_dt[uid == i, cut2 := FALSE]
            ts_dt[uid %in% uid_win & relval == ts_dt[uid %in% uid_win, min(relval)], peak2 := FALSE]
          }
        }
      }
    }
    # Remove maxima corresponding to removed minima
    clean_maxmin_ts(ts_dt, "peak2", "cut2", check_peaks = TRUE, check_cuts = FALSE, ids = ids)
  }
  
  # Remove maxima with less than min_h
  if (min_relh > 0) {
    ts_dt[,c("peak3", "cut3") := list(FALSE,FALSE)]
    for (sel_id in unique(ts_dt$id)) {
      sel_ts_uidmax <- ts_dt[id == sel_id & peak2,][order(relval, decreasing = TRUE), uid]
      for (i in sel_ts_uidmax) {
        # compute ID of adjacent confirmed maxima
        suppressWarnings(uid_win <- c(
          ts_dt[id == sel_id & uid < i & peak3, max(uid)],
          ts_dt[id == sel_id & uid > i & peak3, min(uid)]
        ))
        # compute ID of minimum values within this window
        uid_mins <- c(
          ts_dt[id == sel_id & uid < i & uid > uid_win[1],][relval==min(relval, na.rm=TRUE), max(uid)],
          ts_dt[id == sel_id & uid > i & uid < uid_win[2],][relval==min(relval, na.rm=TRUE), min(uid)]
        )
        # check that the difference with all the minima is > min_relh
        # and that all the minima are <= max_dropvalue
        if (all(
          ts_dt[i, relval] - ts_dt[uid_mins, relval] >= min_relh,
          ts_dt[uid_mins, relval] <= max_dropvalue
        )) {
          ts_dt[uid == i, peak3 := TRUE]
          ts_dt[uid %in% uid_mins, cut3 := TRUE]
        }
      }
    }
    # Remove maxima corresponding to removed minima
    clean_maxmin_ts(ts_dt, "peak3", "cut3", check_peaks = TRUE, check_cuts = TRUE, ids = ids)
  } else {
    ts_dt[,c("peak3", "cut3") := list(peak2,cut2)]
  }
  
  # # Remove "false cycles" (two minima without a maxima)
  # for (sel_id in unique(ts_dt$id)) {
  # for (i in ts_dt[id == sel_id & cut2 == TRUE, uid]) {
  #   j <- ts_dt[id == sel_id & uid > i & cut2 == TRUE,][1, uid]
  #   if (!is.na(j) && ts_dt[seq(i,j), !any(peak2)]) {
  #     ts_dt[uid %in% c(i,j), cut2 := FALSE]
  #     ts_dt[uid == quantile(seq(i,j), 0.5, type = 1), cut2 := TRUE]
  #   }
  # }
  # }
  
  
  ## Return output
  
  # DT with records of peaks
  peak_dt <- ts_dt[peak3 == TRUE, list(s1 = seq_len(.N), maxval = date), by = id]
  # DT with records of begin of the cycle
  begin_dt <- ts_dt[cut3 == TRUE, list(s1 = seq_len(.N), begin = date), by = id]
  # DT with records of end of the cycle
  end_dt <- begin_dt[s1 > 1,]
  end_dt[,s1 := s1-1]
  setnames(end_dt, "begin", "end", skip_absent = TRUE)
  # remove false begins of the cycle (dates only corresponding to ends)
  if (nrow(begin_dt) > 0) {
    begin_dt <- begin_dt[begin_dt[, .I[s1 < max(s1)], by = id]$V1,]
  }
  # bind DTs
  pheno_dt <- merge(merge(begin_dt, end_dt, by = c("id", "s1")), peak_dt, by = c("id", "s1"))
  
  if (nrow(pheno_dt) > 0) {
    
    # Assign year
    pheno_dt[,uid:=seq_len(.N)]
    pheno_dt[,y1:=as.integer(strftime(maxval,"%Y"))]
    pheno_dt[,newyear:=as.Date(paste0(y1,"-",strftime(newyearday,"%m-%d")))]
    if (as.integer(strftime(newyearday,"%m")) >= 7) {
      pheno_dt[,year:=ifelse(maxval>newyear,y1+1,y1)]
    } else {
      pheno_dt[,year:=ifelse(maxval>newyear,y1,y1-1)]
    }
    
    ## Filter cycles basing on numbers
    # Compute metric used to order cycles
    # TODO optimise speed
    if (weight_metric == "integral") {
      for (i in seq_len(pheno_dt[,.N])) {
        pheno_dt[
          i,
          weight := ts_dt[
            date>=pheno_dt[i,begin] & date<pheno_dt[i,end] & id==pheno_dt[i,id],
            sum(relval)
          ]
        ]
      }
    } else if (weight_metric == "length") {
      pheno_dt[, weight := as.numeric(end-begin)]
    } else if (weight_metric == "maxval") {
      for (i in seq_len(pheno_dt[,.N])) {
        pheno_dt[
          i,
          weight := ts_dt[
            date>=pheno_dt[i,begin] & date<pheno_dt[i,end] & id==pheno_dt[i,id],
            max(relval)
          ]
        ]
      }
    } else {
      pheno_dt[, weight := as.numeric(NA)]
    }
    
    # filter basing on this metric
    pheno_dt[,rank:=1+.N-rank(weight),by=list(id,year)]
    pheno_dt <- pheno_dt[rank<=n_cycles,]
    pheno_dt[,cycle:=seq_len(.N),by=list(id,year)]
    
  } else {
    pheno_dt$year <- pheno_dt$cycle <- integer()
    pheno_dt$weight <- numeric()
  }
  
  pheno_dt <- pheno_dt[,list(id, year, cycle, begin, end, maxval, weight)]
  # if (export_weight==FALSE) {pheno_dt[,weight:=NULL]}
  
  attr(pheno_dt, "gen_by") <- "cut_cycles"
  attr(pheno_dt, "weight") <- weight_metric
  pheno_dt
  
}


# Define internal function used to clean minima/maxima
clean_maxmin_ts <- function(
  ts_dt, # DT which is DIRECTLY modified
  which_peak, # name of the logical variable with peaks
  which_cut, # name of the logical variable with peaks
  ids, # ID to check
  check_peaks = TRUE, # if FALSE, check only cuts
  check_cuts = TRUE # if FALSE, check only peaks
) {
  # Avoid check notes for data.table related variables
  id <- uid <- relval <- NULL
  if (missing(ids)) {ids <- unique(ts_dt$id)}
  for (sel_id in ids) {
    # Check peaks among cuts (one peak per couple of cuts)
    sel_ts_peakscuts <- ts_dt[
      id == sel_id & (get(which_peak) | get(which_cut)), 
      list(uid, peak=get(which_peak), cut=get(which_cut))]
    
    if (check_peaks) {
      # Find which peaks are not preceded and followed by cuts
      cut_nrows <- sel_ts_peakscuts[,which(cut)]
      cut_nrows <- cut_nrows[which(diff(c(0, cut_nrows)) > 2)]
      cuts_id <- c(-Inf, sel_ts_peakscuts[cut == TRUE, uid], Inf)
      cuts_id_tocheck <- c(sel_ts_peakscuts[cut_nrows, uid], Inf)
      # Cycle only where needed
      for (cut_r in cuts_id_tocheck) {
        cut_l <- cuts_id[which(cuts_id == cut_r)-1]
        peak_uids_torm <- ts_dt[id == sel_id & uid >= cut_l & uid <= cut_r & get(which_peak),][order(relval, decreasing = TRUE), uid]
        if (all(is.finite(c(cut_l,cut_r)))) {peak_uids_torm <- peak_uids_torm[-1]}
        ts_dt[uid %in% peak_uids_torm, c(which_peak) := FALSE]
      }
    }
    # Check cuts among peaks (one cut per couple of peaks)
    if (check_cuts) {
      # Find which peaks are not preceded and followed by cuts
      peak_nrows <- sel_ts_peakscuts[,which(peak)]
      peak_nrows <- peak_nrows[which(diff(c(0, peak_nrows)) > 2)]
      peaks_id <- c(-Inf, sel_ts_peakscuts[peak == TRUE, uid], Inf)
      peaks_id_tocheck <- c(sel_ts_peakscuts[peak_nrows, uid], Inf)
      # Cycle only where needed
      for (peak_r in peaks_id_tocheck) {
        peak_l <- peaks_id[which(peaks_id == peak_r)-1]
        cut_uids_torm <- ts_dt[id == sel_id & uid >= peak_l & uid <= peak_r & get(which_cut),][order(relval, decreasing = FALSE), uid]
        cut_uids_torm <- cut_uids_torm[-1]
        ts_dt[uid %in% cut_uids_torm, c(which_cut) := FALSE]
      }
    }
  }
  return(invisible(NULL))
}
