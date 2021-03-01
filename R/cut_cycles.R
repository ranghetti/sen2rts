#' @title Cut cycles
#' @description Cut Sentinel-2 time series into separate cycles,
#'  detecting dates of cuts and of peaks.
#' @param ts Time series in `s2ts` format (generated using `fill_s2ts()`).
#' @param n_cycles (optional) Maximum number of cycles to be detected in one year
#'  (default: Inf, meaning that all the identified cycles are kept).
#'  A cycle overlapping the new year's day (argument `newyearday`)
#'  is assigned to the year in which the date of maximum value falls.
#' @param min_win (optional) Minimum time window between two consecutive 
#'  maxima / minima to consider a separate cycle.
#' @param min_relh (optional) Minimum relative difference between the maximum
#'  and each of the two minima to consider a separate cycle.
#' @param max_relval (optional) Maximum relative value to consider a 
#'  cycle breakpoint.
#' @param newyearday (optional) day to be considered as new year's day, used
#'  to assign cycles to the proper year.
#'  It can be an object of class `Date` (in which case the year is ignored) 
#'  or a character value in the form `mm-dd`.
#'  In case it is July 1 or higher, cycles whose maximum value is falling in
#'  the last  part of the year are assigned to the subsequent year; 
#'  otherwise, cycles whose maximum value is falling in the first part of the
#'  year are assigned  to the previous year. Default is January 1
#'  (all cycles are assigned the the year in which their maximum value falls).
#' @param rank_metric (optional) TODO
#' @return A data table with the following fields:
#'  - `id`: the time series ID (see `s2ts`);
#'  - `year`: the year assigned to each cycle;
#'  - `cycle`: the cycle ID (progressive integer within each year);
#'  - `begin`: the date of the begin of the cycle;
#'  - `end`: the date of the end of the cycle;
#'  - `maxval`: the date of the maximum value of the cycle;
#'  - `weight`: the value of the metric used for ranking seasons.
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @import data.table
#' @importFrom stats quantile
#' @importFrom methods as
#' @export

cut_cycles <- function(
  ts,
  n_cycles = Inf,
  min_win = 60,
  min_relh = 0.25,
  max_relval = 0.3,
  newyearday = "01-01",
  weight_metric = "integral"
) {
  
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
        ts_dt[id == sel_id & uid < i & uid > uid_win[1],][relval==min(relval, na.rm=TRUE), max(uid)],
        ts_dt[id == sel_id & uid > i & uid < uid_win[2],][relval==min(relval, na.rm=TRUE), min(uid)]
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
    
    # Remove "false cycles"
    for (i in ts_dt[id == sel_id & cut1 == TRUE, uid]) {
      j <- ts_dt[id == sel_id & uid > i & cut1 == TRUE,][1, uid]
      if (!is.na(j) && ts_dt[seq(i,j), !any(peak1)]) {
        ts_dt[uid %in% c(i,j), cut1 := FALSE]
        ts_dt[uid == quantile(seq(i,j), 0.5, type = 1), cut1 := TRUE]
      }
    }
  }
  
  
  ## Return output
  
  # DT with records of peaks
  peak_dt <- ts_dt[peak1 == TRUE, list(s1 = seq_len(.N), maxval = date), by = id]
  # DT with records of begin of the cycle
  begin_dt <- ts_dt[cut1 == TRUE, list(s1 = seq_len(.N), begin = date), by = id]
  # DT with records of end of the cycle
  end_dt <- begin_dt[s1 > 1,]
  end_dt[,s1 := s1-1]
  setnames(end_dt, "begin", "end")
  # remove false begins of the cycle (dates only corresponding to ends)
  begin_dt <- begin_dt[begin_dt[, .I[s1 < max(s1)], by = id]$V1,]
  # bind DTs
  pheno_dt <- merge(merge(begin_dt, end_dt, by = c("id", "s1")), peak_dt, by = c("id", "s1"))

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
  pheno_dt[,c("newyear","y1","s1","uid","rank"):=NULL]
  pheno_dt <- pheno_dt[,list(id, year, cycle, begin, end, maxval, weight)]
  # if (export_weight==FALSE) {pheno_dt[,weight:=NULL]}
  
  attr(pheno_dt, "gen_by") <- "cut_cycles"
  pheno_dt
  
}
