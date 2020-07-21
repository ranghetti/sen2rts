
# WIP define a curve fitting function using https://github.com/gianlucafilippa/phenopix/blob/master/R/greenProcess.R
#' @title Fit curve over seasons
#' @description Fit a curve using a parametric function  from 
#'  `phenopix::greenProcess()`.
#' @param ts Time series in `s2ts` format (generated using `cut_seasons()`).
#' @param fit Fitting function among `"klosterman"`, `"beck"`,
#'  `"elmore"` and `"gu"` (see `phenopix::greenProcess()`).
#' @return The output time series in tabular format (see `extract_ts()`),
#'  with a new column `fit` containing fitted values.
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @import data.table
#' @importFrom zoo zoo
#' @importFrom phenopix KlostermanFit BeckFit ElmoreFit GuFit
#' @importFrom methods as
#' @export


fit_curve <- function(
  ts,
  fit = "klosterman" # see greenProcess (except from "spline")
) {
  
  
  ## Check arguments
  # TODO
  
  # Check fit
  fit.fun <- if (fit=="klosterman") phenopix::KlostermanFit else
    if (fit=="beck") phenopix::BeckFit else
      if (fit=="elmore") phenopix::ElmoreFit else
        if (fit=="gu") phenopix::GuFit else
          print_message(
            type = "error",
            "Argument 'fun' only accepts values \"klosterman\", \"beck\", ",
            "\"elmore\" and \"gu\"."
          )
  
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
  
  # Compute relative values
  ts_dt[,relval := (value - min(value, na.rm=TRUE)) / diff(range(value, na.rm=TRUE))]
  
  
  # Fit for each ID/season
  for (sel_id in unique(ts_dt$id)) {
    
    print(sel_id)
    cut_dates_c <- ts_dt[id == sel_id & pheno == "cut_seas", date]
    cut_dates <- lapply(seq_along(cut_dates_c)[-1], function(i) {cut_dates_c[c(i-1,i)]})
    
    for (sel_cut_date in cut_dates) {
      print(sel_cut_date)
      # exclude false seasons (no peaks between cut_date)
      if (
        sum(ts_dt[id == sel_id & date >= sel_cut_date[1] & date < sel_cut_date[2], 
                  pheno] == "peak", na.rm=TRUE) > 0
      ) {
        sel_ts_zoo <- zoo::zoo(
          ts_dt[id == sel_id & date >= sel_cut_date[1] & date < sel_cut_date[2], relval]
        )
        
        sel_ts_fit <- try(fit.fun(ts = sel_ts_zoo, uncert = FALSE))
        if (!inherits(sel_ts_fit, "try-error")) {
          ts_dt[
            id == sel_id & date >= sel_cut_date[1] & date < sel_cut_date[2],
            relval_fit := as.numeric(sel_ts_fit$fit$predicted)
            ]
        }
      }
    } # end of cut_date FOR cycle
    
  } # end of id FOR cycle
  
  ts_dt[,fit := min(value, na.rm=TRUE) + relval_fit * diff(range(value, na.rm=TRUE))]
  ts_dt[,relval_fit := NULL]
  
  ts_out <- as(ts_dt, "s2ts")
  attr(ts_out, "gen_by") <- "fit_curve"
  ts_out
  
}
