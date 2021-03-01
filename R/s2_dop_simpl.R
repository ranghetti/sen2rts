#' @title Simplified `sen2r::s2_dop()` for internal usage
#' @description This is a simplified version of `s2_dop()` which returns
#'  Sentinel-2 passages over certain orbits during a defined time interval.
#'  Checks over inputs are skipped to save time, and S2B dates before
#'  Sentinel-2 launch can also be returned, since this is required by 
#'  `smooth_ts()`.
#' @param s2_orbits A vector of Sentinel-2 orbits (as integer numbers
#'  or 3-length character).
#' @param timewindow Temporal window for querying: Date object
#'  of length 2 (time window).
#' @param mission (optional) Vector with the desired Sentinel-2 missions
#'  ("2A", "2B" or both).
#' @return A data table with the dates (column "date"), the missions
#' (column "mission") and the orbits (column "orbit").
#' @author Luigi Ranghetti, PhD (2021) \email{luigi@@ranghetti.info}
#' @note License: GPL 3.0
#' @import data.table
#' @importFrom jsonlite fromJSON
#' @importFrom methods is


s2_dop_simpl <- function(s2_orbits, timewindow, mission) {
  
  # to avoid NOTE on check
  type <- orbit <- doybase <- orbit <- NULL
  
  # generate doybase.json if missing
  eval(parse(text = "json_path <- sen2r:::create_s2_dop()"))
  s2_dop_dt <- data.table(jsonlite::fromJSON(json_path)$dop)
  
  dates_all <- seq(timewindow[1], timewindow[2], by = "day")

  ## Compute the dates
  sel_dop_dt <- s2_dop_dt[orbit %in% s2_orbits,]
  s2a_dates <- if ("2A" %in% mission) {
    dates_all[(as.integer(dates_all)%%10) %in% sel_dop_dt$doybase]
  } else {
    as.Date(character(0))
  }
  s2b_dates <- if ("2B" %in% mission) {
    dates_all[(as.integer(dates_all)%%10) %in% ((sel_dop_dt$doybase+5)%%10)]
  } else {
    as.Date(character(0))
  }
  s2a_orbits <- lapply(s2a_dates, function(d) {sel_dop_dt[doybase==as.integer(d)%%10,orbit]})
  s2b_orbits <- lapply(s2b_dates, function(d) {sel_dop_dt[doybase==(as.integer(d)+5)%%10,orbit]})
  s2_missions <- c(rep("2A", length(s2a_dates)), rep("2B", length(s2b_dates)))
  s2_data <- rbindlist(lapply(seq_along(s2_missions), function(i) {
    expand.grid(
      "date" = c(s2a_dates, s2b_dates)[i],
      "mission" = s2_missions[i],
      "orbit" = c(s2a_orbits, s2b_orbits)[[i]],
      stringsAsFactors = FALSE
    )
  }))
  
  if (nrow(s2_data) > 0) {
    # Order data
    setorder(s2_data, date, mission, orbit)
    return(s2_data)
  } else {
    return(
      data.table(
        "date" = as.Date(character(0)),
        "mission" = character(0),
        "orbit" = character(0),
        stringsAsFactors = FALSE
      )
    )
  }
  
}
