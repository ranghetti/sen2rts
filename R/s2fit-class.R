
# Internal function which will be used as methods of the upcoming class "s2fit"

#' @import data.table

s2fit_to_s2ts <- function(fit) {
  # Check s2fit format
  # TODO
  rbindlist(lapply(names(fit), function(sel_id) {
    rbindlist(lapply(names(fit[[sel_id]]), function(sel_year) {
    rbindlist(lapply(names(fit[[sel_id]][[sel_year]]), function(sel_cycle) {
      fit[[sel_id]][[sel_year]][[sel_cycle]][["ts"]][
        ,list("id" = sel_id, "year" = sel_year, "cycle" = sel_cycle, date, value)
        ]
    }))
    }))
  }))
}
