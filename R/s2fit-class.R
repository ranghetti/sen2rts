
# Internal function which will be used as methods of the upcoming class "s2fit"

s2fit_to_s2ts <- function(fit) {
  # Check s2fit format
  # TODO
  rbindlist(lapply(names(fit), function(sel_id) {
    rbindlist(lapply(names(fit[[sel_id]]), function(sel_season) {
      fit[[sel_id]][[sel_season]][["ts"]][
        ,list("id" = sel_id, date, "season" = sel_season, value)
        ]
    }))
  }))
}
