
## Class definition ----

setClass("s2ts", contains = "list")

#' @return The output time series in `s2ts` format, containing the following 
#'  columns:
#'  - `date` :date of observation;
#'  - `id` : polygon ID, corresponding to argument `in_sf_id`;
#'  - `value`: extracted value;
#'  - `quality`: relative 0-1 quality values (missing if `scl_paths` was not provided).

s2ts <- function(value, date, id, qa, ...) {
  out <- structure(value, date = date, id = id)
  attr(value, "id") <- if (missing(id)) {rep("0", length(value))} else {id}
  if (!missing(qa)) {
    attr(out, "qa") <- qa
  }
  args <- c(as.list(environment()), list(...))
  args <- args[!names(args) %in% c("args", "out", "value", "date", "id", "qa")]
  for (sel_arg in names(args)) {
    attr(out, sel_arg) <- args[[sel_arg]]
  }
  class(out) <- unique(c("s2ts", class(out)))
  out
}


## Methods: input -> s2ts ----

setAs("numeric", "s2ts", function(from) {
  stopifnot(!is.null(attr(from, "date")))
  if (missing(attr(from, "id"))) {attr(from, "id") <- rep("0", length(from))}
  class(from) <- unique(c("s2ts", class(from)))
  from
})

setAs("data.frame", "s2ts", function(from) {
  stopifnot(c("value", "date") %in% names(from))
  to <- from$value
  attr(to, "date") <- from$date
  attr(to, "id") <- if (is.null(from$id)) {rep("0", nrow(from))} else {from$id}
  if (!is.null(from$qa)) {attr(to, "qa") <- from$qa}
  for (colname in names(from)[!names(from) %in% c("value", "date", "qa")]) {
    attr(to, colname) <- from[[colname]]
  }
  class(to) <- unique(c("s2ts", class(to)))
  to
})


## Extract data from s2ts ----

s2ts_date <- function(x) {
  stopifnot(inherits(x, "s2ts"))
  attr(x, "date")
}
s2ts_id <- function(x) {
  stopifnot(inherits(x, "s2ts"))
  sort(unique(attr(x, "id")))
}
s2ts_value <- function(x) {
  stopifnot(inherits(x, "s2ts"))
  dcast(as.data.table(x), date ~ id, value.var = "value")
}
s2ts_qa <- function(x) {
  stopifnot(inherits(x, "s2ts"))
  if (!is.null(attr(x, "qa"))) {
    dcast(as.data.table(x), date ~ id, value.var = "qa")
  } else {
    print_message(
      type = "message",
      "Quality flag is missing."
    )
    invisible(NULL)
  }
}

`$.s2ts` = function(x, name) {
  if (name == "date") {
    s2ts_date(x)
  } else if (name == "id") {
    s2ts_id(x)
  } else if (name == "value") {
    s2ts_value(x)
  } else if (name == "qa") {
    s2ts_qa(x)
  } 
}

`[[.s2ts` = function(x, name) {
  which_sel <- attr(x, "id") == name
  s2ts(
    as.vector(x)[which_sel], 
    date = attr(x, "date")[which_sel],
    id = attr(x, "id")[which_sel],
    qa = attr(x, "qa")[which_sel]
  )
}


## Methods: s2ts -> input ----

as.data.frame.s2ts <- function(x, ...) {
  # Define attributes: which before "value" and which after
  which_attrs_before <- c("id", "date", "orbit", "sensor")
  attrs <- names(attributes(x))[!names(attributes(x)) %in% c("class")]
  attrs_before <- which_attrs_before[which_attrs_before %in% attrs]
  attrs_after <- attrs[!attrs %in% which_attrs_before]
  to_list <- list()
  for (a in attrs_before) {
    to_list[[a]] <- attr(x, a)
  }
  to <- as.data.frame(to_list, stringsAsFactors = FALSE)
  to$value <- as.vector(x)
  for (a in attrs_after) {
    if (length(attr(x, a)) == nrow(to)) {
      to[,a] <- attr(x, a)
    } else {
      attr(to, a) <- attr(x, a)
    }
  }
  to
}
setAs("safelist", "data.frame", function(from) {
  as.data.frame(from, ...)
})

as.data.table.s2ts <- function(x, ...) {
  data.table(as.data.frame(x), ...)
}
setAs("safelist", "data.table", function(from) {
  as.data.table(from)
})


## Print ----

print.s2ts <- function(x, ...) {
  
  n_ids <- 6 # maximum number of IDs to print
  
  x_dt <- as.data.table(x)
  ids <- sort(unique(x_dt$id))
  sel_ids <- ids[seq(min(n_ids, length(ids)))]
  x_dt <- x_dt[id %in% sel_ids,]
  dcast_lhs <- c("date", c("orbit", "sensor")[c("orbit", "sensor") %in% names(x_dt)])
  dcast_formula <- as.formula(paste(paste(dcast_lhs, collapse = " + "), "~ id"))
  x_dt$flag <- ""
  if (!is.null(attr(x, "qa"))) {
    x_dt[,flag := ifelse(is.na(qa), "", 
                         ifelse(qa == 1, "\u25CF ", 
                                ifelse(qa > 0.9, "\u25D5 ", 
                                       ifelse(qa > 0.75, "\u25D1 ", 
                                              ifelse(qa > 0.5, "\u25D4 ", "\u25CB ")))))]
  }
  if (!is.null(attr(x, "interpolated"))) {
    x_dt[interpolated == TRUE,flag := "~ "]
  }
  x_dt_cast <- dcast(x_dt, dcast_formula, value.var = c("value","flag"))
  setcolorder(
    x_dt_cast, 
    c(seq_along(dcast_lhs), 
      as.vector(t(matrix(seq(length(dcast_lhs)+1, ncol(x_dt_cast)), ncol=2)))
    )
  )
  setnames(
    x_dt_cast, 
    c("date", "orbit", "sensor"), 
    c("Date", "Orbit", "Sensor"), 
    skip_absent = TRUE
  )
  setnames(
    x_dt_cast,
    names(x_dt_cast)[grep("^value_",names(x_dt_cast))],
    gsub("^value_", "", names(x_dt_cast)[grep("^value_",names(x_dt_cast))])
  )
  setnames(
    x_dt_cast,
    names(x_dt_cast)[grep("^flag_",names(x_dt_cast))],
    rep("\u00A0", sum(grepl("^flag_",names(x_dt_cast))))
  )
  
  cat("A")
  if (!is.null(attr(x, "gen_by"))) {
    if (attr(x, "gen_by") == "extract_s2ts") {
      cat(" raw")
    } else if (attr(x, "gen_by") == "smooth_s2ts") {
      cat(" smoothed")
    } else if (attr(x, "gen_by") == "fill_s2ts") {
      cat("n interpolated")
    }
  }
  cat(" s2ts time series with", nrow(x_dt_cast), "dates and", length(ids), "IDs.\n")
  print(x_dt_cast)
  if (length(ids) > n_ids) {
    cat("...with", length(ids)-n_ids, "more IDs.\n")
  }
  if (any(!is.null(attr(x, "qa")), !is.null(attr(x, "gen_by")) && attr(x, "gen_by") == "fill_s2ts")) {
    cat("\n")
  }
  if (!is.null(attr(x, "qa"))) {
    cat("Quality flags:  \u25CF [1]  \u25D5 [0.9,1)  \u25D1 [0.75,0.9)  \u25D4 [0.5,0.75)  \u25CB [0,0.5)\n")
  }
  if (!is.null(attr(x, "gen_by")) && attr(x, "gen_by") == "fill_s2ts") {
    cat("Interpolated values are marked with ‘~’.\n")
  }
  attrs <- names(attributes(x))[!names(attributes(x)) %in% c("class", "date", "id", "qa", "orbit", "sensor", "interpolated", "gen_by")]
  if (length(attrs) > 0) {
    cat("The following attributes are included:", paste(attrs, collapse=", "))
    cat(".\n")
  }
  
  invisible(x)

}