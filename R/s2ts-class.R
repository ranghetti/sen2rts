
## Class definition ----

#' @title Class `s2ts`
#' @description TODO
#' @param value (mandatory) Vector with the values ("y") of the time series.
#' @param date (mandatory) Vector (of the same length of `value`) with the dates
#'  of each value.
#' @param id (optional) Vector (of the same length of `value` or of length 1,
#'  in which case the passed value is replicated for each element of `value`)
#'  with the feature IDs of each value. 
#'  If missing, all the values are considered as extracted from the same feature.
#' @param qa (optional) Vector (of the same length of `value`) with the quality
#'  assessment values (range 0-1) associated to each value.
#'  If missing, all the values are considered as equally weighted.
#' @param orbit (optional) Vector (of the same length of `value`or of length 1,
#'  in which case the passed value is replicated for each element of `value`)
#'  with the Sentinel-2 orbits of each value.
#' @param sensor (optional) Vector (of the same length of `value` or of length 1,
#'  in which case the passed value is replicated for each element of `value`)
#'  with the Sentinel-2 sensors (`"2A"` or `"2B"`) of each value.
#' @param rawval (optional) Vector (of the same length of `value`) with the
#'  non-smoothed values (this is generally created by outputs of `smooth_s2ts()`).
#' @param ... Additional vectors to be passed. 
#'  Each additional argument is threated as an additional element of the output
#'  list in case it is of the same length of `value`, otherwise it is threated
#'  as an output attribute.
#' @return The output time series in `s2ts` format.
#' 
#'  This is a format derived from `data.table`, containing the mandatory columns
#'  `id`, `date`, `value`, and additional ones among which
#'  `qa`, `orbit`, `sensor` and `rawval` (see the arguments in this help for
#'  their meanings).
#'  
#'  The optional attribute `gen_by` provides information about the function 
#'  which generated the object (if provided).
#' @details Some specific methods for this class are defined.
#'  - `<s2ts_obj>$value` returns a `data.table` with the values for each date, 
#'      in wide format.
#'  - `<s2ts_obj>$date` returns a `vector` with the dates.
#'  - `<s2ts_obj>$id` returns a `vector` with the unique ID values.
#'  - `<s2ts_obj>$qa` returns a `data.table` with the quality assessment values 
#'      for each date, in wide format.
#'  - `<s2ts_obj>$rawval` returns a `data.table` with the `rawval` values for 
#'      each date, in wide format.
#'  - `<s2ts_obj>[[<idname>]]` returns a subsampled `s2ts` containing only
#'      the values of the provided ID.
#' 
#'  Notice that since `$` and `[[` methods are defined, the syntaxes
#'  `<s2ts_obj>$<fieldname>` and `<s2ts_obj>[["<fieldname>"]]`
#'  can not be used (use instead `<s2ts_obj>[,<fieldname>]`).
#' @author Luigi Ranghetti, PhD (2020) \email{luigi@@ranghetti.info}
#' @export

s2ts <- function(value, date, id = NA, qa, orbit, sensor, rawval, ...) {
  
  # leave as first function command!
  args <- c(as.list(environment()), list(...))
  
  # Define constants
  # mandatory arguments
  which_args_mandatory <- c("value", "date")
  # arguments for which the length must be the same as value
  which_args_l_asval <- c("date", "qa", "rawval")
  # arguments for which the length can be the same as value or 1 
  # (in which case it is replicated to the length of value)
  which_args_l_toval <- c("id", "orbit", "sensor")
  # arguments for which the length must be 1
  which_args_l_1 <- c("gen_by")
  # arguments which must be placed before 'value' (in this order)
  which_args_before <- c("id", "date", "orbit", "sensor")
  
  # Define arguments
  args_all <- names(args)
  args_formal <- names(formals())
  args_formal <- args_formal[!args_formal %in% c("...")]
  args_formal_missing <- logical(0)
  for (a in args_formal) {
    # args_formal_missing[a] <- do.call(missing, list(a))
    args_formal_missing[a] <- inherits(args[[a]], "name")
    # in this way, also arguments with a default value are considered
  }
  args_passed <- args_all[!args_all %in% args_formal[args_formal_missing]]
  
  # Check mandatory arguments
  args_mandatory_missing <- logical(0)
  for (a in which_args_mandatory) {
    args_mandatory_missing[a] <- do.call(missing, list(a))
  }
  if (any(args_mandatory_missing)) {
    print_message(
      type = "error",
      "Some mandatory arguments ('",
      paste(which_args_mandatory[args_mandatory_missing], collapse = "', '"),
      "') are missing."
    )
  }
  
  # Check argument lengths
  for (a in which_args_l_toval) {
    if (a %in% args_passed && length(args[[a]]) == 1) {
      args[[a]] <- rep(args[[a]], length(value))
    }
  }
  args_l_not_asval <- logical(0)
  for (a in c(which_args_l_asval, which_args_l_toval)) {
    args_l_not_asval[a] <- a %in% args_passed && length(args[[a]]) != length(value)
  }
  if (any(args_l_not_asval)) {
    print_message(
      type = "error",
      "Arguments '",
      paste(names(args_l_not_asval)[args_l_not_asval], sep = "', '"),
      "' must be of the same length of 'value'."
    )
  }
  args_l_not_1 <- logical(0)
  for (a in which_args_l_1) {
    args_l_not_1[a] <- a %in% args_passed && length(args[[a]]) != 1
  }
  if (any(args_l_not_1)) {
    print_message(
      type = "error",
      "Arguments '",
      paste(which_args_l_1[args_l_not_1], sep = "', '"),
      "' must be of length 1."
    )
  }
  
  # Define arguments: which before "value" and which after
  args_before <- which_args_before[which_args_before %in% args_passed]
  args_after <- args_passed[!args_passed %in% c("value",which_args_before)]
  
  out_l <- list()
  for (a in args_before) {
    out_l[[a]] <- args[[a]]
  }
  out_l[["value"]] <- value
  for (a in args_after) {
    if (length(args[[a]]) == length(value)) {
      out_l[[a]] <- args[[a]]
    } else {
      attr(out_l, a) <- args[[a]]
    }
  }
  
  # Return s2ts
  out <- as.data.table(out_l)
  for (a in names(attributes(out_l))[names(attributes(out_l)) != "names"]) {
    attr(out, a) <- attr(out_l, a)
  }
  
  # Check univocity
  if (any(any(out[,duplicated(paste(id, date))]))) {
    print_message(
      type = "error",
      "Dpuplicated were detected; please check your input data ",
      "and ensure that only a unique data was provided for each ID value."
    )
  }
  
  class(out) <- c("s2ts", class(out))
  out
  
}

setClass("s2ts", contains = "list")


## Methods: input -> s2ts ----

#' @param x Input element to be converted to `s2ts`.
#' @name as.s2ts
#' @rdname s2ts
#' @export
as.s2ts <- function(x, ...) {
  UseMethod("as.s2ts")
}

as.s2ts.numeric <- function(x, ...) {
  # Accept a named vector with values, names being dates
  # and optional additional arguments
  stopifnot(!is.null(names(x)))
  x_list <- c(
    list("value" = as.vector(x)), 
    attributes(x)
  )
  x_list[["date"]] <- as.Date(x_list[["names"]])
  x_list[["names"]] <- NULL
  do.call(s2ts, x_list)
}
setAs("numeric", "s2ts", function(from) {
  as.s2ts.numeric(from)
})

as.s2ts.integer <- function(x, ...) {
  as.s2ts.numeric(x, ...)
}
setAs("integer", "s2ts", function(from) {
  as.s2ts.integer(from)
})


as.s2ts.data.frame <- function(x, ...) {
  as.s2ts(as.list(x))
}
setAs("data.frame", "s2ts", function(from) {
  as.s2ts.data.frame(from)
})

as.s2ts.list <- function(x, ...) {
  for (a in names(attributes(x))[names(attributes(x)) != "names"]) {
    x[[a]] <- attr(x, a)
  }
  do.call(s2ts, x)
}
setAs("list", "s2ts", function(from) {
  as.s2ts.list(from)
})


## Extract data from s2ts ----

s2ts_date <- function(x) {
  stopifnot(inherits(x, "s2ts"))
  unclass(x)[["date"]]
}
s2ts_id <- function(x) {
  stopifnot(inherits(x, "s2ts"))
  sort(unique(unclass(x)[["id"]]))
}
s2ts_value <- function(x) {
  stopifnot(inherits(x, "s2ts"))
  dcast(as.data.table(x)[!is.na(value),], date ~ id, value.var = "value")
}
s2ts_qa <- function(x) {
  stopifnot(inherits(x, "s2ts"))
  if (!is.null(unclass(x)[["qa"]])) {
    dcast(as.data.table(x)[!is.na(qa),], date ~ id, value.var = "qa")
  } else {
    print_message(
      type = "message",
      "Quality flag is missing."
    )
    invisible(NULL)
  }
}
s2ts_rawval <- function(x) {
  stopifnot(inherits(x, "s2ts"))
  if (!is.null(unclass(x)[["rawval"]])) {
    dcast(as.data.table(x)[!is.na(rawval),], date ~ id, value.var = "rawval")
  } else {
    print_message(
      type = "message",
      "Non-smoothed values are missing (probably this is not a smoothed s2ts)."
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
  } else if (name == "rawval") {
    s2ts_rawval(x)
  }
}

`[[.s2ts` = function(x, name) {
  x_dt <- as.data.table(x)
  as(x_dt[id %in% name,], "s2ts")
}

`[.s2ts` = function(x, ...) {
  x_dt <- as.data.table(x)[...]
  tryCatch(
    as(x_dt, "s2ts"),
    error = function(e) {x_dt}
  )
}


## Methods: s2ts -> input ----

#' @export
as.list.s2ts <- function(x, ...) {
  unclass(x)
}
setAs("s2ts", "list", function(from) {
  as.list(from)
})


## Plot ----

#' @export
plot.s2ts <- function(x, ...) {
  
  # Check optional suggested ggplot2 to be present
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    print_message(
      type = "error",
      "Package 'ggplot2' is required to plot a 's2ts' object."
    )
  }
  
  # Determine plot mode
  plot_mode <- if (is.null(attr(x, "gen_by"))) {
    "base"
  } else if (attr(x, "gen_by") %in% c("smooth_s2ts")) {
    "smoothed"
  } else if (attr(x, "gen_by") %in% c("fill_s2ts")) {
    "filled"
  } else { # including attr(x, "gen_by") == "extract_s2ts"
    "base"
  }
  
  # Extract input data.table
  x_dt <- as.data.table(x)
  setnames(x_dt, "qa", "QA", skip_absent = TRUE)
  if (plot_mode == "filled") {
    x_dt_smooth <- x_dt
  } else {
    x_dt_smooth <- x_dt[!is.na(value),]
  }
  if (plot_mode == "base") {
    x_dt_raw <- x_dt[!is.na(value),]
  } else {
    x_dt_raw <- x_dt[!is.na(rawval),]
    x_dt_raw[,value := rawval]
  }
  
  # Base plot
  out <- ggplot2::ggplot(x_dt, ggplot2::aes(x = date, y = value))
  
  # Add raw line
  out <- out + ggplot2::geom_line(
    data = x_dt_raw, 
    alpha = if (plot_mode %in% c("smoothed", "filled")) {0.1} else {0.35}
  )
  
  # Add smoothed line
  if (plot_mode %in% c("smoothed", "filled")) {
    out <- out + ggplot2::geom_line(data = x_dt_smooth, alpha = 0.5)
  }
  
  # Add points
  out <- out + ggplot2::geom_point(
    data = x_dt_raw, 
    if (!is.null(x_dt$QA)) {ggplot2::aes(colour = QA)}, 
    size = 0.75
  )
  
  # Facet in case of multiple IDs
  if (length(sort(unique(x_dt$id))) > 0) {
    out <- out + ggplot2::facet_wrap(~id)
  }
  
  # Format options
  out <- out +
    ggplot2::scale_x_date(name = "Date") +
    ggplot2::scale_y_continuous(name = NULL) +
    ggplot2::scale_colour_viridis_c(option = "inferno", direction = -1) +
    ggplot2::theme_light()
  
  out
  
}


## Print ----

#' @export
print.s2ts <- function(x, ...) {
  
  # Define constants
  # maximum number of IDs to print
  n_ids <- 6
  
  x_dt <- as.data.table(x)
  ids <- sort(unique(x_dt$id))
  sel_ids <- ids[seq(min(n_ids, length(ids)))]
  x_dt <- x_dt[id %in% sel_ids,]
  dcast_lhs <- c("date", c("orbit", "sensor")[c("orbit", "sensor") %in% names(x_dt)])
  dcast_formula <- as.formula(paste(paste(dcast_lhs, collapse = " + "), "~ id"))
  x_dt$flag <- ""
  if (!is.null(x_dt$qa)) {
    x_dt[,flag := ifelse(is.na(qa), "", 
                         ifelse(qa == 1, "\u25CF ", 
                                ifelse(qa > 0.9, "\u25D5 ", 
                                       ifelse(qa > 0.75, "\u25D1 ", 
                                              ifelse(qa > 0.5, "\u25D4 ", "\u25CB ")))))]
  }
  if (!is.null(x_dt$interpolated)) {
    x_dt[interpolated == TRUE,flag := "~ "]
  }
  
  if (nrow(x_dt) > 0) {
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
    # in case of a single TS without explicit ID
    setnames(x_dt_cast, "NA", "Value", skip_absent = TRUE)
  }
  
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
  if (nrow(x_dt) == 0) {
    cat(" empty s2ts time series.\n")
  } else {
    cat(" s2ts time series with", nrow(x_dt_cast), "dates")
    if (length(ids) > 0) {cat(" and", length(ids), "IDs")}
    cat(".\n")
    print(x_dt_cast)
    if (length(ids) > n_ids) {
      cat("...with", length(ids)-n_ids, "more IDs.\n")
    }
    if (any(!is.null(x_dt$qa), !is.null(attr(x, "gen_by")) && attr(x, "gen_by") == "fill_s2ts")) {
      cat("\n")
    }
    if (!is.null(x_dt$qa)) {
      cat("Quality flags:  \u25CF [1]  \u25D5 [0.9,1)  \u25D1 [0.75,0.9)  \u25D4 [0.5,0.75)  \u25CB [0,0.5)\n")
    }
    if (!is.null(attr(x, "gen_by")) && attr(x, "gen_by") == "fill_s2ts") {
      cat("Interpolated values are marked with ‘~’.\n")
    }
  }
  
  invisible(x)
  
}
