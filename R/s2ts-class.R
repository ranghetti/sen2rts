
## Class definition ----

setClass("s2ts", contains = "list")

#' @return The output time series in `s2ts` format, containing the following 
#'  columns:
#'  - `date` :date of observation;
#'  - `id` : polygon ID, corresponding to argument `in_sf_id`;
#'  - `value`: extracted value;
#'  - `quality`: relative 0-1 quality values (missing if `scl_paths` was not provided).

s2ts <- function(value, date, id = NA, qa, orbit, sensor, rawval, ...) {
  
  # leave as first function command!
  args <- c(as.list(environment()), list(...))
  
  # Define constants
  # mandatory arguments
  which_args_mandatory <- c("value", "date")
  # arguments for which the length must be the same as value
  which_args_l_asval <- c("date", "qa", "orbit", "sensor", "rawval")
  # arguments for which the length can be the same as value or 1 
  # (in which case it is replicated to the length of value)
  which_args_l_toval <- c("id")
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
      paste(which_args_mandatory[args_mandatory_missing], sep = "', '"),
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
  
  out <- list()
  for (a in args_before) {
    out[[a]] <- args[[a]]
  }
  out[["value"]] <- value
  for (a in args_after) {
    if (length(args[[a]]) == length(value)) {
      out[[a]] <- args[[a]]
    } else {
      attr(out, a) <- args[[a]]
    }
  }
  
  # Return s2ts
  class(out) <- "s2ts"
  out
  
}


## Methods: input -> s2ts ----

setAs("numeric", "s2ts", function(from) {
  # Accept a named vector with values, names being dates
  # and optional additional arguments
  stopifnot(!is.null(names(from)))
  from_list <- c(
    list("value" = as.vector(tmp)), 
    attributes(tmp)
  )
  from_list[["date"]] <- as.Date(from_list[["names"]])
  from_list[["names"]] <- NULL
  do.call(s2ts, from_list)
})

setAs("data.frame", "s2ts", function(from) {
  do.call(s2ts, as.list(from))
})

setAs("list", "s2ts", function(from) {
  do.call(s2ts, from)
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

`[.s2ts` = function(x, name) {
  x_dt <- as.data.table(x) 
  if (all(!name %in% x_dt$id)) {
    print_message(
      type = "error",
      ""
    )
  }
  as(x_dt[id %in% name,], "s2ts")
}


## Methods: s2ts -> input ----

as.list.s2ts <- function(x, ...) {
  unclass(x)
}
setAs("safelist", "list", function(from) {
  as.list(from)
})

as.data.frame.s2ts <- function(x, ...) {
  as.data.frame(as.list(x), stringsAsFactors = FALSE, ...)
}
setAs("safelist", "data.frame", function(from) {
  as.data.frame(from)
})

as.data.table.s2ts <- function(x, ...) {
  data.table(as.data.frame(x), ...)
}
setAs("safelist", "data.table", function(from) {
  as.data.table(from)
})


## Print ----

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