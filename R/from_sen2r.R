# Internal functions imported from package sen2r.
#
# These functions were originally provided by package {sen2r}
# (https://sen2r.ranghetti.info, GPL-3, same author as {sen2rts}).
# They are included here as internals so that {sen2rts} no longer requires
# {sen2r} as a mandatory dependency (see NEWS).
# `raster_metadata()` is a reduced version tailored to the needs of {sen2rts}
# (only `format = "list"` and metadata `res`/`bbox`/`proj`).


#' @title Print a message
#' @description A common interface for printing messages of several types.
#' @param ... `R` objects which are concatenated.
#' @param type Type of the output. Accepted values: `'message'`, `'string'`,
#'  `'cat'`, `'error'`, `'warning'`, `'waiting'`.
#' @param sep (optional) character used to separate input values.
#' @param date Logical: set `TRUE` to place the date before the message.
#' @param date_format Format of the date (see `strftime()`).
#' @param width Positive integer: target column for wrapping lines.
#' @param indent,exdent Indentation of the first / subsequent lines.
#' @param prefix,initial Prefix for each line / for the first line.
#' @return Message (in the defined format).
#' @author Luigi Ranghetti, PhD (2020)
#' @note Originally from package {sen2r} (GPL-3).
#' @keywords internal

print_message <- function(
  ...,
  type,
  sep = "",
  date = FALSE,
  date_format = "",
  width = 0.9 * getOption("width"),
  indent = TRUE, exdent = TRUE,
  prefix = "", initial = prefix
) {

  if (date == TRUE) {
    initial <- paste0(initial, "[",strftime(Sys.time(), format=date_format),"] ")
  }
  if (all(is.logical(exdent), exdent)) {
    exdent <- max(0, min(
      nchar(initial) - nchar(prefix),
      .1 * width
    ))
  }
  if (all(is.logical(indent), indent)) {
    indent <- max(0, min(
      nchar(prefix) - nchar(initial),
      .1 * width
    ))
  }
  message_string <- strwrap(
    unlist(strsplit(paste(c(...), collapse=sep),"\n")),
    width = width,
    indent = indent, exdent = exdent,
    prefix = prefix, initial = initial
  )
  switch(
    type,
    message = message(paste(message_string, collapse = "\n")),
    string = message_string,
    cat = cat(message_string, sep="\n"),
    error = stop(paste0("\n",paste0(message_string, collapse = "\n")), call.=FALSE),
    warning = warning(paste(message_string, collapse = "\n"), call.=FALSE),
    waiting = {cat(message_string, sep="\n"); readline(prompt = "... ")},
    stop(paste0("Type '",type,"' not yet supported."))
  )
}


#' @title Express file paths in canonical form depending on the operating system
#' @description Accessory function wrapper for `normalizePath()` in Linux
#'  and `shortPathName(normalizePath())` in Windows.
#' @param path character vector of file paths
#' @param ... additional parameters passed to [normalizePath] (i.e. `mustWork`).
#' @return The paths normalized.
#' @author Luigi Ranghetti, PhD (2019)
#' @note Originally from package {sen2r} (GPL-3).
#' @keywords internal
#' @importFrom utils shortPathName

normalize_path <- function(path, ...) {

  if (Sys.info()["sysname"] == "Windows") {
    utils::shortPathName(normalizePath(gsub("\\\\$", "", path), ...))

  } else {
    normalizePath(path, ...)
  }

}


#' @title Get information from S2 short name
#' @description This accessory function extracts metadata included in
#'  the name of a Sentinel-2 product which follows the sen2r naming convention.
#' @param s2_names A vector of Sentinel-2 product names in the
#'  sen2r naming convention.
#' @param naming_convention The naming convention used to extract information
#'  from `s2_names` names (`"sen2r"` or `"sen2r_new"`, or a custom list).
#' @param format One between `data.table` (default), `data.frame` and `list`.
#' @param abort Logical parameter: if TRUE (default), the function aborts
#'  in case any of `s2_names` is not recognised; if FALSE, a warning is shown.
#' @return A data.table, data.frame or list of the output metadata.
#' @author Luigi Ranghetti, PhD (2019)
#' @note Originally from package {sen2r} (GPL-3).
#' @keywords internal
#' @import data.table

sen2r_getElements <- function(
  s2_names,
  naming_convention,
  format = "data.table",
  abort = TRUE
) {

  # static definitions: regex
  list_regex <- list(
    "sen2r" = list(
      "regex" = "^S2([AB])([12][AC])\\_([0-9]{8})\\_([0-9]{3})\\_([^\\_\\.]*)\\_([^\\_\\.]+)\\_([126]0)\\.?([^\\_]*)$",
      "elements" = c("mission","level","sensing_date","id_orbit","extent_name","prod_type","res","file_ext"),
      "date_format" = "%Y%m%d"
    ),
    "sen2r_new" = list(
      "regex" = "^S2\\_([0-9]{8})\\_([0-9]{3})\\_([^\\_\\.]*)\\_([AB])\\_([^\\_\\.]+)\\.?([^\\_]*)$",
      "elements" = c("sensing_date","id_orbit","extent_name","mission","prod_type","file_ext"),
      "date_format" = "%Y%m%d"
    )
  )

  # check format
  if (!format %in% c("list", "data.frame", "data.table")) {
    print_message(
      type="warning",
      "Argument must be one between 'data.frame' and 'list'.",
      "Returnig a list.")
    format <- "list"
  }

  # if input is NULL, return NULL
  if (is.null(s2_names)) {
    return(invisible(NULL))
  }
  s2_names <- basename(s2_names)

  # define regular expressions to identify products
  if (missing(naming_convention) || is.null(naming_convention)) {
    # try all the defined conventions
    regex_match <- sapply(list_regex, function(x){sum(grepl(x$regex,s2_names))})
    if (regex_match[["sen2r"]] == 0 & regex_match[["sen2r_new"]] > 0) {
      naming_convention <- "sen2r_new"
    } else {
      naming_convention <- "sen2r"
    }
  }
  if (inherits(naming_convention, "character")) {
    if (naming_convention[1] %in% c("sen2r", "sen2r_new")) {
      fs2nc_regex <- list_regex[[naming_convention[1]]]
    } else {
      print_message(
        type = "error",
        "The argument 'naming_convention' is not recognised."
      )
    }
  } else if (
    inherits(naming_convention, "list") &&
    all(c("regex", "elements", "date_format") %in% names(naming_convention))
  ) {
    fs2nc_regex <- naming_convention
  } else {
    print_message(
      type = "error",
      "The argument 'naming_convention' is not recognised."
    )
  }

  metadata <- data.frame(
    "type" = rep(NA, length(s2_names))
  ) # output object, with requested metadata

  # retrieve info
  for (sel_el in fs2nc_regex$elements) {
    # generic formattation
    metadata[,sel_el] <- gsub(
      fs2nc_regex$regex,
      paste0("\\",which(fs2nc_regex$elements==sel_el)),
      s2_names
    )
  }
  # specific formattations
  metadata[,"sensing_date"] <- as.Date(
    metadata[,"sensing_date"],
    format = fs2nc_regex$date_format
  )
  if (nrow(metadata)>0) {
    if (!is.null(metadata$res) && all(grepl("[126]0", metadata[,"res"]))) {
      metadata[,"res"] <- paste0(metadata[,"res"],"m")
    }
    # retrieve type
    metadata$type <- ifelse(
      !grepl(fs2nc_regex$regex,s2_names), "unrecognised",
      ifelse(
        grepl("[0-9]{2}[A-Z]{3}[a-z]?",metadata$extent_name), "tile",
        ifelse(
          metadata$extent_name=="", "merged", "clipped"
        )
      )
    )
  } else {
    metadata$type <- as.character(metadata$type)
  }

  # manage unrecognised files
  if (sum(metadata$type=="unrecognised") > 0) {
    print_message(
      type = if(abort==TRUE){"error"}else{"warning"},
      "\"",paste(s2_names[metadata$type=="unrecognised"], collapse="\", \""),
      "\" were not recognised."
    )
    metadata[metadata$type=="unrecognised",2:(length(fs2nc_regex$elements)+1)] <- NA
  }

  # return output
  if (format == "data.table") {
    return(data.table(metadata))
  } else if (format == "data.frame") {
    return(metadata)
  } else if (format == "list") {
    meta_list <- lapply(seq_along(s2_names), function(i) {
      l <- as.list(metadata[i,])
      l$sensing_date <- as.character(l$sensing_date)
      l[l==""|is.na(l)] <- NULL
      l
    })
    names(meta_list) <- s2_names
    if (length(meta_list)==1) {
      return(meta_list[[1]])
    } else {
      return(meta_list)
    }

  }

}


#' @title Get metadata from raster paths (reduced version)
#' @description This accessory function extracts some useful metadata from
#'  a vector of raster paths. This is a reduced version of the homonymous
#'  `sen2r::raster_metadata()`, limited to the needs of `{sen2rts}`: it only
#'  supports `format = "list"` and returns the metadata `res`, `bbox` and
#'  `proj` (plus `path` and `valid`).
#' @param raster_paths A vector of raster paths.
#' @param meta Kept for signature compatibility; ignored (metadata
#'  `res`, `bbox` and `proj` are always returned).
#' @param format Only `"list"` is supported.
#' @return A list of the output metadata.
#' @author Luigi Ranghetti, PhD (2019)
#' @note Originally from package {sen2r} (GPL-3); reduced for {sen2rts}.
#' @keywords internal
#' @importFrom stars read_stars st_dimensions
#' @importFrom sf st_bbox gdal_crs
#' @importFrom methods is
#' @importFrom stats setNames

raster_metadata <- function(raster_paths, meta = "all", format = "list") {

  if (format != "list") {
    print_message(
      type = "error",
      "The internal raster_metadata() only supports format = \"list\"."
    )
  }

  out_list <- list()
  for (i in seq_along(raster_paths)) {

    raster_path <- raster_paths[i]
    sel_raster <- suppressWarnings(suppressMessages(try(
      read_stars(raster_path, proxy = TRUE, quiet = TRUE),
      silent = TRUE
    )))
    valid <- !is(sel_raster, "try-error")

    out_list[[i]] <- list(
      "path" = raster_path,
      "valid" = valid
    )

    if (valid) {

      # resolution (x, y)
      ref_res <- sapply(st_dimensions(sel_raster), function(xy){abs(xy$delta)})
      out_list[[i]][["res"]] <- ref_res[1:2]

      # bounding box and projection (with fallback for NA crs)
      ref_bbox <- st_bbox(sel_raster)
      ref_proj <- attr(ref_bbox, "crs")
      if (is.na(ref_proj)) {
        ref_proj <- gdal_crs(raster_path)
        ref_bbox <- st_bbox(
          setNames(as.numeric(ref_bbox), names(ref_bbox)),
          crs = ref_proj
        )
      }
      out_list[[i]][["bbox"]] <- ref_bbox
      out_list[[i]][["proj"]] <- ref_proj

    }

  }

  out_list

}
