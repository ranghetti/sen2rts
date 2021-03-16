# Patch phenopix::PhenoTrs() in order to manage also `data` inputs which
# do not come from fitting procedures (or which are not monotonic).

#' @importFrom zoo index
#' @importFrom phenopix PhenoDeriv PhenoGu PhenoKl

patch_PhenoTrs <- function() {
  
  PhenoTrs_fun <- deparse(phenopix::PhenoTrs)
  
  # Prepare patch lines
  repl1_line <- which(grepl(
    "greenup <- .Greenup(ratio)",
    PhenoTrs_fun, fixed = TRUE
  ))
  patch1 <- c(
    "    greenup <- .Greenup(ratio) & index(ratio) < pop",
    "    senescence <- !.Greenup(ratio) & index(ratio) >= pop"
  )
  repl2_line <- which(grepl(
    "eos <- round(median(soseos[!greenup & bool], na.rm = TRUE))", 
    PhenoTrs_fun, fixed = TRUE
  ))
  patch2 <- "    eos <- round(median(soseos[senescence & bool], na.rm = TRUE))"
  
  # Apply patches if corresponding lines were found
  PhenoTrs_fun_patched <- if (all(
    length(repl1_line) == 1, 
    length(repl2_line) == 1,
    repl1_line < repl2_line
  )) {
    c(
      PhenoTrs_fun[1:(repl1_line-1)],
      patch1,
      PhenoTrs_fun[(repl1_line+1):(repl2_line-1)],
      patch2,
      PhenoTrs_fun[(repl2_line+1):length(PhenoTrs_fun)]
    )
  } else {
    PhenoTrs_fun
  }

  # Return as function
  eval(parse(text = PhenoTrs_fun_patched))
  
}

PhenoTrs <- patch_PhenoTrs()
PhenoExtract  <- eval(parse(text = deparse(phenopix::PhenoExtract)))
