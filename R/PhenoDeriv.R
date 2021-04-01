# Patch phenopix::PhenoDeriv() in order to allow also the retrieval of
# derivatives different from the maximum one.

patch_PhenoDeriv <- function() {
  
  PhenoDeriv_fun <- deparse(phenopix::PhenoDeriv)
  
  # Prepare patch lines
  repl1_line <- which(grepl(
    "formula ?= ?NULL",
    PhenoDeriv_fun,
  ))
  patch1 <- gsub(
    "formula ?= ?NULL",
    "formula = NULL, trs = 1, concavity = \"up\"",
    PhenoDeriv_fun[repl1_line]
  )
  repl2_linebefore <- which(grepl(
    "xd ?<- ?c\\(NA, ?diff\\(x\\)\\)", 
    PhenoDeriv_fun
  ))
  patch2 <- c(
    "    xdr <- rep(NA, length(xd))",
    "    xdr[which(xd>=0)] <- xd[which(xd>=0)]/max(xd[which(xd>=0)],na.rm=TRUE)",
    "    xdr[which(xd<0)] <- -xd[which(xd<0)]/min(xd[which(xd<0)],na.rm=TRUE)",
    "    xd2 <- c(NA, diff(xd))",
    "    if (concavity == 'down') {xd2 <- -xd2}"
  )
  repl3_line <- which(grepl(
    "rsp ?<- ?max\\(xd, ?na.rm ?= ?TRUE\\)", 
    PhenoDeriv_fun
  ))
  patch3 <- c(
    "    rsp0 <- xd[which(xdr >= trs & xd2 >= 0)]",
    "    rsp <- if (length(rsp0) > 0) {rsp0[1]} else {max(xd[which(xd>0)], na.rm = TRUE)}"
  )
  repl4_line <- which(grepl(
    "rau ?<- ?min\\(xd, ?na.rm ?= ?TRUE\\)", 
    PhenoDeriv_fun
  ))
  patch4 <- c(
    "    rau0 <- xd[which(xdr <= -trs & xd2 >= 0)]",
    "    rau <- if (length(rau0) > 0) {rev(rau0)[1]} else {min(xd[which(xd<0)], na.rm = TRUE)}"
  )
  repl5_line <- which(grepl(
    "if ?\\(sos ?< ?eos\\) ?\\{", 
    PhenoDeriv_fun
  ))
  patch5 <- c(
    "    if (anyNA(c(sos,eos))) {",
    "      mgs <- NA",
    "    } else if (sos < eos) {"
  )
  
  # Apply patches if corresponding lines were found
  PhenoDeriv_fun_patched <- if (all(
    length(repl1_line) == 1, 
    length(repl2_linebefore) == 1,
    length(repl3_line) == 1,
    length(repl4_line) == 1,
    length(repl5_line) == 1,
    repl1_line == 1, 
    repl1_line < repl2_linebefore,
    repl2_linebefore < repl3_line,
    repl3_line == repl4_line - 1,
    repl4_line < repl5_line
  )) {
    c(
      patch1,
      PhenoDeriv_fun[(repl1_line+1):(repl2_linebefore)],
      patch2,
      PhenoDeriv_fun[(repl2_linebefore+1):(repl3_line-1)],
      patch3,
      patch4,
      PhenoDeriv_fun[(repl4_line+1):(repl5_line-1)],
      patch5,
      PhenoDeriv_fun[(repl5_line+1):length(PhenoDeriv_fun)]
    )
  } else {
    PhenoDeriv_fun
  }
  
  # Return as function
  eval(parse(text = PhenoDeriv_fun_patched))
  
}

PhenoDeriv <- patch_PhenoDeriv()
