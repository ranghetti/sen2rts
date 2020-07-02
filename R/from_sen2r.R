# Internal sen2r functions used in sen2rts as internals

print_message <- function(...) {
  eval(parse(text = "sen2r:::print_message(...)"))
}

