.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("hadex_use_interactive_plots")))
    options(hadex_use_interactive_plots = FALSE)
}