.onAttach <- function(libname, pkgname) {
  message <- "Warning: Package diseq is deprecated. Please use package markets instead."
  packageStartupMessage(message, appendLF = TRUE)

  invisible()
}
