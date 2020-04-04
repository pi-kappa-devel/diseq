#' Logger class
#'
#' @slot verbosity Controls the intensity of output messages. Errors are always printed.
#' Other than this, a value of
#' \describe{
#'  \item{1}{prints warnings,}
#'  \item{2}{prints basic information,}
#'  \item{3}{prints verbose information and,}
#'  \item{4}{prints debug information.}
#' }

setClass(
  "model_logger",
  representation(
    verbosity = "numeric"
  ),
  prototype(
    verbosity = 0
  )
)

setMethod(
  "initialize", "model_logger",
  function(.Object, verbosity) {
    .Object@verbosity <- verbosity
    .Object
  }
)

setGeneric("print_error", function(object, ...) {
  standardGeneric("print_error")
})

setGeneric("print_warning", function(object, ...) {
  standardGeneric("print_warning")
})

setGeneric("print_info", function(object, ...) {
  standardGeneric("print_info")
})

setGeneric("print_verbose", function(object, ...) {
  standardGeneric("print_verbose")
})

setGeneric("print_debug", function(object, ...) {
  standardGeneric("print_debug")
})

setMethod("print_error", signature(object = "model_logger"), function(object, ...) {
  stop(...)
  object
})

setMethod("print_warning", signature(object = "model_logger"), function(object, ...) {
  ate <- ifelse(rlang::is_installed("knitr") && length(knitr::opts_chunk$get()) > 0, "\n", "")
  if (object@verbosity > 0) warning(..., ate, immediate. = TRUE, call. = FALSE)
  object
})

setMethod("print_info", signature(object = "model_logger"), function(object, ...) {
  if (object@verbosity > 1) cat("Info: ", ..., "\n", sep = "")
  object
})

setMethod("print_verbose", signature(object = "model_logger"), function(object, ...) {
  if (object@verbosity > 2) cat("Verbose: ", ..., "\n", sep = "")
  object
})

setMethod("print_debug", signature(object = "model_logger"), function(object, ...) {
  if (object@verbosity > 3) cat("Debug: ", ..., "\n", sep = "")
  object
})
