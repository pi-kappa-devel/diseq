#' @include equation_basic.R

#' @describeIn equation_classes Directional disequilibrium model equation class
#'
#' @slot separation_subset A vector of indicators specifying the observations of the
#' sample described by this equation according to the separation rule of the model.
setClass(
  "equation_directional",
  contains = "equation_basic",
  representation(
    separation_subset = "vector"
  ),
  prototype(
    separation_subset = NULL
  )
)

setMethod(
  "initialize", "equation_directional",
  function(.Object, specification, data, name, prefix, separation_subset) {
    .Object <- callNextMethod(.Object, specification, data, name, prefix)
    .Object@separation_subset <- separation_subset
    .Object
  }
)
