#' @include equation_basic.R

#' @title Directional disequilibrium model equation class
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
  function(.Object, quantity, price, specification, data, name, prefix, separation_subset) {
    .Object <- callNextMethod(.Object, quantity, price, specification, data, name, prefix)
    .Object@separation_subset <- separation_subset
    .Object
  }
)

setMethod(
  "set_parameters", signature(object = "equation_directional"),
  function(object, parameters) {
    object <- callNextMethod(object, parameters)
    object
  }
)
