#' @include equation_base.R

#' @describeIn equation_classes Stochastic adjustment disequilibrium model equation class
setClass(
  "equation_stochastic_adjustment",
  contains = "equation_base",
  representation()
)

setMethod(
  "initialize", "equation_stochastic_adjustment",
  function(.Object, quantity, price, specification, data, name, prefix) {
    specification <- Formula(formula(paste0(
      quantity, " ~ ", specification,
      " | ", price
    )))
    .Object <- callNextMethod(.Object, specification, data, name, prefix)
    .Object
  }
)

setMethod(
  "set_parameters", signature(object = "equation_stochastic_adjustment"),
  function(object, parameters) {
    object <- callNextMethod(object, parameters)
    object
  }
)
