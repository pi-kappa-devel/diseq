#' @include equation_base.R

#' @title Basic disequilibrium model equation class
setClass(
  "equation_basic",
  contains = "equation_base",
  representation(
    h = "matrix",
    z = "matrix",
    psi = "matrix",
    Psi = "matrix"
  ),
  prototype(
    h = matrix(NA_real_),
    z = matrix(NA_real_),
    psi = matrix(NA_real_),
    Psi = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "equation_basic",
  function(.Object, quantity, price, specification, data, name, prefix) {
    .Object <- callNextMethod(.Object, quantity, price, specification, data, name, prefix)
    .Object
  }
)

setMethod("set_parameters", signature(object = "equation_basic"), function(object, parameters) {
  object <- callNextMethod(object, parameters)
})
