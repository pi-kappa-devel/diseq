#' @include equation_basic.R
#' @include system_base.R
#' @importFrom stats dnorm pnorm

#' @describeIn system_classes Basic model's system class
#' @slot lh Likelihood values for each observation.
setClass(
  "system_basic",
  contains = "system_base",
  representation(
    lh = "matrix"
  ),
  prototype(
    lh = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "system_basic",
  function(
           .Object, quantity, price,
           demand_specification, supply_specification, data, correlated_shocks,
           demand_initializer = NULL, supply_initializer = NULL) {
    .Object <- callNextMethod(
      .Object, quantity, price, demand_specification, supply_specification, data, correlated_shocks,
      ifelse(is.null(demand_initializer),
        function(...) new("equation_basic", ...), demand_initializer
      ),
      ifelse(is.null(supply_initializer),
        function(...) new("equation_basic", ...), supply_initializer
      )
    )
  }
)

setMethod(
  "set_parameters", signature(object = "system_basic"),
  function(object, parameters) {
    object <- callNextMethod(object, parameters)
    object <- calculate_system_moments(object)
    object@lh <- calculate_system_likelihood(object)
    object
  }
)
