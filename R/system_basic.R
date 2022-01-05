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
  function(.Object, specification, data, correlated_shocks,
           demand_initializer = NULL, supply_initializer = NULL) {
    .Object <- callNextMethod(
      .Object, specification, data, correlated_shocks,
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
  "show_implementation", signature(object = "system_basic"),
  function(object) {
    callNextMethod(object)
    cat(sprintf(
      "  %-18s: %s\n", "Short Side Rule", paste0(
        quantity_variable(object@demand), " = min(",
        prefixed_quantity_variable(object@demand), ", ",
        prefixed_quantity_variable(object@supply), ")"
      )
    ))
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
