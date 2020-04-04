#' @include equation_basic.R
#' @include system_base.R
#' @importFrom stats dnorm pnorm
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

setGeneric("calculate_psi", function(object, equation_i, equation_j) {
  standardGeneric("calculate_psi")
})

setGeneric("calculate_Psi", function(object, equation_i, equation_j) {
  standardGeneric("calculate_Psi")
})

setGeneric("calculate_lh", function(object) {
  standardGeneric("calculate_lh")
})

setMethod(
  "calculate_psi",
  signature(object = "system_basic", equation_i = "equation_basic", equation_j = "equation_basic"),
  function(object, equation_i, equation_j) {
    dnorm(equation_i@h) * dnorm(equation_j@z)
  }
)

setMethod(
  "calculate_Psi",
  signature(object = "system_basic", equation_i = "equation_basic", equation_j = "equation_basic"),
  function(object, equation_i, equation_j) {
    dnorm(equation_i@h) * pnorm(equation_j@z, lower.tail = FALSE)
  }
)

setMethod("calculate_lh", signature(object = "system_basic"), function(object) {
  object@demand@Psi / object@demand@sigma + object@supply@Psi / object@supply@sigma
})

setMethod("set_parameters", signature(object = "system_basic"), function(object, parameters) {
  object <- callNextMethod(object, parameters)
  object@demand@h <- calculate_h(object, object@demand)
  object@supply@h <- calculate_h(object, object@supply)
  object@demand@z <- calculate_z(object, object@demand, object@supply)
  object@supply@z <- calculate_z(object, object@supply, object@demand)
  object@demand@psi <- calculate_psi(object, object@demand, object@supply)
  object@supply@psi <- calculate_psi(object, object@supply, object@demand)
  object@demand@Psi <- calculate_Psi(object, object@demand, object@supply)
  object@supply@Psi <- calculate_Psi(object, object@supply, object@demand)
  object@lh <- calculate_lh(object)
  object
})
