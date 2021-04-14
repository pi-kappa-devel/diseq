#' @include equation_basic.R
#' @include system_base.R
setClass(
  "system_equilibrium",
  contains = "system_base",
  representation(
    delta = "numeric",

    mu_P = "matrix",
    var_P = "numeric",
    sigma_P = "numeric",
    h_P = "matrix",

    mu_Q = "matrix",
    var_Q = "numeric",
    sigma_Q = "numeric",
    h_Q = "matrix",

    rho_QP = "numeric",
    rho_1QP = "numeric",
    rho_2QP = "numeric",

    z_QP = "matrix",
    z_PQ = "matrix",

    llh = "matrix"
  ),
  prototype(
    llh = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "system_equilibrium",
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

setMethod("set_parameters", signature(object = "system_equilibrium"),
          function(object, parameters) {
  object <- callNextMethod(object, parameters)

  object@delta <- object@supply@alpha - object@demand@alpha
  object <- calculate_system_moments(object)
  object@llh <- calculate_system_loglikelihood(object)

  object
})
