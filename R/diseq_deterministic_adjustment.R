#' @include diseq_base.R
#' @include derivatives_deterministic_adjustment.R

#' @title Disequilibrium model with deterministic price dynamics.
#'
#' @description The disequilibrium model with deterministic price adjustment consists of four
#' equations. The two market equations, the short side rule and  price evolution equation. The first
#' two equations are stochastic. The price equation is deterministic. The sample is separated
#' based on the sign of the price changes as in the \code{\linkS4class{diseq_directional}} model.
#' The model is estimated using full information maximum likelihood.
#'
#' \deqn{D_{nt} = X_{d,nt}'\beta_{d} + P_{nt}\alpha_{d} + u_{d,nt},}
#' \deqn{S_{nt} = X_{s,nt}'\beta_{s} + P_{nt}\alpha_{s} + u_{s,nt},}
#' \deqn{Q_{nt} = \min\{D_{nt},S_{nt}\},}
#' \deqn{\Delta P_{nt} = \frac{1}{\gamma} \left( D_{nt} - S_{nt} \right).}
#'
#' @examples
#' simulated_data <- simulate_model_data(
#'   "diseq_deterministic_adjustment", 500, 3, # model type, observed entities and time points
#'   -0.9, 8.9, c(0.03, -0.02), c(-0.03, -0.01), # demand coefficients
#'   0.9, 4.2, c(0.03), c(0.05, 0.02), # supply coefficients
#'   1.4 # price adjustment coefficient
#' )
#'
#' # initialize the model
#' model <- new(
#'   "diseq_deterministic_adjustment", # model type
#'   c("id", "date"), "date", "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'   simulated_data, # data
#'   use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#' @export
setClass(
  "diseq_deterministic_adjustment",
  contains = "diseq_base",
  representation(),
  prototype()
)

#' @describeIn initialize_model_base Disequilibrium model with deterministic price
#'   adjustment constructor
setMethod(
  "initialize", "diseq_deterministic_adjustment",
  function(
           .Object,
           key_columns, time_column, quantity_column, price_column,
           demand_specification, supply_specification,
           data,
           use_correlated_shocks = TRUE, verbose = 0) {
    .Object <- callNextMethod(
      .Object,
      "Deterministic Adjustment", verbose,
      key_columns, time_column,
      quantity_column, price_column, demand_specification, supply_specification, NULL,
      use_correlated_shocks,
      data,
      function(...) new("system_deterministic_adjustment", ...)
    )

    print_info(
      .Object@logger,
      "Sample separated with ", sum(.Object@system@demand@separation_subset),
      " rows in excess supply and ",
      sum(.Object@system@supply@separation_subset), " in excess demand regime."
    )

    .Object
  }
)

#' @rdname minus_log_likelihood
setMethod(
  "minus_log_likelihood", signature(object = "diseq_deterministic_adjustment"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    object@system <- calculate_system_loglikelihood(object@system)
    -sum(object@system@log_likelihood)
  }
)

setMethod(
  "gradient", signature(object = "diseq_deterministic_adjustment"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    object@system <- calculate_system_gradient(object@system)
    -object@system@gradient
  }
)
