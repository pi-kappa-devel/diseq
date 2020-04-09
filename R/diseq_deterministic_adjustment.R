#' @include diseq_base.R
#' @include derivatives_deterministic_adjustment.R

#' @title Disequilibrium model with deterministic price dynamics.
#'
#' @description The disequilibrium model with deterministic price adjustsmenst consists of four
#' equations. The two market euations, the short side rule and  price evolution equation. The first
#' two equations are stochastic. The price equation is deterministic. The sample is seperated
#' based on the sign of the price changes as in the \code{\linkS4class{diseq_directional}} model.
#' The model is estimated using full information maximum likelihood.
#'
#' \deqn{
#'   \begin{aligned}
#'   D_{nt} &= X_{d,nt}'\beta_{d} + P_{nt}\alpha_{d} + u_{d,nt}, \\
#'   S_{nt} &= X_{s,nt}'\beta_{s} + P_{nt}\alpha_{s} + u_{s,nt}, \\
#'   Q_{nt} &= \min\{D_{nt},S_{nt}\}, \\
#'   \Delta P_{nt} &= \frac{1}{\gamma} \left( D_{nt} - S_{nt} \right).
#'   \end{aligned}
#' }
#'
#' @seealso \code{\link{initialize_model}}
#' @export
setClass(
  "diseq_deterministic_adjustment",
  contains = "diseq_base",
  representation(),
  prototype()
)

#' @describeIn initialize_model Disequilibrium model with deterministic price
#'   adjustment constructor
#' @export
setMethod(
  "initialize", "diseq_deterministic_adjustment",
  function(
           .Object,
           verbose = 0,
           key_columns, time_column,
           quantity_column, price_column, demand_specification, supply_specification,
           use_correlated_shocks = TRUE,
           data) {
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
