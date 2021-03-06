#' @include disequilibrium_model.R

#' @describeIn market_models Disequilibrium model with stochastic price dynamics.
#'
#' @description
#' \subsection{diseq_stochastic_adjustment}{
#' The disequilibrium model with stochastic price adjustment is described
#' by a system of four equations. Three of of them form a stochastic linear system of
#' market equations equations coupled with a stochastic price evolution equation. The
#' fourth equation is the short side rule. In contrast to the deterministic counterpart,
#' the model does not impose any separation rule on the sample. It is estimated using
#' full information maximum likelihood.
#'
#' \deqn{D_{nt} = X_{d,nt}'\beta_{d} + P_{nt}\alpha_{d} + u_{d,nt},}
#' \deqn{S_{nt} = X_{s,nt}'\beta_{s} + P_{nt}\alpha_{s} + u_{s,nt},}
#' \deqn{Q_{nt} = \min\{D_{nt},S_{nt}\},}
#' \deqn{\Delta P_{nt} =
#'     \frac{1}{\gamma} \left( D_{nt} - S_{nt} \right) +  X_{p,nt}'\beta_{p} + u_{p,nt}.}
#' }
#' @export
setClass(
  "diseq_stochastic_adjustment",
  contains = "disequilibrium_model",
  representation(),
  prototype()
)

#' @describeIn initialize_market_model Disequilibrium model with stochastic price
#'   adjustment constructor
#' @examples
#' simulated_data <- simulate_data(
#'   # model type, observed entities and time points
#'   "diseq_stochastic_adjustment", 500, 3,
#'   # demand coefficients
#'   -0.1, 9.8, c(0.3, -0.2), c(0.6, 0.1),
#'   # supply coefficients
#'   0.1, 5.1, c(0.9), c(-0.5, 0.2),
#'   # price adjustment coefficient
#'   1.4, 3.1, c(0.8)
#' )
#'
#' # initialize the model
#' model <- new(
#'   "diseq_stochastic_adjustment", # model type
#'   c("id", "date"), "date", "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", "Xp1", # equation specifications
#'   simulated_data, # data
#'   correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' show(model)
setMethod(
  "initialize", "diseq_stochastic_adjustment",
  function(
           .Object,
           key_columns, time_column, quantity_column, price_column,
           demand_specification, supply_specification, price_specification,
           data,
           correlated_shocks = TRUE, verbose = 0) {
    .Object <- callNextMethod(
      .Object,
      "Stochastic Adjustment", verbose,
      key_columns, time_column,
      quantity_column, price_column,
      demand_specification, supply_specification, price_specification,
      correlated_shocks,
      data,
      function(...) new("system_stochastic_adjustment", ...)
    )

    .Object
  }
)

#' @rdname shortage_analysis
setMethod(
  "shortage_standard_deviation", signature(object = "diseq_stochastic_adjustment"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    sqrt(object@system@demand@var + object@system@supply@var -
      2 * object@system@demand@sigma * object@system@supply@sigma * object@system@rho_ds)
  }
)

setMethod(
  "calculate_initializing_values", signature(object = "diseq_stochastic_adjustment"),
  function(object) {
    start <- callNextMethod(object)

    len <- length(start)
    pos <- len - ifelse(object@system@correlated_shocks, 3, 2)
    start <- c(
      start[1:pos],
      object@system@price_equation@linear_model$coefficients, start[(pos + 1):len]
    )

    len <- length(start)
    if (object@system@correlated_shocks) {
      start <- c(start[1:(len - 1)], 1, start[len], 0, 0)
      names(start)[len:length(start)] <- c(
        prefixed_variance_variable(object@system@price_equation),
        paste0(correlation_variable(object@system), c("_DS", "_DP", "_SP"))
      )
    }
    else {
      start <- c(start, price_variance = 1)
      names(start)[len + 1] <- prefixed_variance_variable(object@system@price_equation)
    }

    start
  }
)

#' @rdname minus_log_likelihood
setMethod(
  "minus_log_likelihood", signature(object = "diseq_stochastic_adjustment"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    -sum(calculate_system_loglikelihood(object@system))
  }
)

#' @rdname gradient
setMethod(
  "gradient", signature(object = "diseq_stochastic_adjustment"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    -colSums(calculate_system_scores(object@system))
  }
)

#' @rdname scores
setMethod(
  "scores", signature(object = "diseq_stochastic_adjustment"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    -calculate_system_scores(object@system)
  }
)
