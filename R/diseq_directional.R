#' @include disequilibrium_model.R

#' @title Directional disequilibrium model with sample separation.
#'
#' @description The directional disequilibrium model consists of three equations and a separation
#' rule. The market is described by a linear demand, a linear supply equation and the short side
#' rule. The separation
#' rule splits the sample into regimes of excess supply and excess demand. If a price change is
#' positive at the time point of the observation, then the observation is classified as being in an
#' excess demand regime. Otherwise, it is assumed that it represents an excess supply state. The
#' model is estimated using full information maximum likelihood.
#'
#' \deqn{D_{nt} = X_{d,nt}'\beta_{d} + u_{d,nt},}
#' \deqn{S_{nt} = X_{s,nt}'\beta_{s} + u_{s,nt},}
#' \deqn{Q_{nt} = \min\{D_{nt},S_{nt}\},}
#' \deqn{\Delta P_{nt} \ge 0 \Longrightarrow D_{nt} \ge S_{nt}.}
#'
#' @examples
#' \donttest{
#' simulated_data <- simulate_model_data(
#'   "diseq_directional", 500, 3, # model type, observed entities, observed time points
#'   -0.2, 4.3, c(0.03, 0.02), c(0.03, 0.01), # demand coefficients
#'   0.0, 4.0, c(0.03), c(0.05, 0.02) # supply coefficients
#' )
#'
#' # in the directional model prices cannot be included in both demand and supply
#' model <- new(
#'   "diseq_directional", # model type
#'   c("id", "date"), "date", "Q", "P", # keys, time point, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "Xs1 + X1 + X2", # equation specifications
#'   simulated_data, # data
#'   correlated_shocks = TRUE # allow shocks to be correlated
#' )
#' }
#' @export
setClass(
  "diseq_directional",
  contains = "disequilibrium_model",
  representation(),
  prototype()
)

#' @describeIn initialize_market_model Directional disequilibrium model base constructor
setMethod(
  "initialize", "diseq_directional",
  function(
           .Object,
           key_columns, time_column, quantity_column, price_column,
           demand_specification, supply_specification,
           data,
           correlated_shocks = TRUE, verbose = 0) {
    .Object <- callNextMethod(
      .Object,
      "Directional", verbose,
      key_columns, time_column,
      quantity_column, price_column, demand_specification, supply_specification, NULL,
      correlated_shocks,
      data,
      function(...) new("system_directional", ...)
    )

    # Check for mis-specification
    if (
      price_column %in% .Object@system@demand@independent_variables &&
        price_column %in% .Object@system@supply@independent_variables
    ) {
      print_error(
        .Object@logger,
        "Price cannot be part of both the demand and supply equations here ",
        "(See Maddala, (1974) <https://doi.org/10.2307/1914215>, p1021)"
      )
    }

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
  "minus_log_likelihood", signature(object = "diseq_directional"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)

    loglhd <- sum(
      log(object@system@demand@Psi[object@system@demand@separation_subset] /
        object@system@demand@sigma)
    )
    loglhs <- sum(
      log(object@system@supply@Psi[object@system@supply@separation_subset] /
        object@system@supply@sigma)
    )
    -loglhd - loglhs
  }
)

setMethod("gradient", signature(object = "diseq_directional"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  -colSums(calculate_system_scores(object@system))
})

#' @rdname scores
setMethod("scores", signature(object = "diseq_directional"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  -calculate_system_scores(object@system)
})
