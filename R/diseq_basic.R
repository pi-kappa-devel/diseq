#' @include disequilibrium_model.R

#' @title Basic disequilibrium model with unknown sample separation.
#'
#' @description The basic disequilibrium model consists of three equations. Two of them are
#' the demand and supply equations. In addition, the model replaces the market clearing condition
#' with the short side rule. The model is estimated using full information maximum likelihood.
#'
#' \deqn{D_{nt} = X_{d,nt}'\beta_{d} + u_{d,nt},}
#' \deqn{S_{nt} = X_{s,nt}'\beta_{s} + u_{s,nt},}
#' \deqn{Q_{nt} = \min\{D_{nt},S_{nt}\}.}
#'
#' @examples
#' simulated_data <- simulate_model_data(
#'   "diseq_basic", 500, 3, # model type, observed entities, observed time points
#'   -0.9, 8.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 4.2, c(0.03), c(-0.05, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "diseq_basic", # model type
#'   c("id", "date"), "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'   simulated_data, # data
#'   correlated_shocks = TRUE # allow shocks to be correlated
#' )
#' @export
setClass(
  "diseq_basic",
  contains = "disequilibrium_model",
  representation(),
  prototype()
)

#' @describeIn initialize_market_model Basic disequilibrium model base constructor
setMethod(
  "initialize", "diseq_basic",
  function(.Object,
           key_columns, quantity_column, price_column,
           demand_specification, supply_specification,
           data,
           correlated_shocks = TRUE, verbose = 0) {
    .Object <- callNextMethod(
      .Object,
      "Basic", verbose,
      key_columns, NULL,
      quantity_column, price_column, demand_specification, supply_specification, NULL,
      correlated_shocks,
      data,
      function(...) new("system_basic", ...)
    )

    .Object
  }
)

#' @rdname minus_log_likelihood
setMethod("minus_log_likelihood", signature(object = "diseq_basic"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  -sum(log(object@system@lh))
})

setMethod("gradient", signature(object = "diseq_basic"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  -colSums(calculate_system_scores(object@system))
})

#' @rdname scores
setMethod("scores", signature(object = "diseq_basic"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  -calculate_system_scores(object@system)
})
