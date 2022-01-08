#' @include disequilibrium_model.R

#' @describeIn market_models Basic disequilibrium model with unknown sample
#' separation.
#'
#' @description
#' \subsection{diseq_basic}{
#' The basic disequilibrium model consists of three equations. Two of them
#' are the demand and supply equations. In addition, the model replaces the
#' market clearing condition with the short side rule. The model is estimated
#' using full information maximum likelihood.
#'
#' \deqn{D_{nt} = X_{d,nt}'\beta_{d} + u_{d,nt},}
#' \deqn{S_{nt} = X_{s,nt}'\beta_{s} + u_{s,nt},}
#' \deqn{Q_{nt} = \min\{D_{nt},S_{nt}\}.}
#' }
#' @export
setClass(
  "diseq_basic",
  contains = "disequilibrium_model",
  representation(),
  prototype()
)

#' @describeIn initialize_market_model Basic disequilibrium model base
#' constructor
#' @examples
#' simulated_data <- simulate_data(
#'   "diseq_basic", 500, 3, # model type, observed entities, observed time points
#'   -0.9, 8.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 4.2, c(0.03), c(-0.05, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "diseq_basic", # model type
#'   subject = id, time = date, quantity = Q, price = P,
#'   demand = P + Xd1 + Xd2 + X1 + X2, supply = P + Xs1 + X1 + X2,
#'   simulated_data, # data
#'   correlated_shocks = FALSE # use independent shocks
#' )
#'
#' show(model)
setMethod(
  "initialize", "diseq_basic",
  function(.Object,
           quantity, price, demand, supply, subject, time,
           data, correlated_shocks = TRUE, verbose = 0) {
    specification <- make_specification(
      data, quantity, price, demand, supply, subject, time
    )
    .Object <- callNextMethod(
      .Object,
      "Basic", verbose,
      specification,
      correlated_shocks,
      data,
      function(...) new("system_basic", ...)
    )
    .Object
  }
)

#' @describeIn single_call_estimation Basic disequilibrium model.
#' @export
setGeneric(
  "diseq_basic",
  function(specification, data,
           correlated_shocks = TRUE, verbose = 0,
           estimation_options = list()) {
    standardGeneric("diseq_basic")
  }
)

#' @rdname single_call_estimation
setMethod(
  "diseq_basic", signature(specification = "formula"),
  function(specification, data, correlated_shocks, verbose,
           estimation_options) {
    initialize_from_formula(
      "diseq_basic", specification, data, correlated_shocks, verbose,
      estimation_options
    )
  }
)

#' @rdname minus_log_likelihood
setMethod(
  "minus_log_likelihood", signature(object = "diseq_basic"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    -sum(log(object@system@lh))
  }
)

#' @rdname gradient
setMethod("gradient", signature(object = "diseq_basic"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  -colSums(calculate_system_scores(object@system))
})

#' @rdname scores
setMethod("scores", signature(object = "diseq_basic"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  -calculate_system_scores(object@system)
})
