#' @include market_model.R
#' @importFrom systemfit systemfit

#' @describeIn market_models Equilibrium model
#'
#' @description
#' \subsection{equilibrium_model}{
#' The equilibrium model consists of thee equations. The demand, the
#' supply and the market clearing equations. The model can be estimated using both full
#' information maximum likelihood and two-stage least squares.
#'
#' \deqn{D_{nt} = X_{d,nt}'\beta_{d} + P_{nt}\alpha_{d} + u_{d,nt},}
#' \deqn{S_{nt} = X_{s,nt}'\beta_{s} + P_{nt}\alpha_{s} + u_{s,nt},}
#' \deqn{Q_{nt} = D_{nt} = S_{nt}.}
#'
#' A necessary identification condition is that
#' there is at least one control that is exclusively part of the demand and one control
#' that is exclusively part of the supply equation. In the first stage of the two-stage
#' least square estimation, prices are regressed on remaining controls from both the
#' demand and supply equations. In the second stage, the demand and supply equation is
#' estimated using the fitted prices instead of the observed.
#' }
#' @export
setClass(
  "equilibrium_model",
  contains = "market_model",
  representation()
)

#' @describeIn initialize_market_model Equilibrium model constructor
#' @examples
#' simulated_data <- simulate_data(
#'   "equilibrium_model", 500, 3, # model type, observed entities and time points
#'   -0.9, 14.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 3.2, c(0.3), c(0.5, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "equilibrium_model", # model type
#'   subject = id, time = date, quantity = Q, price = P,
#'   demand = P + Xd1 + Xd2 + X1 + X2, supply = P + Xs1 + X1 + X2,
#'   simulated_data, # data
#'   correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' show(model)
setMethod(
  "initialize", "equilibrium_model",
  function(.Object,
           quantity, price, demand, supply, subject, time,
           data, correlated_shocks = TRUE, verbose = 0) {
    specification <- make_specification(
      data, quantity, price, demand, supply, subject, time
    )
    .Object <- callNextMethod(
      .Object,
      "Equilibrium", verbose,
      specification,
      correlated_shocks,
      data,
      function(...) new("system_equilibrium", ...)
    )
    .Object@market_type_string <- "Equilibrium"

    .Object
  }
)

#' @describeIn single_call_estimation Equilibrium model
#' @export
setGeneric(
  "equilibrium_model",
  function(specification, data,
           correlated_shocks = TRUE, verbose = 0,
           estimation_options = list()) {
    standardGeneric("equilibrium_model")
  }
)

#' @rdname single_call_estimation
setMethod(
  "equilibrium_model", signature(specification = "formula"),
  function(specification, data, correlated_shocks, verbose,
           estimation_options) {
    initialize_from_formula(
      "equilibrium_model", specification, data, correlated_shocks, verbose,
      estimation_options
    )
  }
)

#' @rdname minus_log_likelihood
setMethod(
  "minus_log_likelihood", signature(object = "equilibrium_model"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    -sum(object@system@llh)
  }
)


#' @rdname gradient
setMethod(
  "gradient", signature(object = "equilibrium_model"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    g <- colSums(calculate_system_scores(object@system))

    as.matrix(-g)
  }
)

#' @rdname scores
setMethod(
  "scores", signature(object = "equilibrium_model"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    -calculate_system_scores(object@system)
  }
)

#' @rdname maximize_log_likelihood
setMethod(
  "maximize_log_likelihood", signature(object = "equilibrium_model"),
  function(object, start, step, objective_tolerance, gradient_tolerance,
           max_it) {
    start <- prepare_initializing_values(object, NULL)

    cpp_model <- new(cpp_equilibrium_model, object@system)
    cpp_model$minimize(
      start, step, objective_tolerance,
      gradient_tolerance, max_it
    )
  }
)

setMethod(
  "calculate_initializing_values", signature(object = "equilibrium_model"),
  function(object) {
    coef(estimate(object, method = "2SLS"))
  }
)
