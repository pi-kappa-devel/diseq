#' @include market_model.R

#' @describeIn market_models Base class for disequilibrium models
setClass(
  "disequilibrium_model",
  contains = "market_model",
  representation(),
  prototype()
)

setMethod(
  "initialize", "disequilibrium_model",
  function(
           .Object,
           model_type_string, verbose,
           key_columns, time_column,
           quantity_column, price_column,
           demand_specification, supply_specification, price_specification,
           correlated_shocks,
           data,
           system_initializer) {
    .Object <- callNextMethod(
      .Object,
      model_type_string, verbose,
      key_columns, time_column,
      quantity_column, price_column,
      demand_specification, supply_specification, price_specification,
      correlated_shocks,
      data,
      system_initializer
    )
    .Object@market_type_string <- "Disequilibrium"

    .Object
  }
)

#' @title Analysis of shortages
#'
#' @details The following methods offer functionality for analyzing estimated shortages
#' in the disequilibrium models.
#' @param object A disequilibrium model object.
#' @param parameters A vector of parameters at which the shortages are evaluated.
#' @return A vector with the (modified) shortages.
#' @examples
#' \donttest{
#' # initialize the model using the houses dataset
#' model <- new(
#'   "diseq_deterministic_adjustment", # model type
#'   c("ID", "TREND"), "TREND", "HS", "RM", # keys, time, quantity, and price variables
#'   "RM + TREND + W + CSHS + L1RM + L2RM + MONTH", # demand specification
#'   "RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH", # supply specification
#'   fair_houses(), # data
#'   correlated_shocks = FALSE # allow shocks to be correlated
#' )
#'
#' # estimate the model object (BFGS is used by default)
#' est <- estimate(model, control = list(maxit = 1e+5))
#'
#' # get estimated normalized shortages
#' head(normalized_shortages(model, est@coef))
#'
#' # get estimated relative shortages
#' head(relative_shortages(model, est@coef))
#'
#' # get the estimated shortage probabilities
#' head(shortage_probabilities(model, est@coef))
#'
#' # get the estimated shortage indicators
#' head(shortage_indicators(model, est@coef))
#'
#' # get the estimated shortages
#' head(shortages(model, est@coef))
#'
#' # get the estimated shortage variance
#' shortage_standard_deviation(model, est@coef)
#' }
#' @name shortage_analysis
NULL

#' @describeIn shortage_analysis Shortages.
#' @details
#' \subsection{shortages}{
#' Returns the predicted shortages at a given point.
#' }
#' @export
setGeneric("shortages", function(object, parameters) {
  standardGeneric("shortages")
})

#' @describeIn shortage_analysis Normalized shortages.
#' @details
#' \subsection{normalized_shortages}{
#' Returns the shortages normalized by the variance of the difference of the shocks at a
#' given point.
#' }
#' @export
setGeneric("normalized_shortages", function(object, parameters) {
  standardGeneric("normalized_shortages")
})

#' @describeIn shortage_analysis Relative shortages.
#' @details
#' \subsection{relative_shortages}{
#' Returns the shortages normalized by the supplied quantity at a given point.
#' }
#' @export
setGeneric("relative_shortages", function(object, parameters) {
  standardGeneric("relative_shortages")
})

#' @describeIn shortage_analysis Shortage probabilities.
#' @details
#' \subsection{shortage_probabilities}{
#' Returns the shortage probabilities, i.e. the probabilities of an
#' observation coming from an excess demand regime, at the given point.
#' }
#' @export
setGeneric("shortage_probabilities", function(object, parameters) {
  standardGeneric("shortage_probabilities")
})

#' @describeIn shortage_analysis Shortage indicators.
#' @details
#' \subsection{shortage_indicators}{
#' Returns a vector of indicators (Boolean values) for each observation. An element of
#' the vector is TRUE for observations at which the estimated shortages are
#' non-negative, i.e. the market at in an excess demand state. The remaining elements
#' are FALSE. The evaluation of the shortages is performed using the passed parameter
#' vector.
#' }
#' @export
setGeneric("shortage_indicators", function(object, parameters) {
  standardGeneric("shortage_indicators")
})

#' @describeIn shortage_analysis Shortage variance.
#' @details
#' \subsection{shortage_standard_deviation}{
#' Returns the variance of excess demand.
#' }
#' @export
setGeneric("shortage_standard_deviation", function(object, parameters) {
  standardGeneric("shortage_standard_deviation")
})


#' Marginal effects
#'
#' Returns the estimated effect of a variable.
#' @param object A disequilibrium model object.
#' @param parameters A vector of parameters.
#' @param variable Variable name for which the effect is calculated.
#' @param aggregate Mode of aggregation. Valid options are "mean" (the default) and
#' "at_the_mean".
#' @return The estimated effect of the passed variable.
#' @examples
#' \donttest{
#' # initialize the model using the houses dataset
#' model <- new(
#'   "diseq_deterministic_adjustment", # model type
#'   c("ID", "TREND"), "TREND", "HS", "RM", # keys, time, quantity, and price variables
#'   "RM + TREND + W + CSHS + L1RM + L2RM + MONTH", # demand specification
#'   "RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH", # supply specification
#'   fair_houses(), # data
#'   correlated_shocks = FALSE # allow shocks to be correlated
#' )
#'
#' # estimate the model object (BFGS is used by default)
#' est <- estimate(model, control = list(maxit = 1e+5))
#'
#' # get the mean marginal effect of variable "RM" on the shortage probabilities
#' shortage_probability_marginal(model, est@coef, "RM")
#'
#' # get the marginal effect at the mean of variable "RM" on the shortage probabilities
#' shortage_probability_marginal(model, est@coef, "CSHS", aggregate = "at_the_mean")
#'
#' # get the marginal effect of variable "RM" on the system
#' shortage_marginal(model, est@coef, "RM")
#' }
#' @name marginal_effects
NULL

#' @describeIn marginal_effects Marginal effect on market system
#'
#' Returns the estimated marginal effect of a variable on the market system. For a
#' system variable \eqn{x} with demand coefficient \eqn{\beta_{d, x}} and supply
#' coefficient \eqn{\beta_{s, x}}, the marginal effect on the market system  is given by
#' \deqn{M_{x} = \frac{\beta_{d, x} - \beta_{s, x}}{\sqrt{\sigma_{d, x}^{2} +
#' \sigma_{s, x}^{2} - 2 \rho_{ds} \sigma_{d, x} \sigma_{s, x}}}.}
#' @export
setGeneric("shortage_marginal", function(object, parameters, variable) {
  standardGeneric("shortage_marginal")
})

#' @describeIn marginal_effects Marginal effect on shortage probabilities
#'
#' Returns the estimated marginal effect of a variable on the probability of observing
#' a shortage state. The mean marginal effect on the shortage probability is given by
#' \deqn{M_{x} \mathrm{E}\phi(D - S)}
#' and the marginal effect at the mean by
#' \deqn{M_{x} \phi(\mathrm{E}(D - S)),}
#' where \eqn{M_{x}} is the marginal effect on the system, \eqn{D} is the demanded
#' quantity, \eqn{S} the supplied quantity, and \eqn{\phi} is the standard normal
#' density.
#' @export
setGeneric("shortage_probability_marginal",
           function(object, parameters, variable, aggregate = "mean") {
  standardGeneric("shortage_probability_marginal")
})


#' @rdname shortage_analysis
setMethod(
  "shortages", signature(object = "disequilibrium_model"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    (object@system@demand@independent_matrix %*% object@system@demand@alpha_beta -
     object@system@supply@independent_matrix %*% object@system@supply@alpha_beta)
  }
)

#' @rdname shortage_analysis
setMethod(
  "normalized_shortages", signature(object = "disequilibrium_model"),
  function(object, parameters) {
    shortages(object, parameters) / shortage_standard_deviation(object, parameters)
  }
)

#' @rdname shortage_analysis
setMethod(
  "relative_shortages", signature(object = "disequilibrium_model"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    demand <- object@system@demand@independent_matrix %*% object@system@demand@alpha_beta
    supply <- object@system@supply@independent_matrix %*% object@system@supply@alpha_beta
    (demand - supply) / supply
  }
)

#' @rdname shortage_analysis
setMethod(
  "shortage_probabilities", signature(object = "disequilibrium_model"),
  function(object, parameters) {
    pnorm(normalized_shortages(object, parameters))
  }
)

#' @rdname shortage_analysis
setMethod(
  "shortage_indicators", signature(object = "disequilibrium_model"),
  function(object, parameters) {
    shortages(object, parameters) >= 0
  }
)

#' @rdname shortage_analysis
setMethod(
  "shortage_standard_deviation", signature(object = "disequilibrium_model"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    sqrt(object@system@demand@var + object@system@supply@var -
      2 * object@system@demand@sigma * object@system@supply@sigma * object@system@rho)
  }
)

#' @rdname marginal_effects
setMethod(
  "shortage_marginal", signature(object = "disequilibrium_model"),
  function(object, parameters, variable) {
    var <- shortage_standard_deviation(object, parameters)
    dname <- paste0(object@system@demand@variable_prefix, variable)
    dvar <- parameters[dname]
    sname <- paste0(object@system@supply@variable_prefix, variable)
    svar <- parameters[sname]
    in_demand <- dname %in% prefixed_independent_variables(object@system@demand)
    in_supply <- sname %in% prefixed_independent_variables(object@system@supply)
    if (in_demand && in_supply) {
      effect <- (dvar - svar) / var
      names(effect) <- paste0("B_", variable)
    }
    else if (in_demand) {
      effect <- dvar / var
    }
    else {
      effect <- -svar / var
    }
    effect
  }
)

#' @rdname marginal_effects
setMethod(
  "shortage_probability_marginal", signature(object = "disequilibrium_model"),
  function(object, parameters, variable, aggregate) {
    marginal_scale_function <- NULL
    if (aggregate == "mean") {
      marginal_scale_function <- function(x) mean(dnorm(normalized_shortages(object, x)))
    } else if (aggregate == "at_the_mean") {
      marginal_scale_function <- function(x) dnorm(mean(normalized_shortages(object, x)))
    } else {
      allowed_aggergate <- c("mean", "at_the_mean")
      print_error(object@logger, paste0(
        "Invalid `aggregate` option '", aggregate, "'. Valid options are ('",
        paste0(allowed_aggergate, collapse = "', '"), "')."))
    }

    marginal_scale <- marginal_scale_function(parameters)
    shortage_marginal(object, parameters, variable) * marginal_scale
  }
)
