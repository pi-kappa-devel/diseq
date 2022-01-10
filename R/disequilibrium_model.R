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
  function(.Object,
           model_type_string, verbose,
           specification,
           correlated_shocks,
           data,
           system_initializer) {
    .Object <- callNextMethod(
      .Object,
      model_type_string, verbose,
      specification,
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
#' @details The following methods offer functionality for analyzing estimated
#' shortages in the disequilibrium models. The methods can be called either
#' using directly a fitted model object, or by separately providing a model
#' object and a parameter vector.
#' @param fit A fitted model object.
#' @param model A disequilibrium model object.
#' @param parameters A vector of parameters at which the shortages are evaluated.
#' @return A vector with the (estimated) shortages.
#' @examples
#' \donttest{
#' # estimate a model using the houses dataset
#' fit <- diseq_deterministic_adjustment(
#'   HS | RM | ID | TREND ~
#'   RM + TREND + W + CSHS + L1RM + L2RM + MONTH |
#'   RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
#'   fair_houses(),  correlated_shocks = FALSE,
#'   estimation_options = list(control = list(maxit = 1e+5)))
#'
#' # get estimated normalized shortages
#' head(normalized_shortages(fit))
#'
#' # get estimated relative shortages
#' head(relative_shortages(fit))
#'
#' # get the estimated shortage probabilities
#' head(shortage_probabilities(fit))
#'
#' # get the estimated shortage indicators
#' head(shortage_indicators(fit))
#'
#' # get the estimated shortages
#' head(shortages(fit))
#'
#' # get the estimated shortage variance
#' shortage_standard_deviation(fit)
#' }
#' @name shortage_analysis
NULL

#' @describeIn shortage_analysis Shortages.
#' @details
#' \subsection{shortages}{
#' Returns the predicted shortages at a given point.
#' }
#' @export
setGeneric("shortages", function(fit, model, parameters) {
  standardGeneric("shortages")
})

#' @describeIn shortage_analysis Normalized shortages.
#' @details
#' \subsection{normalized_shortages}{
#' Returns the shortages normalized by the variance of the difference of the shocks at a
#' given point.
#' }
#' @export
setGeneric("normalized_shortages", function(fit, model, parameters) {
  standardGeneric("normalized_shortages")
})

#' @describeIn shortage_analysis Relative shortages.
#' @details
#' \subsection{relative_shortages}{
#' Returns the shortages normalized by the supplied quantity at a given point.
#' }
#' @export
setGeneric("relative_shortages", function(fit, model, parameters) {
  standardGeneric("relative_shortages")
})

#' @describeIn shortage_analysis Shortage probabilities.
#' @details
#' \subsection{shortage_probabilities}{
#' Returns the shortage probabilities, i.e. the probabilities of an
#' observation coming from an excess demand state, at the given point.
#' }
#' @export
setGeneric("shortage_probabilities", function(fit, model, parameters) {
  standardGeneric("shortage_probabilities")
})

#' @describeIn shortage_analysis Shortage indicators.
#' @details
#' \subsection{shortage_indicators}{
#' Returns a vector of indicators (Boolean values) for each observation. An
#' element of the vector is TRUE for observations at which the estimated
#' shortages are non-negative, i.e. the market at in an excess demand state.
#' The remaining elements are FALSE. The evaluation of the shortages is
#' performed using the passed parameter vector.
#' }
#' @export
setGeneric("shortage_indicators", function(fit, model, parameters) {
  standardGeneric("shortage_indicators")
})

#' @describeIn shortage_analysis Shortage variance.
#' @details
#' \subsection{shortage_standard_deviation}{
#' Returns the variance of excess demand.
#' }
#' @export
setGeneric("shortage_standard_deviation", function(fit, model, parameters) {
  standardGeneric("shortage_standard_deviation")
})


#' Marginal effects
#'
#' Returns the estimated effect of a variable.
#' @param fit A fitted disequilibrium market model.
#' @param variable Variable name for which the effect is calculated.
#' @param model A disequilibrium model object.
#' @param parameters A vector of parameters.
#' @param aggregate Mode of aggregation. Valid options are "mean" (the
#' default) and "at_the_mean".
#' @return The estimated effect of the passed variable.
#' @examples
#' \donttest{
#' # estimate a model using the houses dataset
#' fit <- diseq_deterministic_adjustment(
#'   HS | RM | ID | TREND ~
#'   RM + TREND + W + CSHS + L1RM + L2RM + MONTH |
#'   RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
#'   fair_houses(),  correlated_shocks = FALSE,
#'   estimation_options = list(control = list(maxit = 1e+5)))
#'
#' # mean marginal effect of variable "RM" on the shortage probabilities
#' #' shortage_probability_marginal(fit, "RM")
#'
#' # marginal effect at the mean of variable "RM" on the shortage probabilities
#' shortage_probability_marginal(fit, "CSHS", aggregate = "at_the_mean")
#'
#' # marginal effect of variable "RM" on the system
#' shortage_marginal(fit, "RM")
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
setGeneric("shortage_marginal", function(fit, variable, model, parameters) {
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
setGeneric(
  "shortage_probability_marginal",
  function(fit, variable, aggregate = "mean", model, parameters) {
    standardGeneric("shortage_probability_marginal")
  }
)


#' @rdname shortage_analysis
setMethod(
  "shortages", signature(model = "disequilibrium_model", fit = "missing"),
  function(model, parameters) {
    model@system <- set_parameters(model@system, parameters)
    result <- (
      model@system@demand@independent_matrix %*% model@system@demand@alpha_beta -
        model@system@supply@independent_matrix %*% model@system@supply@alpha_beta)
    colnames(result) <- "shortages"
    result
  }
)

#' @rdname shortage_analysis
setMethod(
  "normalized_shortages",
  signature(model = "disequilibrium_model", fit = "missing"),
  function(model, parameters) {
    result <- shortages(
      model = model, parameters = parameters
    ) / shortage_standard_deviation(
      model = model, parameters = parameters
    )
    colnames(result) <- c("normalized_shortages")
    result
  }
)

#' @rdname shortage_analysis
setMethod(
  "relative_shortages",
  signature(model = "disequilibrium_model", fit = "missing"),
  function(model, parameters) {
    model@system <- set_parameters(model@system, parameters)
    demand <- model@system@demand@independent_matrix %*% model@system@demand@alpha_beta
    supply <- model@system@supply@independent_matrix %*% model@system@supply@alpha_beta
    result <- (demand - supply) / supply
    colnames(result) <- "relative_shortages"
    result
  }
)

#' @rdname shortage_analysis
setMethod(
  "shortage_probabilities",
  signature(model = "disequilibrium_model", fit = "missing"),
  function(model, parameters) {
    result <- pnorm(normalized_shortages(
      model = model, parameters = parameters
    ))
    colnames(result) <- "shortage_probabilities"
    result
  }
)

#' @rdname shortage_analysis
setMethod(
  "shortage_indicators",
  signature(model = "disequilibrium_model", fit = "missing"),
  function(model, parameters) {
    result <- shortages(model = model, parameters = parameters) >= 0
    colnames(result) <- "shortage_indicators"
    result
  }
)

#' @rdname shortage_analysis
setMethod(
  "shortage_standard_deviation", signature(
    model = "disequilibrium_model",
    fit = "missing"
  ),
  function(model, parameters) {
    model@system <- set_parameters(model@system, parameters)
    result <- sqrt(
      model@system@demand@var + model@system@supply@var -
        2 * model@system@demand@sigma * model@system@supply@sigma * model@system@rho
    )
    names(result) <- "shortage_standard_deviation"
    result
  }
)

#' @rdname marginal_effects
setMethod(
  "shortage_marginal", signature(
    fit = "missing",
    model = "disequilibrium_model"
  ),
  function(variable, model, parameters) {
    var <- shortage_standard_deviation(model = model, parameters = parameters)
    dname <- paste0(model@system@demand@variable_prefix, variable)
    dvar <- parameters[dname]
    sname <- paste0(model@system@supply@variable_prefix, variable)
    svar <- parameters[sname]
    in_demand <- dname %in% prefixed_independent_variables(model@system@demand)
    in_supply <- sname %in% prefixed_independent_variables(model@system@supply)
    if (in_demand && in_supply) {
      effect <- (dvar - svar) / var
      names(effect) <- paste0("B_", variable)
    } else if (in_demand) {
      effect <- dvar / var
    } else {
      effect <- -svar / var
    }
    effect
  }
)

#' @rdname marginal_effects
setMethod(
  "shortage_probability_marginal", signature(
    fit = "missing",
    model = "disequilibrium_model"
  ),
  function(variable, aggregate, model, parameters) {
    marginal_scale_function <- NULL
    if (aggregate == "mean") {
      marginal_scale_function <- function(x) {
        mean(dnorm(normalized_shortages(model = model, parameters = x)))
      }
    } else if (aggregate == "at_the_mean") {
      marginal_scale_function <- function(x) {
        dnorm(mean(normalized_shortages(model = model, parameters = x)))
      }
    } else {
      allowed_aggergate <- c("mean", "at_the_mean")
      print_error(model@logger, paste0(
        "Invalid `aggregate` option '", aggregate, "'. Valid options are ('",
        paste0(allowed_aggergate, collapse = "', '"), "')."
      ))
    }

    marginal_scale <- marginal_scale_function(parameters)
    shortage_marginal(
      model = model, parameters = parameters,
      variable = variable
    ) * marginal_scale
  }
)


#' @rdname marginal_effects
setMethod(
  "shortage_marginal", signature(
    fit = "missing",
    model = "disequilibrium_model"
  ),
  function(variable, model, parameters) {
    var <- shortage_standard_deviation(model = model, parameters = parameters)
    dname <- paste0(model@system@demand@variable_prefix, variable)
    dvar <- parameters[dname]
    sname <- paste0(model@system@supply@variable_prefix, variable)
    svar <- parameters[sname]
    in_demand <- dname %in% prefixed_independent_variables(model@system@demand)
    in_supply <- sname %in% prefixed_independent_variables(model@system@supply)
    if (in_demand && in_supply) {
      effect <- (dvar - svar) / var
      names(effect) <- paste0("B_", variable)
    } else if (in_demand) {
      effect <- dvar / var
    } else {
      effect <- -svar / var
    }
    effect
  }
)
