#' @include model_base.R
setClass(
  "diseq_base",
  contains = "model_base",
  representation(),
  prototype()
)

setMethod("initialize", "diseq_base", function(
  .Object,
  model_type_string, verbose,
  key_columns, time_column,
  quantity_column, price_column, demand_specification, supply_specification, price_specification,
  use_correlated_shocks,
  data,
  system_initializer
) {

  .Object <- callNextMethod(
    .Object,
    model_type_string, verbose,
    key_columns, time_column,
    quantity_column, price_column, demand_specification, supply_specification, price_specification,
    use_correlated_shocks,
    data,
    system_initializer
  )
})

setGeneric("calculate_h", function(object, equation) {
  standardGeneric("calculate_h")
})

setGeneric("calculate_z", function(object, equation_i, equation_j) {
  standardGeneric("calculate_z")
})

setGeneric("get_shortage_variance", function(object) {
  standardGeneric("get_shortage_variance")
})

#' Normalized shortages.
#'
#' Returns the estimated shortages normalized by the variance of the difference of the shocks.
#' @param object A disequilibrium model object.
#' @param parameters A vector of parameters at which the shortages are evaluated.
#' @rdname get_normalized_shortages
#' @export
setGeneric("get_normalized_shortages", function(object, parameters) {
  standardGeneric("get_normalized_shortages")
})

#' Relative shortages.
#'
#' Returns the estimated shortages normalized by the estimated supplied quantity.
#' @param object A disequilibrium model object.
#' @param parameters A vector of parameters at which the shortages are evaluated.
#' @rdname get_relative_shortages
#' @export
setGeneric("get_relative_shortages", function(object, parameters) {
  standardGeneric("get_relative_shortages")
})

#' Shortage probabilities.
#'
#' Returns the estimated shortage probabilities.
#' @param object A disequilibrium model object.
#' @param parameters A vector of parameters at which the shortage probabilities are evaluated.
#' @rdname get_shortage_probabilities
#' @export
setGeneric("get_shortage_probabilities", function(object, parameters) {
  standardGeneric("get_shortage_probabilities")
})

setGeneric("get_scaled_effect", function(object, estimation, variable, scale_function) {
  standardGeneric("get_scaled_effect")
})

#' Mean marginal effects
#'
#' Returns the average estimated marginal effect.
#' @param object A disequilibrium model object.
#' @param estimation A model estimation object (i.e. a \code{\link[bbmle]{mle2}} object).
#' @param variable Variable name for which the effect is calculated.
#' @rdname get_mean_marginal_effect
#' @export
setGeneric("get_mean_marginal_effect", function(object, estimation, variable) {
  standardGeneric("get_mean_marginal_effect")
})

#' Marginal effects at the mean.
#'
#' Returns the estimated marginal effects evaluated at the mean.
#' @param object A disequilibrium model object.
#' @param estimation A model estimation object (i.e. a \code{\link[bbmle]{mle2}} object).
#' @param variable Variable name for which the effect is calculated.
#' @rdname get_marginal_effect_at_mean
#' @export
setGeneric("get_marginal_effect_at_mean", function(object, estimation, variable) {
  standardGeneric("get_marginal_effect_at_mean")
})

setGeneric("gradient", function(object, parameters) {
  standardGeneric("gradient")
})

setGeneric("hessian", function(object, parameters) {
  standardGeneric("hessian")
})

#' Indices of estimated shortages.
#'
#' Returns the indices for which the estimated shortages of the market are non-negative.
#' @param object A disequilibrium model object.
#' @param estimation A model estimation object (i.e. a \code{\link[bbmle]{mle2}} object).
#' @rdname get_estimated_shortage_indices
#' @export
setGeneric("get_estimated_shortage_indices", function(object, estimation) {
  standardGeneric("get_estimated_shortage_indices")
})

setMethod("calculate_h", signature(object = "system_base", equation = "equation_base"),
  function(object, equation) {
    (object@quantity_vector - equation@independent_matrix %*% equation@alpha_beta) / equation@sigma
  }
)

setMethod("calculate_z",
  signature(object = "system_base", equation_i = "equation_base", equation_j = "equation_base"),
  function(object, equation_i, equation_j) {
    (equation_i@h - equation_j@h * object@rho) * object@rho1
  }
)

#' @describeIn estimate Disequilibrium model estimation.
#' @param use_numerical_hessian If true, the variance-covariance matrix is calculated using the numerically
#' approximated Hessian. Calculated Hessians are available for the basic and directional models.
setMethod("estimate", signature(object = "diseq_base"), function(object, use_numerical_hessian = TRUE, ...) {
  va_args <- list(...)

  va_args$skip.hessian <- !use_numerical_hessian

  if (is.null(va_args$start)) {
    print_verbose(object@logger, "Initializing using linear regression estimations.")
    va_args$start <- get_initializing_values(object)
  }
  names(va_args$start) <- get_likelihood_variables(object@system)
  print_debug(
    object@logger, "Using starting values: ",
    paste(names(va_args$start), va_args$start, sep = " = ", collapse = ", ")
  )

  if (is.null(va_args$method)) {
    va_args$method <- "BFGS"
  }

  va_args$minuslogl <- function(...) minus_log_likelihood(object, ...)
  bbmle::parnames(va_args$minuslogl) <- get_likelihood_variables(object@system)
  va_args$gr <- function(...) gradient(object, ...)
  bbmle::parnames(va_args$gr) <- get_likelihood_variables(object@system)

  est <- do.call(bbmle::mle2, va_args)
  est@call.orig <- call("bbmle::mle2", va_args)

  if ((object@model_type_string %in% c("Basic Model", "Directional Model")) && va_args$skip.hessian) {
    print_verbose(object@logger, "Calculating hessian and variance-covariance matrix.")
    est@details$hessian <- hessian(object, est@coef)
    tryCatch(
      est@vcov <- MASS::ginv(est@details$hessian),
      error = function(e) print_warning(object@logger, e$message)
    )
  }

  est
})

setMethod("get_shortage_variance", signature(object = "diseq_base"), function(object) {
  sqrt(object@system@demand@var + object@system@supply@var - 2 * object@system@demand@sigma *
         object@system@supply@sigma * object@system@rho)
})

#' @rdname get_normalized_shortages
setMethod("get_normalized_shortages", signature(object = "diseq_base"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  (object@system@demand@independent_matrix %*% object@system@demand@alpha_beta -
      object@system@supply@independent_matrix %*% object@system@supply@alpha_beta) / get_shortage_variance(object)
})

#' @rdname get_relative_shortages
setMethod("get_relative_shortages", signature(object = "diseq_base"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  (
    object@system@demand@independent_matrix %*% object@system@demand@alpha_beta -
      object@system@supply@independent_matrix %*% object@system@supply@alpha_beta
  ) / object@system@supply@independent_matrix %*% object@system@supply@alpha_beta
})

#' @rdname get_shortage_probabilities
setMethod("get_shortage_probabilities", signature(object = "diseq_base"), function(object, parameters) {
  pnorm(get_normalized_shortages(object, parameters))
})

setMethod("get_scaled_effect", signature(object = "diseq_base"),
  function(object, estimation, variable, scale_function) {
    object@system <- set_parameters(object@system, estimation@coef)
    dvar <- paste0(object@system@demand@variable_prefix, variable)
    svar <- paste0(object@system@supply@variable_prefix, variable)
    in_demand <- dvar %in% get_prefixed_independent_variables(object@system@demand)
    in_supply <- svar %in% get_prefixed_independent_variables(object@system@supply)
    scale <- scale_function(estimation@coef)
    if (in_demand && in_supply) {
      effect <- scale * (estimation@coef[dvar] - estimation@coef[svar]) / get_shortage_variance(object)
      names(effect) <- paste0("B_", variable)
    }
    else if (in_demand) {
      effect <- scale * estimation@coef[dvar] / get_shortage_variance(object)
      names(effect) <- dvar
    }
    else {
      effect <- -scale * estimation@coef[svar] / get_shortage_variance(object)
      names(effect) <- svar
    }
    effect
  }
)

#' @rdname get_mean_marginal_effect
setMethod("get_mean_marginal_effect", signature(object = "diseq_base"), function(object, estimation, variable) {
  get_scaled_effect(object, estimation, variable, function(x) mean(dnorm(get_normalized_shortages(object, x))))
})

#' @rdname get_marginal_effect_at_mean
setMethod("get_marginal_effect_at_mean", signature(object = "diseq_base"), function(object, estimation, variable) {
  get_scaled_effect(object, estimation, variable, function(x) dnorm(mean(get_normalized_shortages(object, x))))
})

#' @rdname get_estimated_shortage_indices
setMethod("get_estimated_shortage_indices", signature(object = "diseq_base"), function(object, estimation) {
  get_normalized_shortages(object, estimation@coef) >= 0
})
