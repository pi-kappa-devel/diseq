#' @include eq_base.R
#' @include derivatives_fiml.R

#' @title Equilibrium model estimated using full-information maximum likelihood.
#'
#' @description The equilibrium model consists of thee equations. The demand, the
#' supply and the market clearing equations. This model is estimated using full information
#' maximum likelihood.
#'
#' \deqn{D_{nt} = X_{d,nt}'\beta_{d} + P_{nt}\alpha_{d} + u_{d,nt},}
#' \deqn{S_{nt} = X_{s,nt}'\beta_{s} + P_{nt}\alpha_{s} + u_{s,nt},}
#' \deqn{Q_{nt} = D_{nt} = S_{nt}.}
#'
#' @examples
#' simulated_data <- simulate_model_data(
#'   "eq_fiml", 500, 3, # model type, observed entities and time points
#'   -0.9, 14.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 3.2, c(0.3), c(0.5, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "eq_fiml", # model type
#'   c("id", "date"), "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'   simulated_data, # data
#'   use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#' @export
setClass(
  "eq_fiml",
  contains = "eq_base",
  representation()
)

#' @describeIn initialize_model_base Full information maximum likelihood model constructor
setMethod(
  "initialize", "eq_fiml",
  function(
           .Object,
           key_columns, quantity_column, price_column,
           demand_specification, supply_specification,
           data,
           use_correlated_shocks = TRUE, verbose = 0) {
    .Object <- callNextMethod(
      .Object,
      "Equilibrium FIML", verbose,
      key_columns,
      quantity_column, price_column, demand_specification, supply_specification,
      use_correlated_shocks,
      data,
      function(...) new("system_fiml", ...)
    )

    .Object
  }
)

#' @rdname minus_log_likelihood
setMethod("minus_log_likelihood", signature(object = "eq_fiml"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  -sum(object@system@llh)
})


setMethod("gradient", signature(object = "eq_fiml"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)

  partial_alpha_d <- partial_alpha_d(object@system)
  partial_beta_d <- partial_beta_d(object@system)
  partial_alpha_s <- partial_alpha_s(object@system)
  partial_beta_s <- partial_beta_s(object@system)
  partial_var_d <- partial_var_d(object@system)
  partial_var_s <- partial_var_s(object@system)
  partial_rho <- partial_rho(object@system)

  g <- rep(NA, length(get_likelihood_variables(object@system)))
  names(g) <- get_likelihood_variables(object@system)

  g[get_prefixed_price_variable(object@system@demand)] <- sum(partial_alpha_d)
  g[get_prefixed_control_variables(object@system@demand)] <- colSums(partial_beta_d)
  g[get_prefixed_price_variable(object@system@supply)] <- sum(partial_alpha_s)
  g[get_prefixed_control_variables(object@system@supply)] <- colSums(partial_beta_s)
  g[get_prefixed_variance_variable(object@system@demand)] <- sum(partial_var_d)
  g[get_prefixed_variance_variable(object@system@supply)] <- sum(partial_var_s)
  if (object@system@correlated_shocks) {
    g[get_correlation_variable(object@system)] <- sum(partial_rho)
  }

  as.matrix(-g)
})

#' @rdname scores
setMethod("scores", signature(object = "eq_fiml"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)

  scores <- cbind(
    partial_alpha_d(object@system), partial_beta_d(object@system),
    partial_alpha_s(object@system), partial_beta_s(object@system),
    partial_var_d(object@system), partial_var_s(object@system)
  )

  if (object@system@correlated_shocks) {
    scores <- cbind(scores, partial_rho(object@system))
  }
  colnames(scores) <- get_likelihood_variables(object@system)

  -scores
})

#' @describeIn estimate Equilibrium model estimation.
#' @param use_heteroscedasticity_consistent_errors If true, the variance-covariance matrix is
#' calculated using heteroscedasticity adjusted (Huber-White) standard errors.
#' @param cluster_errors_by A vector with names of variables belonging in the data of the
#' model. If the vector is supplied, the variance-covariance matrix is calculated by
#' grouping the score matrix based on the passed variables.
setMethod(
  "estimate", signature(object = "eq_fiml"),
  function(object, use_heteroscedasticity_consistent_errors = FALSE,
           cluster_errors_by = NA, ...) {
    va_args <- list(...)

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

    if (use_heteroscedasticity_consistent_errors) {
      est <- set_heteroscedasticity_consistent_errors(object, est)
    }

    if (!is.na(cluster_errors_by)) {
      est <- set_clustered_errors(object, est, cluster_errors_by)
    }

    est
  }
)
