#' Equilibrium model estimated using full-information maximum likelihood.
#'
#' @include eq_base.R
#' @name eq_fiml-class
#' @export
setClass(
  "eq_fiml",
  contains = "eq_base",
  representation()
)

setMethod("initialize", "eq_fiml", function(
  .Object,
  verbose = 0,
  key_columns,
  quantity_column, price_column, demand_specification, supply_specification,
  use_correlated_shocks = TRUE,
  data
) {
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
})

#' @rdname minus_log_likelihood
setMethod("minus_log_likelihood", signature(object = "eq_fiml"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  -sum(object@system@llh)
})



setMethod("gradient", signature(object = "eq_fiml"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)

  partial_alpha_d <- (
  	(object@system@delta ** 2 * object@system@mu_P * object@system@rho1_QP * object@system@var_Q *
    object@system@z_PQ - object@system@delta ** 2 * object@system@rho1_QP * object@system@sigma_P *
    object@system@sigma_Q * object@system@z_QP * (object@system@supply@control_matrix %*%
    object@system@supply@beta - object@system@mu_Q) + object@system@delta ** 2 * object@system@sigma_P *
    object@system@var_Q * (object@system@h_P * object@system@rho1_QP * object@system@z_PQ - 1) +
    object@system@delta * object@system@sigma_P * (object@system@h_Q * object@system@rho1_QP *
    object@system@z_QP - 1) * (object@system@demand@alpha * object@system@supply@sigma ** 2 -
    object@system@supply@alpha * object@system@rho * object@system@demand@sigma * object@system@supply@sigma +
    object@system@delta * object@system@var_Q) - object@system@rho1_QP * (object@system@delta *
    object@system@rho_QP * object@system@sigma_P * (object@system@demand@alpha * object@system@supply@sigma ** 2
    - object@system@supply@alpha * object@system@rho * object@system@demand@sigma * object@system@supply@sigma +
    2 * object@system@delta * object@system@var_Q) + object@system@sigma_Q * (-2 * object@system@supply@alpha *
    object@system@demand@sigma ** 2 + object@system@rho * object@system@demand@sigma *
    object@system@supply@sigma * (object@system@demand@alpha + 3 * object@system@supply@alpha) -
    object@system@supply@sigma ** 2 * (object@system@demand@alpha + object@system@supply@alpha))) *
    (object@system@h_P * object@system@h_Q * object@system@rho1_QP * (object@system@rho1_QP ** 2 +
    object@system@rho2_QP ** 2) - object@system@rho1_QP ** 2 * object@system@rho2_QP * (object@system@h_P ** 2 +
    object@system@h_Q ** 2) + object@system@rho2_QP)) / (object@system@delta ** 3 * object@system@sigma_P *
    object@system@var_Q)
  )

  partial_beta_d <- (
  	object@system@demand@control_matrix * object@system@rho1_QP * c(object@system@supply@alpha *
    object@system@sigma_P * object@system@z_QP + object@system@sigma_Q * object@system@z_PQ) /
    (object@system@delta * object@system@sigma_P * object@system@sigma_Q)
  )

  partial_alpha_s <- (
  	(-object@system@delta ** 2 * object@system@mu_P * object@system@rho1_QP * object@system@var_Q *
    object@system@z_PQ + object@system@delta ** 2 * object@system@rho1_QP * object@system@sigma_P *
    object@system@sigma_Q * object@system@z_QP * (object@system@demand@control_matrix %*%
    object@system@demand@beta - object@system@mu_Q) + object@system@delta ** 2 * object@system@sigma_P *
    object@system@var_Q * (-object@system@h_P * object@system@rho1_QP * object@system@z_PQ + 1) -
    object@system@delta * object@system@sigma_P * (object@system@h_Q * object@system@rho1_QP *
    object@system@z_QP - 1) * (object@system@demand@alpha * object@system@rho * object@system@demand@sigma *
    object@system@supply@sigma - object@system@supply@alpha * object@system@demand@sigma ** 2 +
    object@system@delta * object@system@var_Q) + object@system@rho1_QP * (object@system@delta *
    object@system@rho_QP * object@system@sigma_P * (object@system@demand@alpha * object@system@rho *
    object@system@demand@sigma * object@system@supply@sigma - object@system@supply@alpha *
    object@system@demand@sigma ** 2 + 2 * object@system@delta * object@system@var_Q) + object@system@sigma_Q *
    (-2 * object@system@demand@alpha * object@system@supply@sigma ** 2 + object@system@rho *
    object@system@demand@sigma * object@system@supply@sigma * (3 * object@system@demand@alpha +
    object@system@supply@alpha) - object@system@demand@sigma ** 2 * (object@system@demand@alpha +
    object@system@supply@alpha))) * (object@system@h_P * object@system@h_Q * object@system@rho1_QP *
    (object@system@rho1_QP ** 2 + object@system@rho2_QP ** 2) - object@system@rho1_QP ** 2 *
    object@system@rho2_QP * (object@system@h_P ** 2 + object@system@h_Q ** 2) + object@system@rho2_QP)) /
    (object@system@delta ** 3 * object@system@sigma_P * object@system@var_Q)
  )

  partial_beta_s <- (
  	-object@system@supply@control_matrix * object@system@rho1_QP * c(object@system@demand@alpha *
    object@system@sigma_P * object@system@z_QP + object@system@sigma_Q * object@system@z_PQ) /
    (object@system@delta * object@system@sigma_P * object@system@sigma_Q)
  )

  partial_var_d <- (
  	(-object@system@supply@alpha * object@system@var_P * (object@system@demand@alpha * object@system@rho *
    object@system@supply@sigma - object@system@supply@alpha * object@system@demand@sigma) * (object@system@h_Q *
    object@system@rho1_QP * object@system@z_QP - 1) + object@system@rho1_QP * (object@system@rho_QP *
    (object@system@supply@alpha * object@system@var_P * (object@system@demand@alpha * object@system@rho *
    object@system@supply@sigma - object@system@supply@alpha * object@system@demand@sigma) + object@system@var_Q
    * (object@system@rho * object@system@supply@sigma - object@system@demand@sigma)) + object@system@sigma_P *
    object@system@sigma_Q * (2 * object@system@supply@alpha * object@system@demand@sigma - object@system@rho *
    object@system@supply@sigma * (object@system@demand@alpha + object@system@supply@alpha))) *
    (object@system@h_P * object@system@h_Q * object@system@rho1_QP * (object@system@rho1_QP ** 2 +
    object@system@rho2_QP ** 2) - object@system@rho1_QP ** 2 * object@system@rho2_QP * (object@system@h_P ** 2 +
    object@system@h_Q ** 2) + object@system@rho2_QP) - object@system@var_Q * (object@system@rho *
    object@system@supply@sigma - object@system@demand@sigma) * (object@system@h_P * object@system@rho1_QP *
    object@system@z_PQ - 1)) / (2 * object@system@delta ** 2 * object@system@var_P * object@system@var_Q *
    object@system@demand@sigma)
  )

  partial_var_s <- (
  	(object@system@demand@alpha * object@system@var_P * (object@system@demand@alpha * object@system@supply@sigma -
    object@system@supply@alpha * object@system@rho * object@system@demand@sigma) * (object@system@h_Q *
    object@system@rho1_QP * object@system@z_QP - 1) - object@system@rho1_QP * (object@system@rho_QP *
    (object@system@demand@alpha * object@system@var_P * (object@system@demand@alpha * object@system@supply@sigma
    - object@system@supply@alpha * object@system@rho * object@system@demand@sigma) + object@system@var_Q *
    (-object@system@rho * object@system@demand@sigma + object@system@supply@sigma)) + object@system@sigma_P *
    object@system@sigma_Q * (-2 * object@system@demand@alpha * object@system@supply@sigma + object@system@rho *
    object@system@demand@sigma * (object@system@demand@alpha + object@system@supply@alpha))) *
    (object@system@h_P * object@system@h_Q * object@system@rho1_QP * (object@system@rho1_QP ** 2 +
    object@system@rho2_QP ** 2) - object@system@rho1_QP ** 2 * object@system@rho2_QP * (object@system@h_P ** 2 +
    object@system@h_Q ** 2) + object@system@rho2_QP) - object@system@var_Q * (object@system@rho *
    object@system@demand@sigma - object@system@supply@sigma) * (object@system@h_P * object@system@rho1_QP *
    object@system@z_PQ - 1)) / (2 * object@system@delta ** 2 * object@system@var_P * object@system@var_Q *
    object@system@supply@sigma)
  )

  partial_rho <- (
  	object@system@demand@sigma * object@system@supply@sigma * (-object@system@demand@alpha *
    object@system@supply@alpha * object@system@var_P * (object@system@h_Q * object@system@rho1_QP *
    object@system@z_QP - 1) + object@system@rho1_QP * (object@system@rho_QP * (object@system@demand@alpha *
    object@system@supply@alpha * object@system@var_P + object@system@var_Q) - object@system@sigma_P *
    object@system@sigma_Q * (object@system@demand@alpha + object@system@supply@alpha)) * (object@system@h_P *
    object@system@h_Q * object@system@rho1_QP * (object@system@rho1_QP ** 2 + object@system@rho2_QP ** 2) -
    object@system@rho1_QP ** 2 * object@system@rho2_QP * (object@system@h_P ** 2 + object@system@h_Q ** 2) +
    object@system@rho2_QP) - object@system@var_Q * (object@system@h_P * object@system@rho1_QP *
    object@system@z_PQ - 1)) / (object@system@delta ** 2 * object@system@var_P * object@system@var_Q)
  )

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

#' @describeIn estimate Equilibrium model estimation.
setMethod("estimate", signature(object = "eq_fiml"), function(object, ...) {
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

  est
})
