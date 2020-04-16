#' @include diseq_base.R
#' @include derivatives_basic.R

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
#'   use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#' @export
setClass(
  "diseq_basic",
  contains = "diseq_base",
  representation(),
  prototype()
)

#' @describeIn initialize_model_base Basic disequilibrium model base constructor
setMethod(
  "initialize", "diseq_basic",
  function(
           .Object,
           key_columns, quantity_column, price_column,
           demand_specification, supply_specification,
           data,
           use_correlated_shocks = TRUE, verbose = 0) {
    .Object <- callNextMethod(
      .Object,
      "Basic", verbose,
      key_columns, NULL,
      quantity_column, price_column, demand_specification, supply_specification, NULL,
      use_correlated_shocks,
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

  g <- rep(NA, length(get_likelihood_variables(object@system)))
  names(g) <- get_likelihood_variables(object@system)
  g[colnames(object@system@demand@independent_matrix)] <-
    colSums(partial_beta_d_of_loglh(object@system))
  g[colnames(object@system@supply@independent_matrix)] <-
    colSums(partial_beta_s_of_loglh(object@system))
  g[get_prefixed_variance_variable(object@system@demand)] <-
    sum(partial_var_d_of_loglh(object@system))
  g[get_prefixed_variance_variable(object@system@supply)] <-
    sum(partial_var_s_of_loglh(object@system))
  if (object@system@correlated_shocks) {
    g[get_correlation_variable(object@system)] <- sum(partial_rho_of_loglh(object@system))
  }

  as.matrix(-g)
})

setMethod("hessian", signature(object = "diseq_basic"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)

  pbeta_d_pbeta_d <- partial_beta_d_partial_beta_d_of_loglh(object@system)
  pbeta_d_pbeta_s <- partial_beta_d_partial_beta_s_of_loglh(object@system)
  pbeta_d_pvar_d <- partial_beta_d_partial_var_d_of_loglh(object@system)
  pbeta_d_pvar_s <- partial_beta_d_partial_var_s_of_loglh(object@system)
  pbeta_s_pbeta_s <- partial_beta_s_partial_beta_s_of_loglh(object@system)
  pbeta_s_pvar_d <- partial_beta_s_partial_var_d_of_loglh(object@system)
  pbeta_s_pvar_s <- partial_beta_s_partial_var_s_of_loglh(object@system)
  pvar_d_pvar_d <- partial_var_d_partial_var_d_of_loglh(object@system)
  pvar_d_pvar_s <- partial_var_d_partial_var_s_of_loglh(object@system)
  pvar_s_pvar_s <- partial_var_s_partial_var_s_of_loglh(object@system)

  if (object@system@correlated_shocks) {
    pbeta_d_prho <- partial_beta_d_partial_rho_of_loglh(object@system)
    pbeta_s_prho <- partial_beta_s_partial_rho_of_loglh(object@system)
    pvar_d_prho <- partial_var_d_partial_rho_of_loglh(object@system)
    pvar_s_prho <- partial_var_s_partial_rho_of_loglh(object@system)
    prho_prho <- partial_rho_partial_rho_of_loglh(object@system)
  }

  h <- rep(0.0, length(get_likelihood_variables(object@system)))
  names(h) <- get_likelihood_variables(object@system)
  h <- h %*% t(h)
  rownames(h) <- colnames(h)

  h[rownames(pbeta_d_pbeta_d), colnames(pbeta_d_pbeta_d)] <- pbeta_d_pbeta_d

  h[rownames(pbeta_d_pbeta_s), colnames(pbeta_d_pbeta_s)] <- pbeta_d_pbeta_s
  h[colnames(pbeta_d_pbeta_s), rownames(pbeta_d_pbeta_s)] <- t(pbeta_d_pbeta_s)

  h[get_prefixed_variance_variable(object@system@demand), colnames(pbeta_d_pvar_d)] <-
    t(pbeta_d_pvar_d)
  h[colnames(pbeta_d_pvar_d), get_prefixed_variance_variable(object@system@demand)] <-
    pbeta_d_pvar_d

  h[get_prefixed_variance_variable(object@system@supply), colnames(pbeta_d_pvar_s)] <-
    t(pbeta_d_pvar_s)
  h[colnames(pbeta_d_pvar_s), get_prefixed_variance_variable(object@system@supply)] <-
    pbeta_d_pvar_s

  h[
    get_prefixed_variance_variable(object@system@demand),
    get_prefixed_variance_variable(object@system@demand)
  ] <- pvar_d_pvar_d

  h[
    get_prefixed_variance_variable(object@system@demand),
    get_prefixed_variance_variable(object@system@supply)
  ] <- pvar_d_pvar_s

  h[
    get_prefixed_variance_variable(object@system@supply),
    get_prefixed_variance_variable(object@system@demand)
  ] <- pvar_d_pvar_s

  h[rownames(pbeta_s_pbeta_s), colnames(pbeta_s_pbeta_s)] <- pbeta_s_pbeta_s

  h[
    get_prefixed_variance_variable(object@system@supply),
    colnames(pbeta_s_pvar_s)
  ] <- t(pbeta_s_pvar_s)
  h[
    colnames(pbeta_s_pvar_s),
    get_prefixed_variance_variable(object@system@supply)
  ] <- pbeta_s_pvar_s

  h[
    get_prefixed_variance_variable(object@system@demand),
    colnames(pbeta_s_pvar_d)
  ] <- t(pbeta_s_pvar_d)
  h[
    colnames(pbeta_s_pvar_d),
    get_prefixed_variance_variable(object@system@demand)
  ] <- pbeta_s_pvar_d

  h[
    get_prefixed_variance_variable(object@system@supply),
    get_prefixed_variance_variable(object@system@supply)
  ] <-
    pvar_s_pvar_s

  if (object@system@correlated_shocks) {
    h[get_correlation_variable(object@system), colnames(pbeta_d_prho)] <- t(pbeta_d_prho)
    h[colnames(pbeta_d_prho), get_correlation_variable(object@system)] <- pbeta_d_prho

    h[
      get_prefixed_variance_variable(object@system@demand),
      get_correlation_variable(object@system)
    ] <- pvar_d_prho
    h[
      get_correlation_variable(object@system),
      get_prefixed_variance_variable(object@system@demand)
    ] <- pvar_d_prho

    h[get_correlation_variable(object@system), colnames(pbeta_s_prho)] <- t(pbeta_s_prho)
    h[colnames(pbeta_s_prho), get_correlation_variable(object@system)] <- pbeta_s_prho

    h[
      get_prefixed_variance_variable(object@system@supply),
      get_correlation_variable(object@system)
    ] <- pvar_s_prho
    h[
      get_correlation_variable(object@system),
      get_prefixed_variance_variable(object@system@supply)
    ] <- pvar_s_prho

    h[
      get_correlation_variable(object@system),
      get_correlation_variable(object@system)
    ] <- prho_prho
  }

  -h
})
