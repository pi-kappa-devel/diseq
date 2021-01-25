#' @include disequilibrium_model.R
#' @include derivatives_directional.R

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
#'   use_correlated_shocks = TRUE # allow shocks to be correlated
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
           use_correlated_shocks = TRUE, verbose = 0) {
    .Object <- callNextMethod(
      .Object,
      "Directional", verbose,
      key_columns, time_column,
      quantity_column, price_column, demand_specification, supply_specification, NULL,
      use_correlated_shocks,
      data,
      function(...) new("system_directional", ...)
    )

    # Check for misspecification
    if (
      price_column %in% .Object@system@demand@independent_variables &&
        price_column %in% .Object@system@supply@independent_variables
    ) {
      print_error(
        .Object@logger,
        "Price cannot be part of both the demand and supply equations here ",
        "(See Maddala, (1974) <https://doi.org/10.2307/1914215>, pp1021)"
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

  nd <- object@system@demand@separation_subset
  pd <- object@system@supply@separation_subset

  l_pbd <- (
    colSums(partial_beta_d_of_loglh_d(object@system)[nd, ]) +
      colSums(partial_beta_d_of_loglh_s(object@system)[pd, ]))
  l_pbs <- (
    colSums(partial_beta_s_of_loglh_d(object@system)[nd, ]) +
      colSums(partial_beta_s_of_loglh_s(object@system)[pd, ]))
  l_pvard <- (
    sum(partial_var_d_of_loglh_d(object@system)[nd]) +
      sum(partial_var_d_of_loglh_s(object@system)[pd]))
  l_pvars <- (
    sum(partial_var_s_of_loglh_d(object@system)[nd]) +
      sum(partial_var_s_of_loglh_s(object@system)[pd]))

  if (object@system@correlated_shocks) {
    l_prho <- (
      sum(partial_rho_of_loglh_d(object@system)[nd]) +
        sum(partial_rho_of_loglh_s(object@system)[pd]))
  }

  g <- rep(NA, length(get_likelihood_variables(object@system)))
  names(g) <- get_likelihood_variables(object@system)
  g[colnames(object@system@demand@independent_matrix)] <- l_pbd
  g[colnames(object@system@supply@independent_matrix)] <- l_pbs
  g[get_prefixed_variance_variable(object@system@demand)] <- l_pvard
  g[get_prefixed_variance_variable(object@system@supply)] <- l_pvars
  if (object@system@correlated_shocks) {
    g[get_correlation_variable(object@system)] <- l_prho
  }

  as.matrix(-g)
})

#' @rdname scores
setMethod("scores", signature(object = "diseq_directional"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)

  nd <- object@system@demand@separation_subset
  pd <- object@system@supply@separation_subset

  l_pbd <- partial_beta_d_of_loglh_d(object@system)
  l_pbd[pd, ] <- partial_beta_d_of_loglh_s(object@system)[pd, ]

  l_pbs <- partial_beta_s_of_loglh_d(object@system)
  l_pbs[pd, ] <- partial_beta_s_of_loglh_d(object@system)[pd, ]

  l_pvard <- partial_var_d_of_loglh_d(object@system)
  l_pvard[pd] <- partial_var_d_of_loglh_s(object@system)[pd]

  l_pvars <- partial_var_s_of_loglh_d(object@system)
  l_pvars[pd] <- partial_var_s_of_loglh_s(object@system)[pd]

  scores <- cbind(l_pbd, l_pbs, l_pvard, l_pvars)

  if (object@system@correlated_shocks) {
    l_prho <- partial_rho_of_loglh_d(object@system)
    l_prho[pd] <- partial_rho_of_loglh_s(object@system)[pd]

    scores <- cbind(scores, as.matrix(l_prho))
  }
  colnames(scores) <- get_likelihood_variables(object@system)

  -scores
})

setMethod("hessian", signature(object = "diseq_directional"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)

  l_pbdpbd <- partial_beta_d_partial_beta_d_of_loglh(object@system)
  l_pbdpbs <- partial_beta_d_partial_beta_s_of_loglh(object@system)
  l_pbdpvard <- partial_beta_d_partial_var_d_of_loglh(object@system)
  l_pbdpvars <- partial_beta_d_partial_var_s_of_loglh(object@system)

  l_pbspbs <- partial_beta_s_partial_beta_s_of_loglh(object@system)
  l_pbspvard <- partial_beta_s_partial_var_d_of_loglh(object@system)
  l_pbspvars <- partial_beta_s_partial_var_s_of_loglh(object@system)

  l_pvardpvard <- partial_var_d_partial_var_d_of_loglh(object@system)
  l_pvardpvars <- partial_var_d_partial_var_s_of_loglh(object@system)

  l_pvarspvars <- partial_var_s_partial_var_s_of_loglh(object@system)

  if (object@system@correlated_shocks) {
    l_pbdprho <- partial_beta_d_partial_rho_of_loglh(object@system)
    l_pbsprho <- partial_beta_s_partial_rho_of_loglh(object@system)
    l_pvardprho <- partial_var_d_partial_rho_of_loglh(object@system)
    l_pvarsprho <- partial_var_s_partial_rho_of_loglh(object@system)
    l_prhoprho <- partial_rho_partial_rho_of_loglh(object@system)
  }

  h <- rep(0, length(get_likelihood_variables(object@system)))
  names(h) <- get_likelihood_variables(object@system)
  h <- h %*% t(h)
  rownames(h) <- colnames(h)

  h[rownames(l_pbdpbd), colnames(l_pbdpbd)] <- l_pbdpbd

  h[rownames(l_pbdpbs), colnames(l_pbdpbs)] <- l_pbdpbs
  h[colnames(l_pbdpbs), rownames(l_pbdpbs)] <- t(l_pbdpbs)

  h[get_prefixed_variance_variable(object@system@demand), names(l_pbdpvard)] <- t(l_pbdpvard)
  h[names(l_pbdpvard), get_prefixed_variance_variable(object@system@demand)] <- l_pbdpvard

  h[get_prefixed_variance_variable(object@system@supply), names(l_pbdpvars)] <- t(l_pbdpvars)
  h[names(l_pbdpvars), get_prefixed_variance_variable(object@system@supply)] <- l_pbdpvars

  h[
    get_prefixed_variance_variable(object@system@demand),
    get_prefixed_variance_variable(object@system@demand)
  ] <- l_pvardpvard

  h[
    get_prefixed_variance_variable(object@system@demand),
    get_prefixed_variance_variable(object@system@supply)
  ] <- l_pvardpvars
  h[
    get_prefixed_variance_variable(object@system@supply),
    get_prefixed_variance_variable(object@system@demand)
  ] <- l_pvardpvars

  h[rownames(l_pbspbs), colnames(l_pbspbs)] <- l_pbspbs

  h[get_prefixed_variance_variable(object@system@supply), names(l_pbspvars)] <- t(l_pbspvars)
  h[names(l_pbspvars), get_prefixed_variance_variable(object@system@supply)] <- l_pbspvars

  h[get_prefixed_variance_variable(object@system@demand), names(l_pbspvard)] <- t(l_pbspvard)
  h[names(l_pbspvard), get_prefixed_variance_variable(object@system@demand)] <- l_pbspvard

  h[
    get_prefixed_variance_variable(object@system@supply),
    get_prefixed_variance_variable(object@system@supply)
  ] <- l_pvarspvars

  if (object@system@correlated_shocks) {
    h[get_correlation_variable(object@system), names(l_pbdprho)] <- t(l_pbdprho)
    h[names(l_pbdprho), get_correlation_variable(object@system)] <- l_pbdprho

    h[get_correlation_variable(object@system), names(l_pbsprho)] <- t(l_pbsprho)
    h[names(l_pbsprho), get_correlation_variable(object@system)] <- l_pbsprho

    h[
      get_prefixed_variance_variable(object@system@demand),
      get_correlation_variable(object@system)
    ] <- l_pvardprho
    h[
      get_correlation_variable(object@system),
      get_prefixed_variance_variable(object@system@demand)
    ] <- l_pvardprho

    h[
      get_prefixed_variance_variable(object@system@supply),
      get_correlation_variable(object@system)
    ] <- l_pvarsprho
    h[
      get_correlation_variable(object@system),
      get_prefixed_variance_variable(object@system@supply)
    ] <- l_pvarsprho

    h[
      get_correlation_variable(object@system),
      get_correlation_variable(object@system)
    ] <- l_prhoprho
  }

  -h
})
