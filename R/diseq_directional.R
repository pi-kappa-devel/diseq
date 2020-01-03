#' Directional disequilibrium model with sample separation.
#'
#' @include diseq_base.R
#' @name diseq_directional-class
#' @export
setClass(
  "diseq_directional",
  contains = "diseq_base",
  representation(),
  prototype()
)

setMethod("initialize", "diseq_directional", function(
  .Object,
  verbose = 0,
  key_columns, time_column,
  quantity_column, price_column, demand_specification, supply_specification,
  use_correlated_shocks = TRUE,
  data
) {
  .Object <- callNextMethod(
    .Object,
    "Directional Model", verbose,
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
    print_error(.Object@logger,
      "Price cannot be part of both the demand and supply equations here (See Maddala, 1974, pp1021)"
    )
  }

  print_info(.Object@logger,
     "Sample separated with ", sum(.Object@system@demand@separation_subset), " rows in excess supply and ",
     sum(.Object@system@supply@separation_subset), " in excess demand regime."
  )

  .Object
})

#' @rdname minus_log_likelihood
setMethod("minus_log_likelihood", signature(object = "diseq_directional"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)

  loglhd <- sum(
    log(object@system@demand@Psi[object@system@demand@separation_subset] / object@system@demand@sigma)
  )
  loglhs <- sum(
    log(object@system@supply@Psi[object@system@supply@separation_subset] / object@system@supply@sigma)
  )
  - loglhd - loglhs
})

setMethod("gradient", signature(object = "diseq_directional"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)

  nd <- object@system@demand@separation_subset
  pd <- object@system@supply@separation_subset

  l_pbd <- colSums(partial_beta_d_of_loglh_d(object)[nd, ]) + colSums(partial_beta_d_of_loglh_s(object)[pd, ])
  l_pbs <- colSums(partial_beta_s_of_loglh_d(object)[nd, ]) + colSums(partial_beta_s_of_loglh_s(object)[pd, ])
  l_pvard <- sum(partial_var_d_of_loglh_d(object)[nd]) + sum(partial_var_d_of_loglh_s(object)[pd])
  l_pvars <- sum(partial_var_s_of_loglh_d(object)[nd]) + sum(partial_var_s_of_loglh_s(object)[pd])

  if (object@system@correlated_shocks) {
    l_prho <- sum(partial_rho_of_loglh_d(object)[nd]) + sum(partial_rho_of_loglh_s(object)[pd])
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

setMethod("hessian", signature(object = "diseq_directional"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)

  l_pbdpbd <- partial_beta_d_partial_beta_d_of_loglh(object)
  l_pbdpbs <- partial_beta_d_partial_beta_s_of_loglh(object)
  l_pbdpvard <- partial_beta_d_partial_var_d_of_loglh(object)
  l_pbdpvars <- partial_beta_d_partial_var_s_of_loglh(object)

  l_pbspbs <- partial_beta_s_partial_beta_s_of_loglh(object)
  l_pbspvard <- partial_beta_s_partial_var_d_of_loglh(object)
  l_pbspvars <- partial_beta_s_partial_var_s_of_loglh(object)

  l_pvardpvard <- partial_var_d_partial_var_d_of_loglh(object)
  l_pvardpvars <- partial_var_d_partial_var_s_of_loglh(object)

  l_pvarspvars <- partial_var_s_partial_var_s_of_loglh(object)

  if (object@system@correlated_shocks) {
    l_pbdprho <- partial_beta_d_partial_rho_of_loglh(object)
    l_pbsprho <- partial_beta_s_partial_rho_of_loglh(object)
    l_pvardprho <- partial_var_d_partial_rho_of_loglh(object)
    l_pvarsprho <- partial_var_s_partial_rho_of_loglh(object)
    l_prhoprho <- partial_rho_partial_rho_of_loglh(object)
  }

  ###############################

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

  h[get_prefixed_variance_variable(object@system@demand), get_prefixed_variance_variable(object@system@demand)] <-
    l_pvardpvard

  h[get_prefixed_variance_variable(object@system@demand), get_prefixed_variance_variable(object@system@supply)] <-
    l_pvardpvars
  h[get_prefixed_variance_variable(object@system@supply), get_prefixed_variance_variable(object@system@demand)] <-
    l_pvardpvars

  h[rownames(l_pbspbs), colnames(l_pbspbs)] <- l_pbspbs

  h[get_prefixed_variance_variable(object@system@supply), names(l_pbspvars)] <- t(l_pbspvars)
  h[names(l_pbspvars), get_prefixed_variance_variable(object@system@supply)] <- l_pbspvars

  h[get_prefixed_variance_variable(object@system@demand), names(l_pbspvard)] <- t(l_pbspvard)
  h[names(l_pbspvard), get_prefixed_variance_variable(object@system@demand)] <- l_pbspvard

  h[get_prefixed_variance_variable(object@system@supply), get_prefixed_variance_variable(object@system@supply)] <-
    l_pvarspvars

  if (object@system@correlated_shocks) {
    h[get_correlation_variable(object@system), names(l_pbdprho)] <- t(l_pbdprho)
    h[names(l_pbdprho), get_correlation_variable(object@system)] <- l_pbdprho

    h[get_correlation_variable(object@system), names(l_pbsprho)] <- t(l_pbsprho)
    h[names(l_pbsprho), get_correlation_variable(object@system)] <- l_pbsprho

    h[get_prefixed_variance_variable(object@system@demand), get_correlation_variable(object@system)] <-
      l_pvardprho
    h[get_correlation_variable(object@system), get_prefixed_variance_variable(object@system@demand)] <-
      l_pvardprho

    h[get_prefixed_variance_variable(object@system@supply), get_correlation_variable(object@system)] <-
      l_pvarsprho
    h[get_correlation_variable(object@system), get_prefixed_variance_variable(object@system@supply)] <-
      l_pvarsprho

    h[get_correlation_variable(object@system), get_correlation_variable(object@system)] <- l_prhoprho
  }

  -h
})

setGeneric("partial_beta_d_of_loglh_d", function(object) {
  standardGeneric("partial_beta_d_of_loglh_d")
})

setMethod("partial_beta_d_of_loglh_d", signature(object = "diseq_directional"), function(object) {
  c((object@system@demand@Psi * object@system@demand@h - object@system@demand@psi * object@system@rho2) / (object@system@demand@Psi * object@system@demand@sigma)) * object@system@demand@independent_matrix
})

setGeneric("partial_beta_d_of_loglh_s", function(object) {
  standardGeneric("partial_beta_d_of_loglh_s")
})

setMethod("partial_beta_d_of_loglh_s", signature(object = "diseq_directional"), function(object) {
  c(object@system@supply@psi * object@system@rho1 / (object@system@supply@Psi * object@system@demand@sigma)) * object@system@demand@independent_matrix
})

setGeneric("partial_beta_s_of_loglh_d", function(object) {
  standardGeneric("partial_beta_s_of_loglh_d")
})

setMethod("partial_beta_s_of_loglh_d", signature(object = "diseq_directional"), function(object) {
  c(object@system@demand@psi * object@system@rho1 / (object@system@demand@Psi * object@system@supply@sigma)) * object@system@supply@independent_matrix
})

setGeneric("partial_beta_s_of_loglh_s", function(object) {
  standardGeneric("partial_beta_s_of_loglh_s")
})

setMethod("partial_beta_s_of_loglh_s", signature(object = "diseq_directional"), function(object) {
  c((object@system@supply@Psi * object@system@supply@h - object@system@supply@psi * object@system@rho2) / (object@system@supply@Psi * object@system@supply@sigma)) * object@system@supply@independent_matrix
})

setGeneric("partial_var_d_of_loglh_d", function(object) {
  standardGeneric("partial_var_d_of_loglh_d")
})

setMethod("partial_var_d_of_loglh_d", signature(object = "diseq_directional"), function(object) {
  c((object@system@demand@Psi * (object@system@demand@h ** 2 - 1) - object@system@demand@h * object@system@demand@psi * object@system@rho2) / (2 * object@system@demand@Psi * object@system@demand@var))
})

setGeneric("partial_var_d_of_loglh_s", function(object) {
  standardGeneric("partial_var_d_of_loglh_s")
})

setMethod("partial_var_d_of_loglh_s", signature(object = "diseq_directional"), function(object) {
  c(object@system@demand@h * object@system@supply@psi * object@system@rho1 / (2 * object@system@supply@Psi * object@system@demand@var))
})

setGeneric("partial_var_s_of_loglh_d", function(object) {
  standardGeneric("partial_var_s_of_loglh_d")
})

setMethod("partial_var_s_of_loglh_d", signature(object = "diseq_directional"), function(object) {
  c(object@system@supply@h * object@system@demand@psi * object@system@rho1 / (2 * object@system@demand@Psi * object@system@supply@var))
})

setGeneric("partial_var_s_of_loglh_s", function(object) {
  standardGeneric("partial_var_s_of_loglh_s")
})

setMethod("partial_var_s_of_loglh_s", signature(object = "diseq_directional"), function(object) {
  c((object@system@supply@Psi * (object@system@supply@h ** 2 - 1) - object@system@supply@h * object@system@supply@psi * object@system@rho2) / (2 * object@system@supply@Psi * object@system@supply@var))
})

setGeneric("partial_rho_of_loglh_d", function(object) {
  standardGeneric("partial_rho_of_loglh_d")
})

setMethod("partial_rho_of_loglh_d", signature(object = "diseq_directional"), function(object) {
  c(object@system@demand@psi * object@system@rho1 ** 2 * object@system@demand@z / object@system@demand@Psi)
})

setGeneric("partial_rho_of_loglh_s", function(object) {
  standardGeneric("partial_rho_of_loglh_s")
})

setMethod("partial_rho_of_loglh_s", signature(object = "diseq_directional"), function(object) {
  c(object@system@supply@psi * object@system@rho1 ** 2 * object@system@supply@z / object@system@supply@Psi)
})

setGeneric("partial_beta_d_partial_beta_d_of_loglh", function(object) {
  standardGeneric("partial_beta_d_partial_beta_d_of_loglh")
})

setMethod("partial_beta_d_partial_beta_d_of_loglh", signature(object = "diseq_directional"), function(object) {
  t(object@system@demand@independent_matrix[object@system@demand@separation_subset, ] * c(((-object@system@demand@Psi ** 2 + object@system@demand@Psi * object@system@demand@psi * object@system@rho2 ** 2 * object@system@supply@z - object@system@demand@psi ** 2 * object@system@rho2 ** 2) / (object@system@demand@Psi ** 2 * object@system@demand@var)))[object@system@demand@separation_subset]) %*% object@system@demand@independent_matrix[object@system@demand@separation_subset, ] +
    t(object@system@demand@independent_matrix[object@system@supply@separation_subset, ] * c((object@system@supply@psi * object@system@rho1 ** 2 * (object@system@supply@Psi * object@system@demand@z - object@system@supply@psi) / (object@system@supply@Psi ** 2 * object@system@demand@var)))[object@system@supply@separation_subset]) %*% object@system@demand@independent_matrix[object@system@supply@separation_subset, ]
})

setGeneric("partial_beta_d_partial_beta_s_of_loglh", function(object) {
  standardGeneric("partial_beta_d_partial_beta_s_of_loglh")
})

setMethod("partial_beta_d_partial_beta_s_of_loglh", signature(object = "diseq_directional"), function(object) {
  t(object@system@supply@independent_matrix[object@system@demand@separation_subset, ] * c((-object@system@demand@psi * object@system@rho1 * object@system@rho2 * (object@system@demand@Psi * object@system@supply@z - object@system@demand@psi) / (object@system@demand@Psi ** 2 * object@system@demand@sigma * object@system@supply@sigma)))[object@system@demand@separation_subset]) %*% object@system@demand@independent_matrix[object@system@demand@separation_subset, ] +
    t(object@system@supply@independent_matrix[object@system@supply@separation_subset, ] * c((-object@system@supply@psi * object@system@rho1 * object@system@rho2 * (object@system@supply@Psi * object@system@demand@z - object@system@supply@psi) / (object@system@supply@Psi ** 2 * object@system@demand@sigma * object@system@supply@sigma)))[object@system@supply@separation_subset]) %*% object@system@demand@independent_matrix[object@system@supply@separation_subset, ]
})

setGeneric("partial_beta_d_partial_var_d_of_loglh", function(object) {
  standardGeneric("partial_beta_d_partial_var_d_of_loglh")
})

setMethod("partial_beta_d_partial_var_d_of_loglh", signature(object = "diseq_directional"), function(object) {
  colSums(c((object@system@demand@Psi * object@system@demand@psi * object@system@rho2 * (object@system@demand@h * object@system@rho2 * object@system@supply@z + 1) - object@system@demand@h * (2 * object@system@demand@Psi ** 2 + object@system@demand@psi ** 2 * object@system@rho2 ** 2)) / (2 * object@system@demand@Psi ** 2 * object@system@demand@sigma ** 3))[object@system@demand@separation_subset] * object@system@demand@independent_matrix[object@system@demand@separation_subset, ]) +
    colSums(c(object@system@supply@psi * object@system@rho1 * (object@system@supply@Psi * (object@system@demand@h * object@system@rho1 * object@system@demand@z - 1) - object@system@demand@h * object@system@supply@psi * object@system@rho1) / (2 * object@system@supply@Psi ** 2 * object@system@demand@sigma ** 3))[object@system@supply@separation_subset] * object@system@demand@independent_matrix[object@system@supply@separation_subset, ])
})

setGeneric("partial_beta_d_partial_var_s_of_loglh", function(object) {
  standardGeneric("partial_beta_d_partial_var_s_of_loglh")
})

setMethod("partial_beta_d_partial_var_s_of_loglh", signature(object = "diseq_directional"), function(object) {
  colSums(c(-object@system@supply@h * object@system@demand@psi * object@system@rho1 * object@system@rho2 * (object@system@demand@Psi * object@system@supply@z - object@system@demand@psi) / (2 * object@system@demand@Psi ** 2 * object@system@demand@sigma * object@system@supply@var))[object@system@demand@separation_subset] * object@system@demand@independent_matrix[object@system@demand@separation_subset, ]) +
    colSums(c(-object@system@supply@h * object@system@supply@psi * object@system@rho1 * object@system@rho2 * (object@system@supply@Psi * object@system@demand@z - object@system@supply@psi) / (2 * object@system@supply@Psi ** 2 * object@system@demand@sigma * object@system@supply@var))[object@system@supply@separation_subset] * object@system@demand@independent_matrix[object@system@supply@separation_subset, ])
})

setGeneric("partial_beta_d_partial_rho_of_loglh", function(object) {
  standardGeneric("partial_beta_d_partial_rho_of_loglh")
})

setMethod("partial_beta_d_partial_rho_of_loglh", signature(object = "diseq_directional"), function(object) {
  colSums(c(-object@system@demand@psi * object@system@rho1 ** 2 * (object@system@demand@Psi * (object@system@rho1 + object@system@rho2 * object@system@demand@z * object@system@supply@z) - object@system@demand@psi * object@system@rho2 * object@system@demand@z) / (object@system@demand@Psi ** 2 * object@system@demand@sigma))[object@system@demand@separation_subset] * object@system@demand@independent_matrix[object@system@demand@separation_subset, ]) +
    colSums(c(object@system@supply@psi * object@system@rho1 ** 2 * (object@system@supply@Psi * (object@system@rho1 * object@system@demand@z * object@system@supply@z + object@system@rho2) - object@system@supply@psi * object@system@rho1 * object@system@supply@z) / (object@system@supply@Psi ** 2 * object@system@demand@sigma))[object@system@supply@separation_subset] * object@system@demand@independent_matrix[object@system@supply@separation_subset, ])
})

setGeneric("partial_beta_s_partial_beta_s_of_loglh", function(object) {
  standardGeneric("partial_beta_s_partial_beta_s_of_loglh")
})

setMethod("partial_beta_s_partial_beta_s_of_loglh", signature(object = "diseq_directional"), function(object) {
  t(object@system@supply@independent_matrix[object@system@demand@separation_subset, ] * c((object@system@demand@psi * object@system@rho1 ** 2 * (object@system@demand@Psi * object@system@supply@z - object@system@demand@psi) / (object@system@demand@Psi ** 2 * object@system@supply@var)))[object@system@demand@separation_subset]) %*% object@system@supply@independent_matrix[object@system@demand@separation_subset, ] +
    t(object@system@supply@independent_matrix[object@system@supply@separation_subset, ] * c(((-object@system@supply@Psi ** 2 + object@system@supply@Psi * object@system@supply@psi * object@system@rho2 ** 2 * object@system@demand@z - object@system@supply@psi ** 2 * object@system@rho2 ** 2) / (object@system@supply@Psi ** 2 * object@system@supply@var)))[object@system@supply@separation_subset]) %*% object@system@supply@independent_matrix[object@system@supply@separation_subset, ]
})

setGeneric("partial_beta_s_partial_var_d_of_loglh", function(object) {
  standardGeneric("partial_beta_s_partial_var_d_of_loglh")
})

setMethod("partial_beta_s_partial_var_d_of_loglh", signature(object = "diseq_directional"), function(object) {
  colSums(c(-object@system@demand@h * object@system@demand@psi * object@system@rho1 * object@system@rho2 * (object@system@demand@Psi * object@system@supply@z - object@system@demand@psi) / (2 * object@system@demand@Psi ** 2 * object@system@demand@var * object@system@supply@sigma))[object@system@demand@separation_subset] * object@system@supply@independent_matrix[object@system@demand@separation_subset, ]) +
    colSums(c(-object@system@demand@h * object@system@supply@psi * object@system@rho1 * object@system@rho2 * (object@system@supply@Psi * object@system@demand@z - object@system@supply@psi) / (2 * object@system@supply@Psi ** 2 * object@system@demand@var * object@system@supply@sigma))[object@system@supply@separation_subset] * object@system@supply@independent_matrix[object@system@supply@separation_subset, ])
})

setGeneric("partial_beta_s_partial_var_s_of_loglh", function(object) {
  standardGeneric("partial_beta_s_partial_var_s_of_loglh")
})

setMethod("partial_beta_s_partial_var_s_of_loglh", signature(object = "diseq_directional"), function(object) {
  colSums(c(object@system@demand@psi * object@system@rho1 * (object@system@demand@Psi * (object@system@supply@h * object@system@rho1 * object@system@supply@z - 1) - object@system@supply@h * object@system@demand@psi * object@system@rho1) / (2 * object@system@demand@Psi ** 2 * object@system@supply@sigma ** 3))[object@system@demand@separation_subset] * object@system@supply@independent_matrix[object@system@demand@separation_subset, ]) +
    colSums(c((object@system@supply@Psi * object@system@supply@psi * object@system@rho2 * (object@system@supply@h * object@system@rho2 * object@system@demand@z + 1) - object@system@supply@h * (2 * object@system@supply@Psi ** 2 + object@system@supply@psi ** 2 * object@system@rho2 ** 2)) / (2 * object@system@supply@Psi ** 2 * object@system@supply@sigma ** 3))[object@system@supply@separation_subset] * object@system@supply@independent_matrix[object@system@supply@separation_subset, ])
})

setGeneric("partial_beta_s_partial_rho_of_loglh", function(object) {
  standardGeneric("partial_beta_s_partial_rho_of_loglh")
})

setMethod("partial_beta_s_partial_rho_of_loglh", signature(object = "diseq_directional"), function(object) {
  colSums(c(object@system@demand@psi * object@system@rho1 ** 2 * (object@system@demand@Psi * (object@system@rho1 * object@system@demand@z * object@system@supply@z + object@system@rho2) - object@system@demand@psi * object@system@rho1 * object@system@demand@z) / (object@system@demand@Psi ** 2 * object@system@supply@sigma))[object@system@demand@separation_subset] * object@system@supply@independent_matrix[object@system@demand@separation_subset, ]) +
    colSums(c(-object@system@supply@psi * object@system@rho1 ** 2 * (object@system@supply@Psi * (object@system@rho1 + object@system@rho2 * object@system@demand@z * object@system@supply@z) - object@system@supply@psi * object@system@rho2 * object@system@supply@z) / (object@system@supply@Psi ** 2 * object@system@supply@sigma))[object@system@supply@separation_subset] * object@system@supply@independent_matrix[object@system@supply@separation_subset, ])
})

setGeneric("partial_var_d_partial_var_d_of_loglh", function(object) {
  standardGeneric("partial_var_d_partial_var_d_of_loglh")
})

setMethod("partial_var_d_partial_var_d_of_loglh", signature(object = "diseq_directional"), function(object) {
  sum(((2 * object@system@demand@Psi ** 2 * (1 - 2 * object@system@demand@h ** 2) + object@system@demand@Psi * object@system@demand@h * object@system@demand@psi * object@system@rho2 * (object@system@demand@h * object@system@rho2 * object@system@supply@z + 3) - object@system@demand@h ** 2 * object@system@demand@psi ** 2 * object@system@rho2 ** 2) / (4 * object@system@demand@Psi ** 2 * object@system@demand@sigma ** 4))[object@system@demand@separation_subset]) +
    sum((object@system@demand@h * object@system@supply@psi * object@system@rho1 * (object@system@supply@Psi * (object@system@demand@h * object@system@rho1 * object@system@demand@z - 3) - object@system@demand@h * object@system@supply@psi * object@system@rho1) / (4 * object@system@supply@Psi ** 2 * object@system@demand@sigma ** 4))[object@system@supply@separation_subset])
})

setGeneric("partial_var_d_partial_var_s_of_loglh", function(object) {
  standardGeneric("partial_var_d_partial_var_s_of_loglh")
})

setMethod("partial_var_d_partial_var_s_of_loglh", signature(object = "diseq_directional"), function(object) {
  sum((-object@system@demand@h * object@system@supply@h * object@system@demand@psi * object@system@rho1 * object@system@rho2 * (object@system@demand@Psi * object@system@supply@z - object@system@demand@psi) / (4 * object@system@demand@Psi ** 2 * object@system@demand@var * object@system@supply@var))[object@system@demand@separation_subset]) +
    sum((-object@system@demand@h * object@system@supply@h * object@system@supply@psi * object@system@rho1 * object@system@rho2 * (object@system@supply@Psi * object@system@demand@z - object@system@supply@psi) / (4 * object@system@supply@Psi ** 2 * object@system@demand@var * object@system@supply@var))[object@system@supply@separation_subset])
})

setGeneric("partial_var_d_partial_rho_of_loglh", function(object) {
  standardGeneric("partial_var_d_partial_rho_of_loglh")
})

setMethod("partial_var_d_partial_rho_of_loglh", signature(object = "diseq_directional"), function(object) {
  sum((-object@system@demand@h * object@system@demand@psi * object@system@rho1 ** 2 * (object@system@demand@Psi * (object@system@rho1 + object@system@rho2 * object@system@demand@z * object@system@supply@z) - object@system@demand@psi * object@system@rho2 * object@system@demand@z) / (2 * object@system@demand@Psi ** 2 * object@system@demand@var))[object@system@demand@separation_subset]) +
    sum((object@system@demand@h * object@system@supply@psi * object@system@rho1 ** 2 * (object@system@supply@Psi * (object@system@rho1 * object@system@demand@z * object@system@supply@z + object@system@rho2) - object@system@supply@psi * object@system@rho1 * object@system@supply@z) / (2 * object@system@supply@Psi ** 2 * object@system@demand@var))[object@system@supply@separation_subset])
})

setGeneric("partial_var_s_partial_var_s_of_loglh", function(object) {
  standardGeneric("partial_var_s_partial_var_s_of_loglh")
})

setMethod("partial_var_s_partial_var_s_of_loglh", signature(object = "diseq_directional"), function(object) {
  sum((object@system@supply@h * object@system@demand@psi * object@system@rho1 * (object@system@demand@Psi * (object@system@supply@h * object@system@rho1 * object@system@supply@z - 3) - object@system@supply@h * object@system@demand@psi * object@system@rho1) / (4 * object@system@demand@Psi ** 2 * object@system@supply@sigma ** 4))[object@system@demand@separation_subset]) +
    sum(((2 * object@system@supply@Psi ** 2 * (1 - 2 * object@system@supply@h ** 2) + object@system@supply@Psi * object@system@supply@h * object@system@supply@psi * object@system@rho2 * (object@system@supply@h * object@system@rho2 * object@system@demand@z + 3) - object@system@supply@h ** 2 * object@system@supply@psi ** 2 * object@system@rho2 ** 2) / (4 * object@system@supply@Psi ** 2 * object@system@supply@sigma ** 4))[object@system@supply@separation_subset])
})

setGeneric("partial_var_s_partial_rho_of_loglh", function(object) {
  standardGeneric("partial_var_s_partial_rho_of_loglh")
})

setMethod("partial_var_s_partial_rho_of_loglh", signature(object = "diseq_directional"), function(object) {
  sum((object@system@supply@h * object@system@demand@psi * object@system@rho1 ** 2 * (object@system@demand@Psi * (object@system@rho1 * object@system@demand@z * object@system@supply@z + object@system@rho2) - object@system@demand@psi * object@system@rho1 * object@system@demand@z) / (2 * object@system@demand@Psi ** 2 * object@system@supply@var))[object@system@demand@separation_subset]) +
    sum((-object@system@supply@h * object@system@supply@psi * object@system@rho1 ** 2 * (object@system@supply@Psi * (object@system@rho1 + object@system@rho2 * object@system@demand@z * object@system@supply@z) - object@system@supply@psi * object@system@rho2 * object@system@supply@z) / (2 * object@system@supply@Psi ** 2 * object@system@supply@var))[object@system@supply@separation_subset])
})

setGeneric("partial_rho_partial_rho_of_loglh", function(object) {
  standardGeneric("partial_rho_partial_rho_of_loglh")
})

setMethod("partial_rho_partial_rho_of_loglh", signature(object = "diseq_directional"), function(object) {
  sum((object@system@demand@psi * object@system@rho1 ** 3 * (object@system@demand@Psi * (3 * object@system@demand@h * object@system@rho1 * object@system@rho2 - object@system@supply@h * (3 * object@system@rho2 ** 2 + 1) + object@system@rho1 * object@system@demand@z ** 2 * object@system@supply@z) - object@system@demand@psi * object@system@rho1 * object@system@demand@z ** 2) / object@system@demand@Psi ** 2)[object@system@demand@separation_subset]) +
    sum((object@system@supply@psi * object@system@rho1 ** 3 * (object@system@supply@Psi * (-object@system@demand@h * (3 * object@system@rho2 ** 2 + 1) + 3 * object@system@supply@h * object@system@rho1 * object@system@rho2 + object@system@rho1 * object@system@demand@z * object@system@supply@z ** 2) - object@system@supply@psi * object@system@rho1 * object@system@supply@z ** 2) / object@system@supply@Psi ** 2)[object@system@supply@separation_subset])
})
