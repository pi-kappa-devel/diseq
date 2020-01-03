#' Basic disequilibrium model with unknown sample separation.
#'
#' @include diseq_base.R
#' @name diseq_basic-class
#' @export
setClass(
  "diseq_basic",
  contains = "diseq_base",
  representation(),
  prototype()
)

setMethod("initialize", "diseq_basic", function(
  .Object,
  verbose = 0,
  key_columns,
  quantity_column, price_column, demand_specification, supply_specification,
  use_correlated_shocks = TRUE,
  data
) {
  .Object <- callNextMethod(
    .Object,
    "Basic Model", verbose,
    key_columns, NULL,
    quantity_column, price_column, demand_specification, supply_specification, NULL,
    use_correlated_shocks,
    data,
    function(...) new("system_basic", ...)
  )

  .Object
})

#' @rdname minus_log_likelihood
setMethod("minus_log_likelihood", signature(object = "diseq_basic"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  -sum(log(object@system@lh))
})

setMethod("gradient", signature(object = "diseq_basic"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)

  g <- rep(NA, length(get_likelihood_variables(object@system)))
  names(g) <- get_likelihood_variables(object@system)
  g[colnames(object@system@demand@independent_matrix)] <- colSums(partial_beta_d_of_loglh(object))
  g[colnames(object@system@supply@independent_matrix)] <- colSums(partial_beta_s_of_loglh(object))
  g[get_prefixed_variance_variable(object@system@demand)] <- sum(partial_var_d_of_loglh(object))
  g[get_prefixed_variance_variable(object@system@supply)] <- sum(partial_var_s_of_loglh(object))
  if (object@system@correlated_shocks) {
    g[get_correlation_variable(object@system)] <- sum(partial_rho_of_loglh(object))
  }

  as.matrix(-g)
})

setMethod("hessian", signature(object = "diseq_basic"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)

  pbeta_dpbeta_d <- (
    t(object@system@demand@independent_matrix * c(((object@system@supply@psi * object@system@rho1 ** 2 * object@system@demand@sigma * object@system@demand@z + object@system@supply@sigma * (object@system@demand@Psi * (object@system@demand@h ** 2 - 1) - object@system@demand@psi * object@system@rho1 * (object@system@supply@h - object@system@rho1 * object@system@supply@z))) / (object@system@demand@sigma ** 3 * object@system@supply@sigma))  /  object@system@lh)) %*% object@system@demand@independent_matrix - t(partial_beta_d_of_loglh(object)) %*% partial_beta_d_of_loglh(object)
  )

  pbeta_dpbeta_s <- (
    t(object@system@supply@independent_matrix * c((object@system@rho1 * (object@system@demand@psi * object@system@supply@sigma * (object@system@demand@h - object@system@rho2 * object@system@supply@z) + object@system@supply@psi * object@system@demand@sigma * (object@system@supply@h - object@system@rho2 * object@system@demand@z)) / (object@system@demand@var * object@system@supply@var))  /  object@system@lh)) %*% object@system@demand@independent_matrix - t(partial_beta_s_of_loglh(object)) %*% partial_beta_d_of_loglh(object)
  )

  pbeta_dpvar_d <- (
    colSums(c(((object@system@supply@psi * object@system@rho1 * object@system@demand@sigma * (object@system@demand@h * object@system@rho1 * object@system@demand@z - 1) + object@system@supply@sigma * (object@system@demand@Psi * object@system@demand@h ** 3 - object@system@demand@h * (3 * object@system@demand@Psi + object@system@demand@psi * (2 * object@system@supply@h * object@system@rho1 - object@system@supply@z * (object@system@rho1 ** 2 + 1))) + 2 * object@system@demand@psi * object@system@rho2)) / (2 * object@system@demand@sigma ** 4 * object@system@supply@sigma))  /  object@system@lh) * object@system@demand@independent_matrix) - t(partial_var_d_of_loglh(object)) %*% partial_beta_d_of_loglh(object)
  )

  pbeta_dpvar_s <- (
    colSums(c((object@system@rho1 * (object@system@demand@psi * object@system@supply@sigma * (object@system@demand@h * (object@system@supply@h - object@system@rho1 * object@system@supply@z) + object@system@demand@z * object@system@supply@z) - object@system@supply@psi * object@system@demand@sigma * (object@system@demand@h * object@system@rho1 * object@system@demand@z - object@system@supply@h ** 2 - object@system@demand@z ** 2 + 1)) / (2 * object@system@demand@var * object@system@supply@sigma ** 3))  /  object@system@lh) * object@system@demand@independent_matrix) - t(partial_var_s_of_loglh(object)) %*% partial_beta_d_of_loglh(object)
  )

  pbeta_spbeta_s <- (
    t(object@system@supply@independent_matrix * c(((object@system@demand@psi * object@system@rho1 ** 2 * object@system@supply@sigma * object@system@supply@z + object@system@demand@sigma * (object@system@supply@Psi * (object@system@supply@h ** 2 - 1) - object@system@supply@psi * object@system@rho1 * (object@system@demand@h - object@system@rho1 * object@system@demand@z))) / (object@system@demand@sigma * object@system@supply@sigma ** 3))  /  object@system@lh)) %*% object@system@supply@independent_matrix - t(partial_beta_s_of_loglh(object)) %*% partial_beta_s_of_loglh(object)
  )

  pbeta_spvar_d <- (
    colSums(c((object@system@rho1 * (object@system@demand@psi * object@system@supply@sigma * (object@system@demand@h ** 2 - object@system@supply@h * object@system@rho1 * object@system@supply@z + object@system@supply@z ** 2 - 1) + object@system@supply@psi * object@system@demand@sigma * (object@system@supply@h * (object@system@demand@h - object@system@rho1 * object@system@demand@z) + object@system@demand@z * object@system@supply@z)) / (2 * object@system@demand@sigma ** 3 * object@system@supply@var))  /  object@system@lh) * object@system@supply@independent_matrix) - t(partial_var_d_of_loglh(object)) %*% partial_beta_s_of_loglh(object)
  )

  pbeta_spvar_s <- (
    colSums(c(((object@system@demand@psi * object@system@rho1 * object@system@supply@sigma * (object@system@supply@h * object@system@rho1 * object@system@supply@z - 1) + object@system@demand@sigma * (object@system@supply@Psi * object@system@supply@h ** 3 - object@system@supply@h * (3 * object@system@supply@Psi + object@system@supply@psi * (2 * object@system@demand@h * object@system@rho1 - object@system@demand@z * (object@system@rho1 ** 2 + 1))) + 2 * object@system@supply@psi * object@system@rho2)) / (2 * object@system@demand@sigma * object@system@supply@sigma ** 4))  /  object@system@lh) * object@system@supply@independent_matrix) - t(partial_var_s_of_loglh(object)) %*% partial_beta_s_of_loglh(object)
  )

  pvar_dpvar_d <- (
    sum(((object@system@demand@h * object@system@supply@psi * object@system@rho1 * object@system@demand@sigma * (object@system@demand@h * object@system@rho1 * object@system@demand@z - 3) + object@system@supply@sigma * (object@system@demand@Psi * (object@system@demand@h ** 4 + 3) + object@system@demand@h ** 2 * (-6 * object@system@demand@Psi + object@system@demand@psi * object@system@supply@z * (object@system@rho1 ** 2 + 1)) - object@system@demand@psi * (object@system@supply@h * object@system@rho1 * (2 * object@system@demand@h ** 2 - 3) + 3 * object@system@supply@z))) / (4 * object@system@demand@sigma ** 5 * object@system@supply@sigma))  /  object@system@lh) - t(partial_var_d_of_loglh(object)) %*% partial_var_d_of_loglh(object)
  )

  pvar_dpvar_s <- (
    sum((object@system@rho1 * (object@system@supply@h * object@system@demand@psi * object@system@supply@sigma * (object@system@demand@h ** 2 - object@system@supply@h * object@system@rho1 * object@system@supply@z + object@system@supply@z ** 2 - 1) + object@system@supply@psi * object@system@demand@sigma * (object@system@demand@h * (object@system@supply@h ** 2 - 1) - object@system@supply@h * object@system@demand@z * (object@system@supply@h * object@system@rho1 - object@system@supply@z))) / (4 * object@system@demand@sigma ** 3 * object@system@supply@sigma ** 3))  /  object@system@lh) - t(partial_var_s_of_loglh(object)) %*% partial_var_d_of_loglh(object)
  )

  pvar_spvar_s <- (
    sum(((object@system@supply@h * object@system@demand@psi * object@system@rho1 * object@system@supply@sigma * (object@system@supply@h * object@system@rho1 * object@system@supply@z - 3) + object@system@demand@sigma * (object@system@supply@Psi * (object@system@supply@h ** 4 + 3) + object@system@supply@h ** 2 * (-6 * object@system@supply@Psi + object@system@supply@psi * object@system@demand@z * (object@system@rho1 ** 2 + 1)) - object@system@supply@psi * (object@system@demand@h * object@system@rho1 * (2 * object@system@supply@h ** 2 - 3) + 3 * object@system@demand@z))) / (4 * object@system@demand@sigma * object@system@supply@sigma ** 5))  /  object@system@lh) - t(partial_var_s_of_loglh(object)) %*% partial_var_s_of_loglh(object)
  )

  if (object@system@correlated_shocks) {

    pbeta_dprho <- (
      colSums(c((-object@system@rho1 ** 2 * (object@system@demand@psi * object@system@supply@sigma * (object@system@rho1 - object@system@demand@z * (object@system@demand@h - object@system@rho2 * object@system@supply@z)) - object@system@supply@psi * object@system@demand@sigma * (object@system@rho1 * object@system@demand@z * object@system@supply@z + object@system@rho2)) / (object@system@demand@var * object@system@supply@sigma))  /  object@system@lh) * object@system@demand@independent_matrix) - t(partial_rho_of_loglh(object)) %*% partial_beta_d_of_loglh(object)
    )

    pbeta_sprho <- (
      colSums(c((object@system@rho1 ** 2 * (object@system@demand@psi * object@system@supply@sigma * (object@system@rho1 * object@system@demand@z * object@system@supply@z + object@system@rho2) - object@system@supply@psi * object@system@demand@sigma * (object@system@rho1 - object@system@supply@z * (object@system@supply@h - object@system@rho2 * object@system@demand@z))) / (object@system@demand@sigma * object@system@supply@var))  /  object@system@lh) * object@system@supply@independent_matrix) - t(partial_rho_of_loglh(object)) %*% partial_beta_s_of_loglh(object)
    )

    pvar_dprho <- (
      sum((object@system@rho1 ** 2 * (-object@system@demand@psi * object@system@supply@sigma * (object@system@rho1 * (object@system@demand@h + object@system@supply@h * object@system@demand@z * object@system@supply@z) - object@system@demand@z * (object@system@demand@h ** 2 + object@system@supply@z ** 2 - 1)) + object@system@supply@psi * object@system@demand@sigma * (object@system@rho1 * (object@system@demand@h * object@system@demand@z * object@system@supply@z + object@system@supply@h) - object@system@supply@z)) / (2 * object@system@demand@sigma ** 3 * object@system@supply@sigma))  /  object@system@lh) - t(partial_rho_of_loglh(object)) %*% partial_var_d_of_loglh(object)
    )

    pvar_sprho <- (
      sum((object@system@rho1 ** 2 * (object@system@demand@psi * object@system@supply@sigma * (object@system@rho1 * (object@system@demand@h + object@system@supply@h * object@system@demand@z * object@system@supply@z) - object@system@demand@z) - object@system@supply@psi * object@system@demand@sigma * (object@system@rho1 * (object@system@demand@h * object@system@demand@z * object@system@supply@z + object@system@supply@h) - object@system@supply@z * (object@system@supply@h ** 2 + object@system@demand@z ** 2 - 1))) / (2 * object@system@demand@sigma * object@system@supply@sigma ** 3))  /  object@system@lh) - t(partial_rho_of_loglh(object)) %*% partial_var_s_of_loglh(object)
    )

    prhoprho <- (
      sum((object@system@rho1 ** 3 * (object@system@demand@psi * object@system@supply@sigma * (2 * object@system@supply@h + object@system@rho1 * object@system@supply@z * (object@system@demand@z ** 2 - 3)) + object@system@supply@psi * object@system@demand@sigma * (2 * object@system@demand@h + object@system@rho1 * object@system@demand@z * (object@system@supply@z ** 2 - 3))) / (object@system@demand@sigma * object@system@supply@sigma))  /  object@system@lh) - t(partial_rho_of_loglh(object)) %*% partial_rho_of_loglh(object)
    )

  }

  h <- rep(0.0, length(get_likelihood_variables(object@system)))
  names(h) <- get_likelihood_variables(object@system)
  h <- h %*% t(h)
  rownames(h) <- colnames(h)

  h[rownames(pbeta_dpbeta_d), colnames(pbeta_dpbeta_d)] <- pbeta_dpbeta_d

  h[rownames(pbeta_dpbeta_s), colnames(pbeta_dpbeta_s)] <- pbeta_dpbeta_s
  h[colnames(pbeta_dpbeta_s), rownames(pbeta_dpbeta_s)] <- t(pbeta_dpbeta_s)

  h[get_prefixed_variance_variable(object@system@demand), colnames(pbeta_dpvar_d)] <- t(pbeta_dpvar_d)
  h[colnames(pbeta_dpvar_d), get_prefixed_variance_variable(object@system@demand)] <- pbeta_dpvar_d

  h[get_prefixed_variance_variable(object@system@supply), colnames(pbeta_dpvar_s)] <- t(pbeta_dpvar_s)
  h[colnames(pbeta_dpvar_s), get_prefixed_variance_variable(object@system@supply)] <- pbeta_dpvar_s

  h[get_prefixed_variance_variable(object@system@demand), get_prefixed_variance_variable(object@system@demand)] <-
    pvar_dpvar_d

  h[get_prefixed_variance_variable(object@system@demand), get_prefixed_variance_variable(object@system@supply)] <-
    pvar_dpvar_s
  h[get_prefixed_variance_variable(object@system@supply), get_prefixed_variance_variable(object@system@demand)] <-
    pvar_dpvar_s

  h[rownames(pbeta_spbeta_s), colnames(pbeta_spbeta_s)] <- pbeta_spbeta_s

  h[get_prefixed_variance_variable(object@system@supply), colnames(pbeta_spvar_s)] <- t(pbeta_spvar_s)
  h[colnames(pbeta_spvar_s), get_prefixed_variance_variable(object@system@supply)] <- pbeta_spvar_s

  h[get_prefixed_variance_variable(object@system@demand), colnames(pbeta_spvar_d)] <- t(pbeta_spvar_d)
  h[colnames(pbeta_spvar_d), get_prefixed_variance_variable(object@system@demand)] <- pbeta_spvar_d

  h[get_prefixed_variance_variable(object@system@supply), get_prefixed_variance_variable(object@system@supply)] <-
    pvar_spvar_s

  if (object@system@correlated_shocks) {
    h[get_correlation_variable(object@system), colnames(pbeta_dprho)] <- t(pbeta_dprho)
    h[colnames(pbeta_dprho), get_correlation_variable(object@system)] <- pbeta_dprho

    h[get_prefixed_variance_variable(object@system@demand), get_correlation_variable(object@system)] <-
      pvar_dprho
    h[get_correlation_variable(object@system), get_prefixed_variance_variable(object@system@demand)] <-
      pvar_dprho

    h[get_correlation_variable(object@system), colnames(pbeta_sprho)] <- t(pbeta_sprho)
    h[colnames(pbeta_sprho), get_correlation_variable(object@system)] <- pbeta_sprho

    h[get_prefixed_variance_variable(object@system@supply), get_correlation_variable(object@system)] <-
      pvar_sprho
    h[get_correlation_variable(object@system), get_prefixed_variance_variable(object@system@supply)] <-
      pvar_sprho

    h[get_correlation_variable(object@system), get_correlation_variable(object@system)] <- prhoprho
  }

  -h
})

setGeneric("partial_beta_d_of_loglh", function(object) {
  standardGeneric("partial_beta_d_of_loglh")
})

setMethod("partial_beta_d_of_loglh", signature(object = "diseq_basic"), function(object) {
  c(((object@system@supply@psi * object@system@rho1 / (object@system@demand@sigma * object@system@supply@sigma) + (object@system@demand@Psi * object@system@demand@h - object@system@demand@psi * object@system@rho2) / object@system@demand@var))  /  object@system@lh) * object@system@demand@independent_matrix
})

setGeneric("partial_beta_s_of_loglh", function(object) {
  standardGeneric("partial_beta_s_of_loglh")
})

setMethod("partial_beta_s_of_loglh", signature(object = "diseq_basic"), function(object) {
  c(((object@system@demand@psi * object@system@rho1 / (object@system@demand@sigma * object@system@supply@sigma) + (object@system@supply@Psi * object@system@supply@h - object@system@supply@psi * object@system@rho2) / object@system@supply@var))  /  object@system@lh) * object@system@supply@independent_matrix
})

setGeneric("partial_var_d_of_loglh", function(object) {
  standardGeneric("partial_var_d_of_loglh")
})

setMethod("partial_var_d_of_loglh", signature(object = "diseq_basic"), function(object) {
  c((object@system@demand@h * object@system@supply@psi * object@system@rho1 / (2 * object@system@demand@var * object@system@supply@sigma) + (object@system@demand@Psi * (object@system@demand@h ** 2 - 1) - object@system@demand@h * object@system@demand@psi * object@system@rho2) / (2 * object@system@demand@sigma ** 3))  /  object@system@lh)
})

setGeneric("partial_var_s_of_loglh", function(object) {
  standardGeneric("partial_var_s_of_loglh")
})

setMethod("partial_var_s_of_loglh", signature(object = "diseq_basic"), function(object) {
  c((object@system@supply@h * object@system@demand@psi * object@system@rho1 / (2 * object@system@demand@sigma * object@system@supply@var) + (object@system@supply@Psi * (object@system@supply@h ** 2 - 1) - object@system@supply@h * object@system@supply@psi * object@system@rho2) / (2 * object@system@supply@sigma ** 3))  /  object@system@lh)
})

setGeneric("partial_rho_of_loglh", function(object) {
  standardGeneric("partial_rho_of_loglh")
})

setMethod("partial_rho_of_loglh", signature(object = "diseq_basic"), function(object) {
  c((object@system@rho1 ** 2 * (object@system@demand@psi * object@system@demand@z / object@system@demand@sigma + object@system@supply@psi * object@system@supply@z / object@system@supply@sigma))  /  object@system@lh)
})
