#' @include system_fiml.R

setMethod(
  "calculate_system_moments", signature(object = "system_fiml"),
  function(object) {
    xbd <- object@demand@control_matrix %*% object@demand@beta
    xbs <- object@supply@control_matrix %*% object@supply@beta

    # nolint start
    object@mu_P <- ((xbd - xbs) / object@delta)
    object@var_P <- ((-2 * object@rho * object@demand@sigma * object@supply@sigma + object@demand@sigma**2 + object@supply@sigma**2) / object@delta**2)
    object@mu_Q <- ((object@supply@alpha * xbd - object@demand@alpha * xbs) / object@delta)
    object@var_Q <- ((object@demand@alpha**2 * object@supply@sigma**2 - 2 * object@demand@alpha * object@supply@alpha * object@rho * object@demand@sigma * object@supply@sigma + object@supply@alpha**2 * object@demand@sigma**2) / object@delta**2)
    object@cov_QP <- ((object@demand@alpha * object@supply@sigma**2 + object@supply@alpha * object@demand@sigma**2 - object@rho * object@demand@sigma * object@supply@sigma * (object@demand@alpha + object@supply@alpha)) / object@delta**2)
    # nolint end

    object@sigma_P <- sqrt(object@var_P)
    object@sigma_Q <- sqrt(object@var_Q)

    object@h_P <- (object@price_vector - object@mu_P) / object@sigma_P
    object@h_Q <- (object@quantity_vector - object@mu_Q) / object@sigma_Q

    object@rho_QP <- object@cov_QP / (object@sigma_P * object@sigma_Q)
    if (is.na(object@rho_QP) || abs(object@rho_QP) >= 1) {
      object@rho_QP <- NA_real_
    }
    object@rho_1QP <- 1 / sqrt(1 - object@rho_QP**2)
    object@rho_2QP <- object@rho_QP * object@rho_1QP

    object@z_PQ <- object@rho_1QP * object@h_P - object@rho_2QP * object@h_Q
    object@z_QP <- object@rho_1QP * object@h_Q - object@rho_2QP * object@h_P

    object
  }
)

setMethod(
  "calculate_system_loglikelihood", signature(object = "system_fiml"),
  function(object) {
    # nolint start
    (-object@rho_1QP * (object@h_P * object@z_PQ + object@h_Q * object@z_QP) / 2 + log(object@rho_1QP) - log(object@sigma_P) - log(object@sigma_Q) - log(pi) - log(2))
    # nolint end
  }
)
