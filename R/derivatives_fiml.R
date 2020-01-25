#' @include system_fiml.R


setGeneric("partial_alpha_d", function(object) {
  standardGeneric("partial_alpha_d")
})

setMethod("partial_alpha_d", signature(object = "system_fiml"), function(object) {
  # nolint start
  (object@delta ** 2 * object@mu_P * object@rho1_QP * object@var_Q * object@z_PQ - object@delta ** 2 * object@rho1_QP * object@sigma_P * object@sigma_Q * object@z_QP * (object@supply@control_matrix %*% object@supply@beta - object@mu_Q) + object@delta ** 2 * object@sigma_P * object@var_Q * (object@h_P * object@rho1_QP * object@z_PQ - 1) + object@delta * object@sigma_P * (object@h_Q * object@rho1_QP * object@z_QP - 1) * (object@demand@alpha * object@supply@sigma ** 2 - object@supply@alpha * object@rho * object@demand@sigma * object@supply@sigma + object@delta * object@var_Q) - object@rho1_QP * (object@delta * object@rho_QP * object@sigma_P * (object@demand@alpha * object@supply@sigma ** 2 - object@supply@alpha * object@rho * object@demand@sigma * object@supply@sigma + 2 * object@delta * object@var_Q) + object@sigma_Q * (-2 * object@supply@alpha * object@demand@sigma ** 2 + object@rho * object@demand@sigma * object@supply@sigma * (object@demand@alpha + 3 * object@supply@alpha) - object@supply@sigma ** 2 * (object@demand@alpha + object@supply@alpha))) * (object@h_P * object@h_Q * object@rho1_QP * (object@rho1_QP ** 2 + object@rho2_QP ** 2) - object@rho1_QP ** 2 * object@rho2_QP * (object@h_P ** 2 + object@h_Q ** 2) + object@rho2_QP)) / (object@delta ** 3 * object@sigma_P * object@var_Q)
  # nolint end
})

setGeneric("partial_beta_d", function(object) {
  standardGeneric("partial_beta_d")
})

setMethod("partial_beta_d", signature(object = "system_fiml"), function(object) {
  # nolint start
  object@demand@control_matrix * object@rho1_QP * c(object@supply@alpha * object@sigma_P * object@z_QP + object@sigma_Q * object@z_PQ) / (object@delta * object@sigma_P * object@sigma_Q)
  # nolint end
})

setGeneric("partial_alpha_s", function(object) {
  standardGeneric("partial_alpha_s")
})

setMethod("partial_alpha_s", signature(object = "system_fiml"), function(object) {
  # nolint start
  (-object@delta ** 2 * object@mu_P * object@rho1_QP * object@var_Q * object@z_PQ + object@delta ** 2 * object@rho1_QP * object@sigma_P * object@sigma_Q * object@z_QP * (object@demand@control_matrix %*% object@demand@beta - object@mu_Q) + object@delta ** 2 * object@sigma_P * object@var_Q * (-object@h_P * object@rho1_QP * object@z_PQ + 1) - object@delta * object@sigma_P * (object@h_Q * object@rho1_QP * object@z_QP - 1) * (object@demand@alpha * object@rho * object@demand@sigma * object@supply@sigma - object@supply@alpha * object@demand@sigma ** 2 + object@delta * object@var_Q) + object@rho1_QP * (object@delta * object@rho_QP * object@sigma_P * (object@demand@alpha * object@rho * object@demand@sigma * object@supply@sigma - object@supply@alpha * object@demand@sigma ** 2 + 2 * object@delta * object@var_Q) + object@sigma_Q * (-2 * object@demand@alpha * object@supply@sigma ** 2 + object@rho * object@demand@sigma * object@supply@sigma * (3 * object@demand@alpha + object@supply@alpha) - object@demand@sigma ** 2 * (object@demand@alpha + object@supply@alpha))) * (object@h_P * object@h_Q * object@rho1_QP * (object@rho1_QP ** 2 + object@rho2_QP ** 2) - object@rho1_QP ** 2 * object@rho2_QP * (object@h_P ** 2 + object@h_Q ** 2) + object@rho2_QP)) / (object@delta ** 3 * object@sigma_P * object@var_Q)
  # nolint end
})

setGeneric("partial_beta_s", function(object) {
  standardGeneric("partial_beta_s")
})

setMethod("partial_beta_s", signature(object = "system_fiml"), function(object) {
  # nolint start
  -object@supply@control_matrix * object@rho1_QP * c(object@demand@alpha * object@sigma_P * object@z_QP + object@sigma_Q * object@z_PQ) / (object@delta * object@sigma_P * object@sigma_Q)
  # nolint end
})

setGeneric("partial_var_d", function(object) {
  standardGeneric("partial_var_d")
})

setMethod("partial_var_d", signature(object = "system_fiml"), function(object) {
  # nolint start
  (-object@supply@alpha * object@var_P * (object@demand@alpha * object@rho * object@supply@sigma - object@supply@alpha * object@demand@sigma) * (object@h_Q * object@rho1_QP * object@z_QP - 1) + object@rho1_QP * (object@rho_QP * (object@supply@alpha * object@var_P * (object@demand@alpha * object@rho * object@supply@sigma - object@supply@alpha * object@demand@sigma) + object@var_Q * (object@rho * object@supply@sigma - object@demand@sigma)) + object@sigma_P * object@sigma_Q * (2 * object@supply@alpha * object@demand@sigma - object@rho * object@supply@sigma * (object@demand@alpha + object@supply@alpha))) * (object@h_P * object@h_Q * object@rho1_QP * (object@rho1_QP ** 2 + object@rho2_QP ** 2) - object@rho1_QP ** 2 * object@rho2_QP * (object@h_P ** 2 + object@h_Q ** 2) + object@rho2_QP) - object@var_Q * (object@rho * object@supply@sigma - object@demand@sigma) * (object@h_P * object@rho1_QP * object@z_PQ - 1)) / (2 * object@delta ** 2 * object@var_P * object@var_Q * object@demand@sigma)
  # nolint end
})

setGeneric("partial_var_s", function(object) {
  standardGeneric("partial_var_s")
})

setMethod("partial_var_s", signature(object = "system_fiml"), function(object) {
  # nolint start
  (object@demand@alpha * object@var_P * (object@demand@alpha * object@supply@sigma - object@supply@alpha * object@rho * object@demand@sigma) * (object@h_Q * object@rho1_QP * object@z_QP - 1) - object@rho1_QP * (object@rho_QP * (object@demand@alpha * object@var_P * (object@demand@alpha * object@supply@sigma - object@supply@alpha * object@rho * object@demand@sigma) + object@var_Q * (-object@rho * object@demand@sigma + object@supply@sigma)) + object@sigma_P * object@sigma_Q * (-2 * object@demand@alpha * object@supply@sigma + object@rho * object@demand@sigma * (object@demand@alpha + object@supply@alpha))) * (object@h_P * object@h_Q * object@rho1_QP * (object@rho1_QP ** 2 + object@rho2_QP ** 2) - object@rho1_QP ** 2 * object@rho2_QP * (object@h_P ** 2 + object@h_Q ** 2) + object@rho2_QP) - object@var_Q * (object@rho * object@demand@sigma - object@supply@sigma) * (object@h_P * object@rho1_QP * object@z_PQ - 1)) / (2 * object@delta ** 2 * object@var_P * object@var_Q * object@supply@sigma)
  # nolint end
})

setGeneric("partial_rho", function(object) {
  standardGeneric("partial_rho")
})

setMethod("partial_rho", signature(object = "system_fiml"), function(object) {
  # nolint start
  object@demand@sigma * object@supply@sigma * (-object@demand@alpha * object@supply@alpha * object@var_P * (object@h_Q * object@rho1_QP * object@z_QP - 1) + object@rho1_QP * (object@rho_QP * (object@demand@alpha * object@supply@alpha * object@var_P + object@var_Q) - object@sigma_P * object@sigma_Q * (object@demand@alpha + object@supply@alpha)) * (object@h_P * object@h_Q * object@rho1_QP * (object@rho1_QP ** 2 + object@rho2_QP ** 2) - object@rho1_QP ** 2 * object@rho2_QP * (object@h_P ** 2 + object@h_Q ** 2) + object@rho2_QP) - object@var_Q * (object@h_P * object@rho1_QP * object@z_PQ - 1)) / (object@delta ** 2 * object@var_P * object@var_Q)
  # nolint end
})
