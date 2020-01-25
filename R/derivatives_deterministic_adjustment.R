#' @include system_deterministic_adjustment.R


setGeneric("partial_alpha_d_of_loglh_D", function(object) {
  standardGeneric("partial_alpha_d_of_loglh_D")
})

setMethod("partial_alpha_d_of_loglh_D", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  (object@delta * object@mu_P * object@demand@rho1_PQ * object@demand@var_Q * object@demand@z_PQ + object@delta * object@mu_P * object@demand@rho1_PQ * object@demand@sigma_Q * object@sigma_P * object@demand@z_QP * (object@supply@alpha + object@gamma) + object@delta * object@demand@var_Q * object@sigma_P * (object@h_P * object@demand@rho1_PQ * object@demand@z_PQ - 1) - object@demand@rho1_PQ * (object@demand@rho_PQ * object@sigma_P * (object@delta * object@demand@var_Q + (object@supply@alpha + object@gamma) * (object@demand@alpha * object@delta * object@var_P - object@rho * object@demand@sigma * object@supply@sigma + object@demand@sigma ** 2)) + object@demand@sigma_Q * (object@rho * object@demand@sigma * object@supply@sigma + object@var_P * (object@demand@alpha ** 2 - object@supply@alpha ** 2 - 2 * object@supply@alpha * object@gamma - object@gamma ** 2) - object@demand@sigma ** 2)) * (object@demand@h_Q * object@h_P * object@demand@rho1_PQ * (object@demand@rho1_PQ ** 2 + object@demand@rho2_PQ ** 2) - object@demand@rho1_PQ ** 2 * object@demand@rho2_PQ * (object@demand@h_Q ** 2 + object@h_P ** 2) + object@demand@rho2_PQ) + object@sigma_P * (object@supply@alpha + object@gamma) * (object@demand@h_Q * object@demand@rho1_PQ * object@demand@z_QP - 1) * (object@demand@alpha * object@delta * object@var_P - object@rho * object@demand@sigma * object@supply@sigma + object@demand@sigma ** 2)) / (object@delta ** 2 * object@demand@var_Q * object@sigma_P)
  # nolint end
})

setGeneric("partial_beta_d_of_loglh_D", function(object) {
  standardGeneric("partial_beta_d_of_loglh_D")
})

setMethod("partial_beta_d_of_loglh_D", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  object@demand@control_matrix * object@demand@rho1_PQ * c(object@demand@sigma_Q * object@demand@z_PQ + object@sigma_P * object@demand@z_QP * (object@supply@alpha + object@gamma)) / (object@delta * object@demand@sigma_Q * object@sigma_P)
  # nolint end
})

setGeneric("partial_alpha_s_of_loglh_D", function(object) {
  standardGeneric("partial_alpha_s_of_loglh_D")
})

setMethod("partial_alpha_s_of_loglh_D", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  (-object@demand@alpha * object@delta * object@mu_P * object@demand@rho1_PQ * object@demand@sigma_Q * object@sigma_P * object@demand@z_QP - object@demand@alpha * object@sigma_P * (object@demand@h_Q * object@demand@rho1_PQ * object@demand@z_QP - 1) * (object@demand@alpha * object@delta * object@var_P - object@rho * object@demand@sigma * object@supply@sigma + object@demand@sigma ** 2) - object@delta * object@mu_P * object@demand@rho1_PQ * object@demand@var_Q * object@demand@z_PQ + object@delta * object@demand@var_Q * object@sigma_P * (-object@h_P * object@demand@rho1_PQ * object@demand@z_PQ + 1) + object@demand@rho1_PQ * (object@demand@rho_PQ * object@sigma_P * (object@demand@alpha * (object@demand@alpha * object@delta * object@var_P - object@rho * object@demand@sigma * object@supply@sigma + object@demand@sigma ** 2) + object@delta * object@demand@var_Q) + object@demand@sigma_Q * (-2 * object@demand@alpha * object@delta * object@var_P + object@rho * object@demand@sigma * object@supply@sigma - object@demand@sigma ** 2)) * (object@demand@h_Q * object@h_P * object@demand@rho1_PQ * (object@demand@rho1_PQ ** 2 + object@demand@rho2_PQ ** 2) - object@demand@rho1_PQ ** 2 * object@demand@rho2_PQ * (object@demand@h_Q ** 2 + object@h_P ** 2) + object@demand@rho2_PQ)) / (object@delta ** 2 * object@demand@var_Q * object@sigma_P)
  # nolint end
})

setGeneric("partial_beta_s_of_loglh_D", function(object) {
  standardGeneric("partial_beta_s_of_loglh_D")
})

setMethod("partial_beta_s_of_loglh_D", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  -object@supply@control_matrix * object@demand@rho1_PQ * c(object@demand@alpha * object@sigma_P * object@demand@z_QP + object@demand@sigma_Q * object@demand@z_PQ) / (object@delta * object@demand@sigma_Q * object@sigma_P)
  # nolint end
})

setGeneric("partial_gamma_of_loglh_D", function(object) {
  standardGeneric("partial_gamma_of_loglh_D")
})

setMethod("partial_gamma_of_loglh_D", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  (object@demand@alpha * object@delta * object@demand@rho1_PQ * object@demand@sigma_Q * object@sigma_P * object@demand@z_QP * (object@lagged_price_vector - object@mu_P) - object@demand@alpha * object@sigma_P * (object@demand@h_Q * object@demand@rho1_PQ * object@demand@z_QP - 1) * (object@demand@alpha * object@delta * object@var_P - object@rho * object@demand@sigma * object@supply@sigma + object@demand@sigma ** 2) + object@delta * object@demand@rho1_PQ * object@demand@var_Q * object@demand@z_PQ * (object@lagged_price_vector - object@mu_P) + object@delta * object@demand@var_Q * object@sigma_P * (-object@h_P * object@demand@rho1_PQ * object@demand@z_PQ + 1) + object@demand@rho1_PQ * (object@demand@rho_PQ * object@sigma_P * (object@demand@alpha * (object@demand@alpha * object@delta * object@var_P - object@rho * object@demand@sigma * object@supply@sigma + object@demand@sigma ** 2) + object@delta * object@demand@var_Q) + object@demand@sigma_Q * (-2 * object@demand@alpha * object@delta * object@var_P + object@rho * object@demand@sigma * object@supply@sigma - object@demand@sigma ** 2)) * (object@demand@h_Q * object@h_P * object@demand@rho1_PQ * (object@demand@rho1_PQ ** 2 + object@demand@rho2_PQ ** 2) - object@demand@rho1_PQ ** 2 * object@demand@rho2_PQ * (object@demand@h_Q ** 2 + object@h_P ** 2) + object@demand@rho2_PQ)) / (object@delta ** 2 * object@demand@var_Q * object@sigma_P)
  # nolint end
})

setGeneric("partial_sigma_d_of_loglh_D", function(object) {
  standardGeneric("partial_sigma_d_of_loglh_D")
})

setMethod("partial_sigma_d_of_loglh_D", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  -(-object@demand@rho1_PQ * (object@demand@rho_PQ * (object@demand@var_Q * (object@rho * object@supply@sigma - object@demand@sigma) + object@var_P * (object@supply@alpha + object@gamma) * (object@demand@alpha * object@rho * object@supply@sigma - object@demand@sigma * (object@supply@alpha + object@gamma))) - object@demand@sigma_Q * object@sigma_P * (object@rho * object@supply@sigma * (object@demand@alpha + object@supply@alpha + object@gamma) - 2 * object@demand@sigma * (object@supply@alpha + object@gamma))) * (object@demand@h_Q * object@h_P * object@demand@rho1_PQ * (object@demand@rho1_PQ ** 2 + object@demand@rho2_PQ ** 2) - object@demand@rho1_PQ ** 2 * object@demand@rho2_PQ * (object@demand@h_Q ** 2 + object@h_P ** 2) + object@demand@rho2_PQ) + object@demand@var_Q * (object@rho * object@supply@sigma - object@demand@sigma) * (object@h_P * object@demand@rho1_PQ * object@demand@z_PQ - 1) + object@var_P * (object@supply@alpha + object@gamma) * (object@demand@alpha * object@rho * object@supply@sigma - object@demand@sigma * (object@supply@alpha + object@gamma)) * (object@demand@h_Q * object@demand@rho1_PQ * object@demand@z_QP - 1)) / (2 * object@delta ** 2 * object@demand@var_Q * object@var_P * object@demand@sigma)
  # nolint end
})

setGeneric("partial_sigma_s_of_loglh_D", function(object) {
  standardGeneric("partial_sigma_s_of_loglh_D")
})

setMethod("partial_sigma_s_of_loglh_D", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  (object@demand@alpha * object@var_P * (object@demand@alpha * object@supply@sigma - object@rho * object@demand@sigma * (object@supply@alpha + object@gamma)) * (object@demand@h_Q * object@demand@rho1_PQ * object@demand@z_QP - 1) - object@demand@rho1_PQ * (object@demand@rho_PQ * (object@demand@alpha * object@var_P * (object@demand@alpha * object@supply@sigma - object@rho * object@demand@sigma * (object@supply@alpha + object@gamma)) + object@demand@var_Q * (-object@rho * object@demand@sigma + object@supply@sigma)) + object@demand@sigma_Q * object@sigma_P * (-2 * object@demand@alpha * object@supply@sigma + object@rho * object@demand@sigma * (object@demand@alpha + object@supply@alpha + object@gamma))) * (object@demand@h_Q * object@h_P * object@demand@rho1_PQ * (object@demand@rho1_PQ ** 2 + object@demand@rho2_PQ ** 2) - object@demand@rho1_PQ ** 2 * object@demand@rho2_PQ * (object@demand@h_Q ** 2 + object@h_P ** 2) + object@demand@rho2_PQ) - object@demand@var_Q * (object@rho * object@demand@sigma - object@supply@sigma) * (object@h_P * object@demand@rho1_PQ * object@demand@z_PQ - 1)) / (2 * object@delta ** 2 * object@demand@var_Q * object@var_P * object@supply@sigma)
  # nolint end
})

setGeneric("partial_rho_of_loglh_D", function(object) {
  standardGeneric("partial_rho_of_loglh_D")
})

setMethod("partial_rho_of_loglh_D", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  object@demand@sigma * object@supply@sigma * (-object@demand@alpha * object@var_P * (object@supply@alpha + object@gamma) * (object@demand@h_Q * object@demand@rho1_PQ * object@demand@z_QP - 1) + object@demand@rho1_PQ * (object@demand@rho_PQ * (object@demand@alpha * object@var_P * (object@supply@alpha + object@gamma) + object@demand@var_Q) - object@demand@sigma_Q * object@sigma_P * (object@demand@alpha + object@supply@alpha + object@gamma)) * (object@demand@h_Q * object@h_P * object@demand@rho1_PQ * (object@demand@rho1_PQ ** 2 + object@demand@rho2_PQ ** 2) - object@demand@rho1_PQ ** 2 * object@demand@rho2_PQ * (object@demand@h_Q ** 2 + object@h_P ** 2) + object@demand@rho2_PQ) - object@demand@var_Q * (object@h_P * object@demand@rho1_PQ * object@demand@z_PQ - 1)) / (object@delta ** 2 * object@demand@var_Q * object@var_P)
  # nolint end
})

setGeneric("partial_alpha_d_of_loglh_S", function(object) {
  standardGeneric("partial_alpha_d_of_loglh_S")
})

setMethod("partial_alpha_d_of_loglh_S", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  (object@supply@alpha * object@delta * object@mu_P * object@supply@rho1_PQ * object@sigma_P * object@supply@sigma_Q * object@supply@z_QP + object@supply@alpha * object@sigma_P * (object@supply@h_Q * object@supply@rho1_PQ * object@supply@z_QP - 1) * (object@supply@alpha * object@delta * object@var_P + object@rho * object@demand@sigma * object@supply@sigma - object@supply@sigma ** 2) + object@delta * object@mu_P * object@supply@rho1_PQ * object@supply@var_Q * object@supply@z_PQ + object@delta * object@sigma_P * object@supply@var_Q * (object@h_P * object@supply@rho1_PQ * object@supply@z_PQ - 1) - object@supply@rho1_PQ * (object@supply@rho_PQ * object@sigma_P * (object@supply@alpha * (object@supply@alpha * object@delta * object@var_P + object@rho * object@demand@sigma * object@supply@sigma - object@supply@sigma ** 2) + object@delta * object@supply@var_Q) + object@supply@sigma_Q * (-2 * object@supply@alpha * object@delta * object@var_P - object@rho * object@demand@sigma * object@supply@sigma + object@supply@sigma ** 2)) * (object@h_P * object@supply@h_Q * object@supply@rho1_PQ * (object@supply@rho1_PQ ** 2 + object@supply@rho2_PQ ** 2) - object@supply@rho1_PQ ** 2 * object@supply@rho2_PQ * (object@h_P ** 2 + object@supply@h_Q ** 2) + object@supply@rho2_PQ)) / (object@delta ** 2 * object@sigma_P * object@supply@var_Q)
  # nolint end
})

setGeneric("partial_beta_d_of_loglh_S", function(object) {
  standardGeneric("partial_beta_d_of_loglh_S")
})

setMethod("partial_beta_d_of_loglh_S", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  object@demand@control_matrix * object@supply@rho1_PQ * c(object@supply@alpha * object@sigma_P * object@supply@z_QP + object@supply@sigma_Q * object@supply@z_PQ) / (object@delta * object@sigma_P * object@supply@sigma_Q)
  # nolint end
})

setGeneric("partial_alpha_s_of_loglh_S", function(object) {
  standardGeneric("partial_alpha_s_of_loglh_S")
})

setMethod("partial_alpha_s_of_loglh_S", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  (-object@delta * object@mu_P * object@supply@rho1_PQ * object@sigma_P * object@supply@sigma_Q * object@supply@z_QP * (object@demand@alpha - object@gamma) - object@delta * object@mu_P * object@supply@rho1_PQ * object@supply@var_Q * object@supply@z_PQ + object@delta * object@sigma_P * object@supply@var_Q * (-object@h_P * object@supply@rho1_PQ * object@supply@z_PQ + 1) + object@supply@rho1_PQ * (object@supply@rho_PQ * object@sigma_P * (object@delta * object@supply@var_Q + (object@demand@alpha - object@gamma) * (object@supply@alpha * object@delta * object@var_P + object@rho * object@demand@sigma * object@supply@sigma - object@supply@sigma ** 2)) + object@supply@sigma_Q * (-object@rho * object@demand@sigma * object@supply@sigma + object@var_P * (object@demand@alpha ** 2 - 2 * object@demand@alpha * object@gamma - object@supply@alpha ** 2 + object@gamma ** 2) + object@supply@sigma ** 2)) * (object@h_P * object@supply@h_Q * object@supply@rho1_PQ * (object@supply@rho1_PQ ** 2 + object@supply@rho2_PQ ** 2) - object@supply@rho1_PQ ** 2 * object@supply@rho2_PQ * (object@h_P ** 2 + object@supply@h_Q ** 2) + object@supply@rho2_PQ) - object@sigma_P * (object@demand@alpha - object@gamma) * (object@supply@h_Q * object@supply@rho1_PQ * object@supply@z_QP - 1) * (object@supply@alpha * object@delta * object@var_P + object@rho * object@demand@sigma * object@supply@sigma - object@supply@sigma ** 2)) / (object@delta ** 2 * object@sigma_P * object@supply@var_Q)
  # nolint end
})

setGeneric("partial_beta_s_of_loglh_S", function(object) {
  standardGeneric("partial_beta_s_of_loglh_S")
})

setMethod("partial_beta_s_of_loglh_S", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  -object@supply@control_matrix * object@supply@rho1_PQ * c(object@sigma_P * object@supply@z_QP * (object@demand@alpha - object@gamma) + object@supply@sigma_Q * object@supply@z_PQ) / (object@delta * object@sigma_P * object@supply@sigma_Q)
  # nolint end
})

setGeneric("partial_gamma_of_loglh_S", function(object) {
  standardGeneric("partial_gamma_of_loglh_S")
})

setMethod("partial_gamma_of_loglh_S", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  (object@supply@alpha * object@delta * object@supply@rho1_PQ * object@sigma_P * object@supply@sigma_Q * object@supply@z_QP * (object@lagged_price_vector - object@mu_P) - object@supply@alpha * object@sigma_P * (object@supply@h_Q * object@supply@rho1_PQ * object@supply@z_QP - 1) * (object@supply@alpha * object@delta * object@var_P + object@rho * object@demand@sigma * object@supply@sigma - object@supply@sigma ** 2) + object@delta * object@supply@rho1_PQ * object@supply@var_Q * object@supply@z_PQ * (object@lagged_price_vector - object@mu_P) + object@delta * object@sigma_P * object@supply@var_Q * (-object@h_P * object@supply@rho1_PQ * object@supply@z_PQ + 1) + object@supply@rho1_PQ * (object@supply@rho_PQ * object@sigma_P * (object@supply@alpha * (object@supply@alpha * object@delta * object@var_P + object@rho * object@demand@sigma * object@supply@sigma - object@supply@sigma ** 2) + object@delta * object@supply@var_Q) + object@supply@sigma_Q * (-2 * object@supply@alpha * object@delta * object@var_P - object@rho * object@demand@sigma * object@supply@sigma + object@supply@sigma ** 2)) * (object@h_P * object@supply@h_Q * object@supply@rho1_PQ * (object@supply@rho1_PQ ** 2 + object@supply@rho2_PQ ** 2) - object@supply@rho1_PQ ** 2 * object@supply@rho2_PQ * (object@h_P ** 2 + object@supply@h_Q ** 2) + object@supply@rho2_PQ)) / (object@delta ** 2 * object@sigma_P * object@supply@var_Q)
  # nolint end
})

setGeneric("partial_sigma_d_of_loglh_S", function(object) {
  standardGeneric("partial_sigma_d_of_loglh_S")
})

setMethod("partial_sigma_d_of_loglh_S", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  (object@supply@alpha * object@var_P * (object@supply@alpha * object@demand@sigma - object@rho * object@supply@sigma * (object@demand@alpha - object@gamma)) * (object@supply@h_Q * object@supply@rho1_PQ * object@supply@z_QP - 1) - object@supply@rho1_PQ * (object@supply@rho_PQ * (object@supply@alpha * object@var_P * (object@supply@alpha * object@demand@sigma - object@rho * object@supply@sigma * (object@demand@alpha - object@gamma)) + object@supply@var_Q * (-object@rho * object@supply@sigma + object@demand@sigma)) + object@sigma_P * object@supply@sigma_Q * (-2 * object@supply@alpha * object@demand@sigma + object@rho * object@supply@sigma * (object@demand@alpha + object@supply@alpha - object@gamma))) * (object@h_P * object@supply@h_Q * object@supply@rho1_PQ * (object@supply@rho1_PQ ** 2 + object@supply@rho2_PQ ** 2) - object@supply@rho1_PQ ** 2 * object@supply@rho2_PQ * (object@h_P ** 2 + object@supply@h_Q ** 2) + object@supply@rho2_PQ) - object@supply@var_Q * (object@rho * object@supply@sigma - object@demand@sigma) * (object@h_P * object@supply@rho1_PQ * object@supply@z_PQ - 1)) / (2 * object@delta ** 2 * object@var_P * object@supply@var_Q * object@demand@sigma)
  # nolint end
})

setGeneric("partial_sigma_s_of_loglh_S", function(object) {
  standardGeneric("partial_sigma_s_of_loglh_S")
})

setMethod("partial_sigma_s_of_loglh_S", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  (object@supply@rho1_PQ * (object@supply@rho_PQ * (object@var_P * (object@demand@alpha - object@gamma) * (object@supply@alpha * object@rho * object@demand@sigma - object@supply@sigma * (object@demand@alpha - object@gamma)) + object@supply@var_Q * (object@rho * object@demand@sigma - object@supply@sigma)) + object@sigma_P * object@supply@sigma_Q * (-object@rho * object@demand@sigma * (object@demand@alpha + object@supply@alpha - object@gamma) + 2 * object@supply@sigma * (object@demand@alpha - object@gamma))) * (object@h_P * object@supply@h_Q * object@supply@rho1_PQ * (object@supply@rho1_PQ ** 2 + object@supply@rho2_PQ ** 2) - object@supply@rho1_PQ ** 2 * object@supply@rho2_PQ * (object@h_P ** 2 + object@supply@h_Q ** 2) + object@supply@rho2_PQ) - object@var_P * (object@demand@alpha - object@gamma) * (object@supply@alpha * object@rho * object@demand@sigma - object@supply@sigma * (object@demand@alpha - object@gamma)) * (object@supply@h_Q * object@supply@rho1_PQ * object@supply@z_QP - 1) - object@supply@var_Q * (object@rho * object@demand@sigma - object@supply@sigma) * (object@h_P * object@supply@rho1_PQ * object@supply@z_PQ - 1)) / (2 * object@delta ** 2 * object@var_P * object@supply@var_Q * object@supply@sigma)
  # nolint end
})

setGeneric("partial_rho_of_loglh_S", function(object) {
  standardGeneric("partial_rho_of_loglh_S")
})

setMethod("partial_rho_of_loglh_S", signature(object = "system_deterministic_adjustment"), function(object) {
  # nolint start
  object@demand@sigma * object@supply@sigma * (-object@supply@alpha * object@var_P * (object@demand@alpha - object@gamma) * (object@supply@h_Q * object@supply@rho1_PQ * object@supply@z_QP - 1) + object@supply@rho1_PQ * (object@supply@rho_PQ * (object@supply@alpha * object@var_P * (object@demand@alpha - object@gamma) + object@supply@var_Q) - object@sigma_P * object@supply@sigma_Q * (object@demand@alpha + object@supply@alpha - object@gamma)) * (object@h_P * object@supply@h_Q * object@supply@rho1_PQ * (object@supply@rho1_PQ ** 2 + object@supply@rho2_PQ ** 2) - object@supply@rho1_PQ ** 2 * object@supply@rho2_PQ * (object@h_P ** 2 + object@supply@h_Q ** 2) + object@supply@rho2_PQ) - object@supply@var_Q * (object@h_P * object@supply@rho1_PQ * object@supply@z_PQ - 1)) / (object@delta ** 2 * object@var_P * object@supply@var_Q)
  # nolint end
})
