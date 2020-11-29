#' @include system_directional.R


setGeneric("partial_beta_d_of_loglh_d", function(object) {
  standardGeneric("partial_beta_d_of_loglh_d")
})

setMethod("partial_beta_d_of_loglh_d", signature(object = "system_directional"), function(object) {
  # nolint start
  c((object@demand@Psi * object@demand@h - object@demand@psi * object@rho2) / (object@demand@Psi * object@demand@sigma)) * object@demand@independent_matrix
  # nolint end
})

setGeneric("partial_beta_d_of_loglh_s", function(object) {
  standardGeneric("partial_beta_d_of_loglh_s")
})

setMethod("partial_beta_d_of_loglh_s", signature(object = "system_directional"), function(object) {
  # nolint start
  c(object@supply@psi * object@rho1 / (object@supply@Psi * object@demand@sigma)) * object@demand@independent_matrix
  # nolint end
})

setGeneric("partial_beta_s_of_loglh_d", function(object) {
  standardGeneric("partial_beta_s_of_loglh_d")
})

setMethod("partial_beta_s_of_loglh_d", signature(object = "system_directional"), function(object) {
  # nolint start
  c(object@demand@psi * object@rho1 / (object@demand@Psi * object@supply@sigma)) * object@supply@independent_matrix
  # nolint end
})

setGeneric("partial_beta_s_of_loglh_s", function(object) {
  standardGeneric("partial_beta_s_of_loglh_s")
})

setMethod("partial_beta_s_of_loglh_s", signature(object = "system_directional"), function(object) {
  # nolint start
  c((object@supply@Psi * object@supply@h - object@supply@psi * object@rho2) / (object@supply@Psi * object@supply@sigma)) * object@supply@independent_matrix
  # nolint end
})

setGeneric("partial_var_d_of_loglh_d", function(object) {
  standardGeneric("partial_var_d_of_loglh_d")
})

setMethod("partial_var_d_of_loglh_d", signature(object = "system_directional"), function(object) {
  # nolint start
  c((object@demand@Psi * (object@demand@h**2 - 1) - object@demand@h * object@demand@psi * object@rho2) / (2 * object@demand@Psi * object@demand@var))
  # nolint end
})

setGeneric("partial_var_d_of_loglh_s", function(object) {
  standardGeneric("partial_var_d_of_loglh_s")
})

setMethod("partial_var_d_of_loglh_s", signature(object = "system_directional"), function(object) {
  # nolint start
  c(object@demand@h * object@supply@psi * object@rho1 / (2 * object@supply@Psi * object@demand@var))
  # nolint end
})

setGeneric("partial_var_s_of_loglh_d", function(object) {
  standardGeneric("partial_var_s_of_loglh_d")
})

setMethod("partial_var_s_of_loglh_d", signature(object = "system_directional"), function(object) {
  # nolint start
  c(object@supply@h * object@demand@psi * object@rho1 / (2 * object@demand@Psi * object@supply@var))
  # nolint end
})

setGeneric("partial_var_s_of_loglh_s", function(object) {
  standardGeneric("partial_var_s_of_loglh_s")
})

setMethod("partial_var_s_of_loglh_s", signature(object = "system_directional"), function(object) {
  # nolint start
  c((object@supply@Psi * (object@supply@h**2 - 1) - object@supply@h * object@supply@psi * object@rho2) / (2 * object@supply@Psi * object@supply@var))
  # nolint end
})

setGeneric("partial_rho_of_loglh_d", function(object) {
  standardGeneric("partial_rho_of_loglh_d")
})

setMethod("partial_rho_of_loglh_d", signature(object = "system_directional"), function(object) {
  # nolint start
  c(object@demand@psi * object@rho1**2 * object@demand@z / object@demand@Psi)
  # nolint end
})

setGeneric("partial_rho_of_loglh_s", function(object) {
  standardGeneric("partial_rho_of_loglh_s")
})

setMethod("partial_rho_of_loglh_s", signature(object = "system_directional"), function(object) {
  # nolint start
  c(object@supply@psi * object@rho1**2 * object@supply@z / object@supply@Psi)
  # nolint end
})

setMethod("partial_beta_d_partial_beta_d_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  t(object@demand@independent_matrix[object@demand@separation_subset, ] * c(((-object@demand@Psi**2 + object@demand@Psi * object@demand@psi * object@rho2**2 * object@supply@z - object@demand@psi**2 * object@rho2**2) / (object@demand@Psi**2 * object@demand@var)))[object@demand@separation_subset]) %*% object@demand@independent_matrix[object@demand@separation_subset, ] + t(object@demand@independent_matrix[object@supply@separation_subset, ] * c((object@supply@psi * object@rho1**2 * (object@supply@Psi * object@demand@z - object@supply@psi) / (object@supply@Psi**2 * object@demand@var)))[object@supply@separation_subset]) %*% object@demand@independent_matrix[object@supply@separation_subset, ]
  # nolint end
})

setMethod("partial_beta_d_partial_beta_s_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  t(object@supply@independent_matrix[object@demand@separation_subset, ] * c((-object@demand@psi * object@rho1 * object@rho2 * (object@demand@Psi * object@supply@z - object@demand@psi) / (object@demand@Psi**2 * object@demand@sigma * object@supply@sigma)))[object@demand@separation_subset]) %*% object@demand@independent_matrix[object@demand@separation_subset, ] + t(object@supply@independent_matrix[object@supply@separation_subset, ] * c((-object@supply@psi * object@rho1 * object@rho2 * (object@supply@Psi * object@demand@z - object@supply@psi) / (object@supply@Psi**2 * object@demand@sigma * object@supply@sigma)))[object@supply@separation_subset]) %*% object@demand@independent_matrix[object@supply@separation_subset, ]
  # nolint end
})

setMethod("partial_beta_d_partial_var_d_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  colSums(c((object@demand@Psi * object@demand@psi * object@rho2 * (object@demand@h * object@rho2 * object@supply@z + 1) - object@demand@h * (2 * object@demand@Psi**2 + object@demand@psi**2 * object@rho2**2)) / (2 * object@demand@Psi**2 * object@demand@sigma**3))[object@demand@separation_subset] * object@demand@independent_matrix[object@demand@separation_subset, ]) + colSums(c(object@supply@psi * object@rho1 * (object@supply@Psi * (object@demand@h * object@rho1 * object@demand@z - 1) - object@demand@h * object@supply@psi * object@rho1) / (2 * object@supply@Psi**2 * object@demand@sigma**3))[object@supply@separation_subset] * object@demand@independent_matrix[object@supply@separation_subset, ])
  # nolint end
})

setMethod("partial_beta_d_partial_var_s_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  colSums(c(-object@supply@h * object@demand@psi * object@rho1 * object@rho2 * (object@demand@Psi * object@supply@z - object@demand@psi) / (2 * object@demand@Psi**2 * object@demand@sigma * object@supply@var))[object@demand@separation_subset] * object@demand@independent_matrix[object@demand@separation_subset, ]) + colSums(c(-object@supply@h * object@supply@psi * object@rho1 * object@rho2 * (object@supply@Psi * object@demand@z - object@supply@psi) / (2 * object@supply@Psi**2 * object@demand@sigma * object@supply@var))[object@supply@separation_subset] * object@demand@independent_matrix[object@supply@separation_subset, ])
  # nolint end
})

setMethod("partial_beta_d_partial_rho_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  colSums(c(-object@demand@psi * object@rho1**2 * (object@demand@Psi * (object@rho1 + object@rho2 * object@demand@z * object@supply@z) - object@demand@psi * object@rho2 * object@demand@z) / (object@demand@Psi**2 * object@demand@sigma))[object@demand@separation_subset] * object@demand@independent_matrix[object@demand@separation_subset, ]) + colSums(c(object@supply@psi * object@rho1**2 * (object@supply@Psi * (object@rho1 * object@demand@z * object@supply@z + object@rho2) - object@supply@psi * object@rho1 * object@supply@z) / (object@supply@Psi**2 * object@demand@sigma))[object@supply@separation_subset] * object@demand@independent_matrix[object@supply@separation_subset, ])
  # nolint end
})

setMethod("partial_beta_s_partial_beta_s_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  t(object@supply@independent_matrix[object@demand@separation_subset, ] * c((object@demand@psi * object@rho1**2 * (object@demand@Psi * object@supply@z - object@demand@psi) / (object@demand@Psi**2 * object@supply@var)))[object@demand@separation_subset]) %*% object@supply@independent_matrix[object@demand@separation_subset, ] + t(object@supply@independent_matrix[object@supply@separation_subset, ] * c(((-object@supply@Psi**2 + object@supply@Psi * object@supply@psi * object@rho2**2 * object@demand@z - object@supply@psi**2 * object@rho2**2) / (object@supply@Psi**2 * object@supply@var)))[object@supply@separation_subset]) %*% object@supply@independent_matrix[object@supply@separation_subset, ]
  # nolint end
})

setMethod("partial_beta_s_partial_var_d_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  colSums(c(-object@demand@h * object@demand@psi * object@rho1 * object@rho2 * (object@demand@Psi * object@supply@z - object@demand@psi) / (2 * object@demand@Psi**2 * object@demand@var * object@supply@sigma))[object@demand@separation_subset] * object@supply@independent_matrix[object@demand@separation_subset, ]) + colSums(c(-object@demand@h * object@supply@psi * object@rho1 * object@rho2 * (object@supply@Psi * object@demand@z - object@supply@psi) / (2 * object@supply@Psi**2 * object@demand@var * object@supply@sigma))[object@supply@separation_subset] * object@supply@independent_matrix[object@supply@separation_subset, ])
  # nolint end
})

setMethod("partial_beta_s_partial_var_s_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  colSums(c(object@demand@psi * object@rho1 * (object@demand@Psi * (object@supply@h * object@rho1 * object@supply@z - 1) - object@supply@h * object@demand@psi * object@rho1) / (2 * object@demand@Psi**2 * object@supply@sigma**3))[object@demand@separation_subset] * object@supply@independent_matrix[object@demand@separation_subset, ]) + colSums(c((object@supply@Psi * object@supply@psi * object@rho2 * (object@supply@h * object@rho2 * object@demand@z + 1) - object@supply@h * (2 * object@supply@Psi**2 + object@supply@psi**2 * object@rho2**2)) / (2 * object@supply@Psi**2 * object@supply@sigma**3))[object@supply@separation_subset] * object@supply@independent_matrix[object@supply@separation_subset, ])
  # nolint end
})

setMethod("partial_beta_s_partial_rho_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  colSums(c(object@demand@psi * object@rho1**2 * (object@demand@Psi * (object@rho1 * object@demand@z * object@supply@z + object@rho2) - object@demand@psi * object@rho1 * object@demand@z) / (object@demand@Psi**2 * object@supply@sigma))[object@demand@separation_subset] * object@supply@independent_matrix[object@demand@separation_subset, ]) + colSums(c(-object@supply@psi * object@rho1**2 * (object@supply@Psi * (object@rho1 + object@rho2 * object@demand@z * object@supply@z) - object@supply@psi * object@rho2 * object@supply@z) / (object@supply@Psi**2 * object@supply@sigma))[object@supply@separation_subset] * object@supply@independent_matrix[object@supply@separation_subset, ])
  # nolint end
})

setMethod("partial_var_d_partial_var_d_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  sum(((2 * object@demand@Psi**2 * (1 - 2 * object@demand@h**2) + object@demand@Psi * object@demand@h * object@demand@psi * object@rho2 * (object@demand@h * object@rho2 * object@supply@z + 3) - object@demand@h**2 * object@demand@psi**2 * object@rho2**2) / (4 * object@demand@Psi**2 * object@demand@sigma**4))[object@demand@separation_subset]) + sum((object@demand@h * object@supply@psi * object@rho1 * (object@supply@Psi * (object@demand@h * object@rho1 * object@demand@z - 3) - object@demand@h * object@supply@psi * object@rho1) / (4 * object@supply@Psi**2 * object@demand@sigma**4))[object@supply@separation_subset])
  # nolint end
})

setMethod("partial_var_d_partial_var_s_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  sum((-object@demand@h * object@supply@h * object@demand@psi * object@rho1 * object@rho2 * (object@demand@Psi * object@supply@z - object@demand@psi) / (4 * object@demand@Psi**2 * object@demand@var * object@supply@var))[object@demand@separation_subset]) + sum((-object@demand@h * object@supply@h * object@supply@psi * object@rho1 * object@rho2 * (object@supply@Psi * object@demand@z - object@supply@psi) / (4 * object@supply@Psi**2 * object@demand@var * object@supply@var))[object@supply@separation_subset])
  # nolint end
})

setMethod("partial_var_d_partial_rho_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  sum((-object@demand@h * object@demand@psi * object@rho1**2 * (object@demand@Psi * (object@rho1 + object@rho2 * object@demand@z * object@supply@z) - object@demand@psi * object@rho2 * object@demand@z) / (2 * object@demand@Psi**2 * object@demand@var))[object@demand@separation_subset]) + sum((object@demand@h * object@supply@psi * object@rho1**2 * (object@supply@Psi * (object@rho1 * object@demand@z * object@supply@z + object@rho2) - object@supply@psi * object@rho1 * object@supply@z) / (2 * object@supply@Psi**2 * object@demand@var))[object@supply@separation_subset])
  # nolint end
})

setMethod("partial_var_s_partial_var_s_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  sum((object@supply@h * object@demand@psi * object@rho1 * (object@demand@Psi * (object@supply@h * object@rho1 * object@supply@z - 3) - object@supply@h * object@demand@psi * object@rho1) / (4 * object@demand@Psi**2 * object@supply@sigma**4))[object@demand@separation_subset]) + sum(((2 * object@supply@Psi**2 * (1 - 2 * object@supply@h**2) + object@supply@Psi * object@supply@h * object@supply@psi * object@rho2 * (object@supply@h * object@rho2 * object@demand@z + 3) - object@supply@h**2 * object@supply@psi**2 * object@rho2**2) / (4 * object@supply@Psi**2 * object@supply@sigma**4))[object@supply@separation_subset])
  # nolint end
})

setMethod("partial_var_s_partial_rho_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  sum((object@supply@h * object@demand@psi * object@rho1**2 * (object@demand@Psi * (object@rho1 * object@demand@z * object@supply@z + object@rho2) - object@demand@psi * object@rho1 * object@demand@z) / (2 * object@demand@Psi**2 * object@supply@var))[object@demand@separation_subset]) + sum((-object@supply@h * object@supply@psi * object@rho1**2 * (object@supply@Psi * (object@rho1 + object@rho2 * object@demand@z * object@supply@z) - object@supply@psi * object@rho2 * object@supply@z) / (2 * object@supply@Psi**2 * object@supply@var))[object@supply@separation_subset])
  # nolint end
})

setMethod("partial_rho_partial_rho_of_loglh", signature(object = "system_directional"), function(object) {
  # nolint start
  sum((object@demand@psi * object@rho1**3 * (object@demand@Psi * (3 * object@demand@h * object@rho1 * object@rho2 - object@supply@h * (3 * object@rho2**2 + 1) + object@rho1 * object@demand@z**2 * object@supply@z) - object@demand@psi * object@rho1 * object@demand@z**2) / object@demand@Psi**2)[object@demand@separation_subset]) + sum((object@supply@psi * object@rho1**3 * (object@supply@Psi * (-object@demand@h * (3 * object@rho2**2 + 1) + 3 * object@supply@h * object@rho1 * object@rho2 + object@rho1 * object@demand@z * object@supply@z**2) - object@supply@psi * object@rho1 * object@supply@z**2) / object@supply@Psi**2)[object@supply@separation_subset])
  # nolint end
})
