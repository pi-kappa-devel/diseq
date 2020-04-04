#' @include system_basic.R


setGeneric("partial_beta_d_of_loglh", function(object) {
  standardGeneric("partial_beta_d_of_loglh")
})

setMethod("partial_beta_d_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  c(((object@supply@psi * object@rho1 / (object@demand@sigma * object@supply@sigma) + (object@demand@Psi * object@demand@h - object@demand@psi * object@rho2) / object@demand@var)) / object@lh) * object@demand@independent_matrix
  # nolint end
})

setGeneric("partial_beta_s_of_loglh", function(object) {
  standardGeneric("partial_beta_s_of_loglh")
})

setMethod("partial_beta_s_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  c(((object@demand@psi * object@rho1 / (object@demand@sigma * object@supply@sigma) + (object@supply@Psi * object@supply@h - object@supply@psi * object@rho2) / object@supply@var)) / object@lh) * object@supply@independent_matrix
  # nolint end
})

setGeneric("partial_var_d_of_loglh", function(object) {
  standardGeneric("partial_var_d_of_loglh")
})

setMethod("partial_var_d_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  c((object@demand@h * object@supply@psi * object@rho1 / (2 * object@demand@var * object@supply@sigma) + (object@demand@Psi * (object@demand@h**2 - 1) - object@demand@h * object@demand@psi * object@rho2) / (2 * object@demand@sigma**3)) / object@lh)
  # nolint end
})

setGeneric("partial_var_s_of_loglh", function(object) {
  standardGeneric("partial_var_s_of_loglh")
})

setMethod("partial_var_s_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  c((object@supply@h * object@demand@psi * object@rho1 / (2 * object@demand@sigma * object@supply@var) + (object@supply@Psi * (object@supply@h**2 - 1) - object@supply@h * object@supply@psi * object@rho2) / (2 * object@supply@sigma**3)) / object@lh)
  # nolint end
})

setGeneric("partial_rho_of_loglh", function(object) {
  standardGeneric("partial_rho_of_loglh")
})

setMethod("partial_rho_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  c((object@rho1**2 * (object@demand@psi * object@demand@z / object@demand@sigma + object@supply@psi * object@supply@z / object@supply@sigma)) / object@lh)
  # nolint end
})

setGeneric("partial_beta_d_partial_beta_d_of_loglh", function(object) {
  standardGeneric("partial_beta_d_partial_beta_d_of_loglh")
})

setMethod("partial_beta_d_partial_beta_d_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (t(object@demand@independent_matrix * c(((object@supply@psi * object@rho1**2 * object@demand@sigma * object@demand@z + object@supply@sigma * (object@demand@Psi * (object@demand@h**2 - 1) - object@demand@psi * object@rho1 * (object@supply@h - object@rho1 * object@supply@z))) / (object@demand@sigma**3 * object@supply@sigma)) / object@lh)) %*% object@demand@independent_matrix) - t(partial_beta_d_of_loglh(object)) %*% partial_beta_d_of_loglh(object)
  # nolint end
})

setGeneric("partial_beta_d_partial_beta_s_of_loglh", function(object) {
  standardGeneric("partial_beta_d_partial_beta_s_of_loglh")
})

setMethod("partial_beta_d_partial_beta_s_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (t(object@supply@independent_matrix * c((object@rho1 * (object@demand@psi * object@supply@sigma * (object@demand@h - object@rho2 * object@supply@z) + object@supply@psi * object@demand@sigma * (object@supply@h - object@rho2 * object@demand@z)) / (object@demand@var * object@supply@var)) / object@lh)) %*% object@demand@independent_matrix) - t(partial_beta_s_of_loglh(object)) %*% partial_beta_d_of_loglh(object)
  # nolint end
})

setGeneric("partial_beta_d_partial_var_d_of_loglh", function(object) {
  standardGeneric("partial_beta_d_partial_var_d_of_loglh")
})

setMethod("partial_beta_d_partial_var_d_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (colSums(c(((object@supply@psi * object@rho1 * object@demand@sigma * (object@demand@h * object@rho1 * object@demand@z - 1) + object@supply@sigma * (object@demand@Psi * object@demand@h**3 - object@demand@h * (3 * object@demand@Psi + object@demand@psi * (2 * object@supply@h * object@rho1 - object@supply@z * (object@rho1**2 + 1))) + 2 * object@demand@psi * object@rho2)) / (2 * object@demand@sigma**4 * object@supply@sigma)) / object@lh) * object@demand@independent_matrix)) - t(partial_var_d_of_loglh(object)) %*% partial_beta_d_of_loglh(object)
  # nolint end
})

setGeneric("partial_beta_d_partial_var_s_of_loglh", function(object) {
  standardGeneric("partial_beta_d_partial_var_s_of_loglh")
})

setMethod("partial_beta_d_partial_var_s_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (colSums(c((object@rho1 * (object@demand@psi * object@supply@sigma * (object@demand@h * (object@supply@h - object@rho1 * object@supply@z) + object@demand@z * object@supply@z) - object@supply@psi * object@demand@sigma * (object@demand@h * object@rho1 * object@demand@z - object@supply@h**2 - object@demand@z**2 + 1)) / (2 * object@demand@var * object@supply@sigma**3)) / object@lh) * object@demand@independent_matrix)) - t(partial_var_s_of_loglh(object)) %*% partial_beta_d_of_loglh(object)
  # nolint end
})

setGeneric("partial_beta_d_partial_rho_of_loglh", function(object) {
  standardGeneric("partial_beta_d_partial_rho_of_loglh")
})

setMethod("partial_beta_d_partial_rho_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (colSums(c((-object@rho1**2 * (object@demand@psi * object@supply@sigma * (object@rho1 - object@demand@z * (object@demand@h - object@rho2 * object@supply@z)) - object@supply@psi * object@demand@sigma * (object@rho1 * object@demand@z * object@supply@z + object@rho2)) / (object@demand@var * object@supply@sigma)) / object@lh) * object@demand@independent_matrix)) - t(partial_rho_of_loglh(object)) %*% partial_beta_d_of_loglh(object)
  # nolint end
})

setGeneric("partial_beta_s_partial_beta_s_of_loglh", function(object) {
  standardGeneric("partial_beta_s_partial_beta_s_of_loglh")
})

setMethod("partial_beta_s_partial_beta_s_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (t(object@supply@independent_matrix * c(((object@demand@psi * object@rho1**2 * object@supply@sigma * object@supply@z + object@demand@sigma * (object@supply@Psi * (object@supply@h**2 - 1) - object@supply@psi * object@rho1 * (object@demand@h - object@rho1 * object@demand@z))) / (object@demand@sigma * object@supply@sigma**3)) / object@lh)) %*% object@supply@independent_matrix) - t(partial_beta_s_of_loglh(object)) %*% partial_beta_s_of_loglh(object)
  # nolint end
})

setGeneric("partial_beta_s_partial_var_d_of_loglh", function(object) {
  standardGeneric("partial_beta_s_partial_var_d_of_loglh")
})

setMethod("partial_beta_s_partial_var_d_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (colSums(c((object@rho1 * (object@demand@psi * object@supply@sigma * (object@demand@h**2 - object@supply@h * object@rho1 * object@supply@z + object@supply@z**2 - 1) + object@supply@psi * object@demand@sigma * (object@supply@h * (object@demand@h - object@rho1 * object@demand@z) + object@demand@z * object@supply@z)) / (2 * object@demand@sigma**3 * object@supply@var)) / object@lh) * object@supply@independent_matrix)) - t(partial_var_d_of_loglh(object)) %*% partial_beta_s_of_loglh(object)
  # nolint end
})

setGeneric("partial_beta_s_partial_var_s_of_loglh", function(object) {
  standardGeneric("partial_beta_s_partial_var_s_of_loglh")
})

setMethod("partial_beta_s_partial_var_s_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (colSums(c(((object@demand@psi * object@rho1 * object@supply@sigma * (object@supply@h * object@rho1 * object@supply@z - 1) + object@demand@sigma * (object@supply@Psi * object@supply@h**3 - object@supply@h * (3 * object@supply@Psi + object@supply@psi * (2 * object@demand@h * object@rho1 - object@demand@z * (object@rho1**2 + 1))) + 2 * object@supply@psi * object@rho2)) / (2 * object@demand@sigma * object@supply@sigma**4)) / object@lh) * object@supply@independent_matrix)) - t(partial_var_s_of_loglh(object)) %*% partial_beta_s_of_loglh(object)
  # nolint end
})

setGeneric("partial_beta_s_partial_rho_of_loglh", function(object) {
  standardGeneric("partial_beta_s_partial_rho_of_loglh")
})

setMethod("partial_beta_s_partial_rho_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (colSums(c((object@rho1**2 * (object@demand@psi * object@supply@sigma * (object@rho1 * object@demand@z * object@supply@z + object@rho2) - object@supply@psi * object@demand@sigma * (object@rho1 - object@supply@z * (object@supply@h - object@rho2 * object@demand@z))) / (object@demand@sigma * object@supply@var)) / object@lh) * object@supply@independent_matrix)) - t(partial_rho_of_loglh(object)) %*% partial_beta_s_of_loglh(object)
  # nolint end
})

setGeneric("partial_var_d_partial_var_d_of_loglh", function(object) {
  standardGeneric("partial_var_d_partial_var_d_of_loglh")
})

setMethod("partial_var_d_partial_var_d_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (sum(((object@demand@h * object@supply@psi * object@rho1 * object@demand@sigma * (object@demand@h * object@rho1 * object@demand@z - 3) + object@supply@sigma * (object@demand@Psi * (object@demand@h**4 + 3) + object@demand@h**2 * (-6 * object@demand@Psi + object@demand@psi * object@supply@z * (object@rho1**2 + 1)) - object@demand@psi * (object@supply@h * object@rho1 * (2 * object@demand@h**2 - 3) + 3 * object@supply@z))) / (4 * object@demand@sigma**5 * object@supply@sigma)) / object@lh)) - t(partial_var_d_of_loglh(object)) %*% partial_var_d_of_loglh(object)
  # nolint end
})

setGeneric("partial_var_d_partial_var_s_of_loglh", function(object) {
  standardGeneric("partial_var_d_partial_var_s_of_loglh")
})

setMethod("partial_var_d_partial_var_s_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (sum((object@rho1 * (object@supply@h * object@demand@psi * object@supply@sigma * (object@demand@h**2 - object@supply@h * object@rho1 * object@supply@z + object@supply@z**2 - 1) + object@supply@psi * object@demand@sigma * (object@demand@h * (object@supply@h**2 - 1) - object@supply@h * object@demand@z * (object@supply@h * object@rho1 - object@supply@z))) / (4 * object@demand@sigma**3 * object@supply@sigma**3)) / object@lh)) - t(partial_var_s_of_loglh(object)) %*% partial_var_d_of_loglh(object)
  # nolint end
})

setGeneric("partial_var_d_partial_rho_of_loglh", function(object) {
  standardGeneric("partial_var_d_partial_rho_of_loglh")
})

setMethod("partial_var_d_partial_rho_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (sum((object@rho1**2 * (-object@demand@psi * object@supply@sigma * (object@rho1 * (object@demand@h + object@supply@h * object@demand@z * object@supply@z) - object@demand@z * (object@demand@h**2 + object@supply@z**2 - 1)) + object@supply@psi * object@demand@sigma * (object@rho1 * (object@demand@h * object@demand@z * object@supply@z + object@supply@h) - object@supply@z)) / (2 * object@demand@sigma**3 * object@supply@sigma)) / object@lh)) - t(partial_rho_of_loglh(object)) %*% partial_var_d_of_loglh(object)
  # nolint end
})

setGeneric("partial_var_s_partial_var_s_of_loglh", function(object) {
  standardGeneric("partial_var_s_partial_var_s_of_loglh")
})

setMethod("partial_var_s_partial_var_s_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (sum(((object@supply@h * object@demand@psi * object@rho1 * object@supply@sigma * (object@supply@h * object@rho1 * object@supply@z - 3) + object@demand@sigma * (object@supply@Psi * (object@supply@h**4 + 3) + object@supply@h**2 * (-6 * object@supply@Psi + object@supply@psi * object@demand@z * (object@rho1**2 + 1)) - object@supply@psi * (object@demand@h * object@rho1 * (2 * object@supply@h**2 - 3) + 3 * object@demand@z))) / (4 * object@demand@sigma * object@supply@sigma**5)) / object@lh)) - t(partial_var_s_of_loglh(object)) %*% partial_var_s_of_loglh(object)
  # nolint end
})

setGeneric("partial_var_s_partial_rho_of_loglh", function(object) {
  standardGeneric("partial_var_s_partial_rho_of_loglh")
})

setMethod("partial_var_s_partial_rho_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (sum((object@rho1**2 * (object@demand@psi * object@supply@sigma * (object@rho1 * (object@demand@h + object@supply@h * object@demand@z * object@supply@z) - object@demand@z) - object@supply@psi * object@demand@sigma * (object@rho1 * (object@demand@h * object@demand@z * object@supply@z + object@supply@h) - object@supply@z * (object@supply@h**2 + object@demand@z**2 - 1))) / (2 * object@demand@sigma * object@supply@sigma**3)) / object@lh)) - t(partial_rho_of_loglh(object)) %*% partial_var_s_of_loglh(object)
  # nolint end
})

setGeneric("partial_rho_partial_rho_of_loglh", function(object) {
  standardGeneric("partial_rho_partial_rho_of_loglh")
})

setMethod("partial_rho_partial_rho_of_loglh", signature(object = "system_basic"), function(object) {
  # nolint start
  (sum((object@rho1**3 * (object@demand@psi * object@supply@sigma * (2 * object@supply@h + object@rho1 * object@supply@z * (object@demand@z**2 - 3)) + object@supply@psi * object@demand@sigma * (2 * object@demand@h + object@rho1 * object@demand@z * (object@supply@z**2 - 3))) / (object@demand@sigma * object@supply@sigma)) / object@lh)) - t(partial_rho_of_loglh(object)) %*% partial_rho_of_loglh(object)
  # nolint end
})
