#' @include equation_basic.R
#' @include system_base.R

#' @importFrom Rcpp loadModule
#' @export cpp_eq_fiml_impl
Rcpp::loadModule("diseq_module", TRUE)

#' @export gsl_eq_fiml_impl
Rcpp::loadModule("diseq_module_2", TRUE)

setClass(
  "cpp_system_fiml",
  contains = "system_base",
  representation(
    delta = "numeric",

    mu_P = "matrix",
    var_P = "numeric",
    sigma_P = "numeric",
    h_P = "matrix",

    mu_Q = "matrix",
    var_Q = "numeric",
    sigma_Q = "numeric",
    h_Q = "matrix",

    cov_QP = "numeric",
    rho_QP = "numeric",
    rho1_QP = "numeric",
    rho2_QP = "numeric",

    z_QP = "matrix",
    z_PQ = "matrix",

    llh = "matrix",

    set_parameters = "numeric",
    eq_fiml_cpp_impl = "Rcpp_cpp_eq_fiml_impl",
    eq_fiml_gsl_impl = "Rcpp_gsl_eq_fiml_impl"
  ),
  prototype(
    llh = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "cpp_system_fiml",
  function(
           .Object, quantity, price,
           demand_specification, supply_specification, data, correlated_shocks,
           demand_initializer = NULL, supply_initializer = NULL) {
    .Object <- callNextMethod(
      .Object, quantity, price, demand_specification, supply_specification, data, correlated_shocks,
      ifelse(is.null(demand_initializer),
        function(...) new("equation_basic", ...), demand_initializer
      ),
      ifelse(is.null(supply_initializer),
        function(...) new("equation_basic", ...), supply_initializer
      )
    )

    .Object@set_parameters <- c(0)
    .Object@eq_fiml_cpp_impl <- new(cpp_eq_fiml_impl, .Object)
    .Object@eq_fiml_gsl_impl <- new(gsl_eq_fiml_impl, .Object)

    .Object
  }
)

setMethod("calculate_system_moments", signature(object = "cpp_system_fiml"), function(object) {
  object@mu_P <-
    (
      (
        object@demand@control_matrix %*% object@demand@beta - object@supply@control_matrix %*%
          object@supply@beta
      ) / (-object@demand@alpha + object@supply@alpha)
    )
  object@var_P <-
    (
      (
        -2 * object@rho * object@demand@sigma * object@supply@sigma + object@demand@sigma**2 +
          object@supply@sigma**2
      ) / (-object@demand@alpha + object@supply@alpha)**2
    )
  object@mu_Q <-
    (
      (
        object@demand@control_matrix %*% object@demand@beta * object@supply@alpha -
          object@supply@control_matrix %*% object@supply@beta * object@demand@alpha
      ) / (-object@demand@alpha +
        object@supply@alpha)
    )
  object@var_Q <-
    (
      (
        object@demand@alpha**2 * object@supply@sigma**2 - 2 * object@demand@alpha *
          object@supply@alpha * object@rho * object@demand@sigma * object@supply@sigma +
          object@supply@alpha**2 *
            object@demand@sigma**2
      ) / (-object@demand@alpha + object@supply@alpha)**2
    )
  object@cov_QP <-
    (
      (
        object@demand@alpha * object@supply@sigma**2 + object@supply@alpha * object@demand@sigma
        **2 - object@rho * object@demand@sigma * object@supply@sigma * (object@demand@alpha +
          object@supply@alpha)
      ) /
        (-object@demand@alpha + object@supply@alpha)**2
    )

  object@sigma_P <- sqrt(object@var_P)
  object@sigma_Q <- sqrt(object@var_Q)

  object@h_P <- (object@price_vector - object@mu_P) / object@sigma_P
  object@h_Q <- (object@quantity_vector - object@mu_Q) / object@sigma_Q

  object@rho_QP <- object@cov_QP / (object@sigma_P * object@sigma_Q)
  if (is.na(object@rho_QP) || abs(object@rho_QP) >= 1) {
    object@rho_QP <- NA_real_
  }
  object@rho1_QP <- 1 / sqrt(1 - object@rho_QP**2)
  object@rho2_QP <- object@rho_QP * object@rho1_QP

  object@z_PQ <- object@rho1_QP * object@h_P - object@rho2_QP * object@h_Q
  object@z_QP <- object@rho1_QP * object@h_Q - object@rho2_QP * object@h_P

  object
})

setMethod("calculate_system_loglikelihood", signature(object = "cpp_system_fiml"), function(object) {
  -log(2 * pi) - log(object@sigma_P * object@sigma_Q / object@rho1_QP) - (
    object@rho1_QP**2 * (
      object@h_P**2 - 2 * object@h_P * object@h_Q * object@rho_QP + object@h_Q**2
    )
  ) / 2
})

setMethod("set_parameters", signature(object = "cpp_system_fiml"), function(object, parameters) {
  object <- object@eq_fiml_cpp_impl$system_eq_fiml_set_parameters(object, parameters)
  #z_PQ <- object@z_PQ
  #object <- calculate_system_moments(object)
                                        #print(all(abs(z_PQ - object@z_PQ)<1e-10))
    
  #object@llh <- calculate_system_loglikelihood(object)

  object
})
