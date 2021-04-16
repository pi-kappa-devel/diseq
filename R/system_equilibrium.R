#' @include equation_basic.R
#' @include system_base.R

#' @describeIn system_classes Equilibrium model's system class
#' @slot mu_Q \deqn{\mu_{Q} = \mathrm{E}Q}
#' @slot var_Q \deqn{V_{Q} = \mathrm{Var}Q}
#' @slot sigma_Q \deqn{\sigma_{Q} = \sqrt{V_{Q}}}
#' @slot h_Q \deqn{h_{Q} = \frac{Q - \mu_{Q}}{\sigma_{Q}}}
#' @slot rho_QP \deqn{\rho_{QP} =
#' \frac{\mathrm{Cov}(Q,P)}{\sqrt{\mathrm{Var}Q\mathrm{Var}P}}}
#' @slot rho_1QP \deqn{\rho_{1,QP} = \frac{1}{\sqrt{1 - \rho_{QP}^2}}}
#' @slot rho_2QP \deqn{\rho_{2,QP} = \rho_{QP}\rho_{1,QP}}
#' @slot z_QP \deqn{z_{QP} = \frac{h_{Q} - \rho_{QP}h_{P}}{\sqrt{1 - \rho_{QP}^2}}}
#' @slot z_PQ \deqn{z_{PQ} = \frac{h_{P} - \rho_{PQ}h_{Q}}{\sqrt{1 - \rho_{PQ}^2}}}
setClass(
  "system_equilibrium",
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

    rho_QP = "numeric",
    rho_1QP = "numeric",
    rho_2QP = "numeric",

    z_QP = "matrix",
    z_PQ = "matrix",

    llh = "matrix"
  ),
  prototype(
    llh = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "system_equilibrium",
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
  }
)

setMethod("set_parameters", signature(object = "system_equilibrium"),
          function(object, parameters) {
  object <- callNextMethod(object, parameters)

  object@delta <- object@supply@alpha - object@demand@alpha
  object <- calculate_system_moments(object)
  object@llh <- calculate_system_loglikelihood(object)

  object
})
