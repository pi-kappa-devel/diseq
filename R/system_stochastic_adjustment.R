#' @include equation_stochastic_adjustment.R
#' @include system_base.R
#' @importFrom stats terms

#' @describeIn system_classes Stochastic adjustment model's system class
#' @slot price_equation Price equation.
#' @slot zeta \deqn{\zeta = \sqrt{1 - \rho_{DS}^2 - \rho_{DP}^2 - \rho_{SP}^2 +
#' 2 \rho_DP \rho_DS \rho_SP}}
#' @slot zeta_DD \deqn{\zeta_{DD} = 1 - \rho_{SP}^2}
#' @slot zeta_DS \deqn{\zeta_{DS} = \rho_{DS} - \rho_{DP}\rho_{SP}}
#' @slot zeta_DP \deqn{\zeta_{DP} = \rho_{DP} - \rho_{DS}\rho_{SP}}
#' @slot zeta_SS \deqn{\zeta_{SS} = 1 - \rho_{DP}^2}
#' @slot zeta_SP \deqn{\zeta_{SP} = \rho_{SP} - \rho_{DS}\rho_{DP}}
#' @slot zeta_PP \deqn{\zeta_{PP} = 1 - \rho_{DS}^2}
#' @slot mu_D \deqn{\mu_{D} = \mathrm{E}D}
#' @slot var_D \deqn{V_{D} = \mathrm{Var}D}
#' @slot sigma_D \deqn{\sigma_{D} = \sqrt{V_{D}}}
#' @slot mu_S \deqn{\mu_{S} = \mathrm{E}S}
#' @slot var_S \deqn{V_{S} = \mathrm{Var}S}
#' @slot sigma_S \deqn{\sigma_{S} = \sqrt{V_{S}}}
#' @slot sigma_DP \deqn{\sigma_{DP} = \mathrm{Cov}(D, P)}
#' @slot sigma_DS \deqn{\sigma_{DS} = \mathrm{Cov}(D, S)}
#' @slot sigma_SP \deqn{\sigma_{SP} = \mathrm{Cov}(S, P)}
#' @slot rho_DS \deqn{\rho_{DS} =
#' \frac{\mathrm{Cov}(D,S)}{\sqrt{\mathrm{Var}D\mathrm{Var}S}}}
#' @slot rho_DP \deqn{\rho_{DP} =
#' \frac{\mathrm{Cov}(D,P)}{\sqrt{\mathrm{Var}D\mathrm{Var}P}}}
#' @slot rho_SP \deqn{\rho_{SP} =
#' \frac{\mathrm{Cov}(S,P)}{\sqrt{\mathrm{Var}S\mathrm{Var}P}}}
#' @slot h_D \deqn{h_{D} = \frac{D - \mu_{D}}{\sigma_{D}}}
#' @slot h_S \deqn{h_{S} = \frac{S - \mu_{S}}{\sigma_{S}}}
#' @slot z_DP \deqn{z_{DP} = \frac{h_{D} - \rho_{DP}h_{P}}{\sqrt{1 - \rho_{DP}^2}}}
#' @slot z_PD \deqn{z_{PD} = \frac{h_{P} - \rho_{PD}h_{D}}{\sqrt{1 - \rho_{PD}^2}}}
#' @slot z_SP \deqn{z_{SP} = \frac{h_{S} - \rho_{SP}h_{P}}{\sqrt{1 - \rho_{SP}^2}}}
#' @slot z_PS \deqn{z_{PS} = \frac{h_{P} - \rho_{PS}h_{S}}{\sqrt{1 - \rho_{PS}^2}}}
#' @slot omega_D \deqn{\omega_{D} = \frac{h_{D}\zeta_{DD} - h_{S}\zeta_{DS} -
#' h_{P}\zeta_{DP}}{\zeta_{DD}}}
#' @slot omega_S \deqn{\omega_{S} = \frac{h_{S}\zeta_{SS} - h_{S}\zeta_{SS} -
#' h_{P}\zeta_{SP}}{\zeta_{SS}}}
#' @slot w_D \deqn{w_{D} = - \frac{h_{D}^2 - 2 h_{D} h_{P} \rho_{DP} +
#' h_{P}^2}{2\zeta_{SS}}}
#' @slot w_S \deqn{w_{S} = - \frac{h_{S}^2 - 2 h_{S} h_{P} \rho_{SP} +
#' h_{P}^2}{2\zeta_{DD}}}
#' @slot psi_D \deqn{\psi_{D} = \phi\left(\frac{\omega_{D}}{\zeta}\right)}
#' @slot psi_S \deqn{\psi_{S} = \phi\left(\frac{\omega_{S}}{\zeta}\right)}
#' @slot Psi_D \deqn{\Psi_{D} = 1 - \Phi\left(\frac{\omega_{D}}{\zeta}\right)}
#' @slot Psi_S \deqn{\Psi_{S} = 1 - \Phi\left(\frac{\omega_{S}}{\zeta}\right)}
#' @slot g_D \deqn{g_{D} = \frac{\psi_{D}}{\Psi_{D}}}
#' @slot g_S \deqn{g_{S} = \frac{\psi_{S}}{\Psi_{S}}}
#' @slot rho_ds Shadows \code{rho} in the \linkS4class{diseq_stochastic_adjustment} model
#' @slot rho_dp Correlation of demand and price equations' shocks.
#' @slot rho_sp Correlation of supply and price equations' shocks.
#' @slot L_D Likelihood conditional on excess supply.
#' @slot L_S Likelihood conditional on excess demand.
setClass(
  "system_stochastic_adjustment",
  contains = "system_base",
  representation(
    price_equation = "equation_base",
    gamma = "numeric",
    delta = "numeric",
    zeta = "numeric",
    zeta_DD = "numeric",
    zeta_DS = "numeric",
    zeta_DP = "numeric",
    zeta_SS = "numeric",
    zeta_SP = "numeric",
    zeta_PP = "numeric",
    mu_P = "matrix",
    var_P = "numeric",
    sigma_P = "numeric",
    mu_D = "matrix",
    var_D = "numeric",
    sigma_D = "numeric",
    mu_S = "matrix",
    var_S = "numeric",
    sigma_S = "numeric",
    sigma_DP = "numeric",
    sigma_DS = "numeric",
    sigma_SP = "numeric",
    rho_DS = "numeric",
    rho_DP = "numeric",
    rho_SP = "numeric",
    h_P = "matrix",
    h_D = "matrix",
    h_S = "matrix",
    z_DP = "matrix",
    z_PD = "matrix",
    z_SP = "matrix",
    z_PS = "matrix",
    omega_D = "matrix",
    omega_S = "matrix",
    w_D = "matrix",
    w_S = "matrix",
    psi_D = "matrix",
    psi_S = "matrix",
    Psi_D = "matrix",
    Psi_S = "matrix",
    g_D = "matrix",
    g_S = "matrix",
    rho_ds = "numeric",
    rho_dp = "numeric",
    rho_sp = "numeric",
    L_D = "matrix",
    L_S = "matrix",
    lagged_price_vector = "matrix"
  ),
  prototype(
    gamma = NA_real_,
    rho_ds = 0,
    rho_dp = 0,
    rho_sp = 0,
    lagged_price_vector = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "system_stochastic_adjustment",
  function(.Object, specification, data, correlated_shocks) {
    demand_initializer <- function(...) {
      new("equation_stochastic_adjustment", ...)
    }
    supply_initializer <- function(...) {
      new("equation_stochastic_adjustment", ...)
    }

    .Object <- callNextMethod(
      .Object, specification, data,
      correlated_shocks, demand_initializer, supply_initializer
    )
    .Object@price_equation <- new(
      "equation_stochastic_adjustment", formula(specification, rhs = 3), data,
      "Price Dynamics", "P_"
    )
    # The standard equation initialization correctly creates the control matrix
    # needed in the models' calculations. We only need to adjust the formula
    # for the `show` and `summary` functions.
    .Object@price_equation@formula <- Formula(formula(paste0(
      price_differences_variable(.Object), " ~ (",
      prefixed_quantity_variable(.Object@demand), " - ",
      prefixed_quantity_variable(.Object@supply), ") + ",
      deparse(terms(specification, lhs = 0, rhs = 3)[[2]])
    )))

    .Object@lagged_price_vector <- as.matrix(data[, lagged_price_variable(.Object)])

    .Object
  }
)

setMethod(
  "show_implementation", signature(object = "system_stochastic_adjustment"),
  function(object) {
    callNextMethod(object)
    show_implementation(object@price_equation)
    cat(sprintf(
      "  %-18s: %s\n", "Short Side Rule", paste0(
        quantity_variable(object@demand), " = min(",
        prefixed_quantity_variable(object@demand), ", ",
        prefixed_quantity_variable(object@supply), ")"
      )
    ))
  }
)

setMethod(
  "likelihood_variables", signature(object = "system_stochastic_adjustment"),
  function(object) {
    likelihood_variables <- callNextMethod(object)

    len <- length(likelihood_variables)
    pos <- len - ifelse(object@correlated_shocks, 3, 2)
    likelihood_variables <- c(
      likelihood_variables[1:pos],
      price_differences_variable(object),
      prefixed_control_variables(object@price_equation),
      likelihood_variables[(pos + 1):len]
    )

    len <- length(likelihood_variables)
    if (object@correlated_shocks) {
      likelihood_variables <- c(
        likelihood_variables[1:(len - 1)],
        prefixed_variance_variable(object@price_equation),
        paste0(likelihood_variables[len], c("_DS", "_DP", "_SP"))
      )
    } else {
      likelihood_variables <- c(
        likelihood_variables,
        prefixed_variance_variable(object@price_equation)
      )
    }

    likelihood_variables
  }
)

setMethod(
  "set_parameters", signature(object = "system_stochastic_adjustment"),
  function(object, parameters) {
    object@demand <- set_parameters(object@demand, parameters)
    object@supply <- set_parameters(object@supply, parameters)
    object@price_equation <- set_parameters(object@price_equation, parameters)
    if (object@correlated_shocks) {
      object@rho_ds <- parameters[paste0(correlation_variable(object), "_DS")]
      object@rho_dp <- parameters[paste0(correlation_variable(object), "_DP")]
      object@rho_sp <- parameters[paste0(correlation_variable(object), "_SP")]
    }
    object@gamma <- parameters[price_differences_variable(object)]
    object@delta <- object@gamma + object@supply@alpha - object@demand@alpha

    object <- calculate_system_moments(object)

    object
  }
)
