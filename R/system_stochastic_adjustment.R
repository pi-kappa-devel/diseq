#' @include equation_stochastic_adjustment.R
#' @include system_base.R
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
  function(
           .Object, quantity, price,
           demand_specification, supply_specification, price_specification,
           data, correlated_shocks) {
    demand_initializer <- function(...) {
      new("equation_stochastic_adjustment", ...)
    }
    supply_initializer <- function(...) {
      new("equation_stochastic_adjustment", ...)
    }

    .Object <- callNextMethod(
      .Object, quantity, price, demand_specification, supply_specification, data, correlated_shocks,
      demand_initializer, supply_initializer
    )
    .Object@price_equation <- new(
      "equation_stochastic_adjustment", quantity, price, price_specification, data,
      "Price Equation", "P_"
    )
    .Object@lagged_price_vector <- as.matrix(data[, lagged_price_variable(.Object)])

    .Object
  }
)

setMethod(
  "show_implementation", signature(object = "system_stochastic_adjustment"),
  function(object) {
    callNextMethod(object)
    show_implementation(object@price_equation)
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
      price_differences_variable(object), prefixed_control_variables(object@price_equation),
      likelihood_variables[(pos + 1):len]
    )

    len <- length(likelihood_variables)
    if (object@correlated_shocks) {
      likelihood_variables <- c(
        likelihood_variables[1:(len - 1)],
        prefixed_variance_variable(object@price_equation),
        paste0(likelihood_variables[len], c("_DS", "_DP", "_SP"))
      )
    }
    else {
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
