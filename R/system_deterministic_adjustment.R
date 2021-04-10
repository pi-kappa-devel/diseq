#' @include equation_deterministic_adjustment.R
#' @include system_base.R
setClass(
  "system_deterministic_adjustment",
  contains = "system_base",
  representation(
    gamma = "numeric",
    delta = "numeric",

    mu_P = "matrix",
    var_P = "numeric",
    sigma_P = "numeric",
    h_P = "matrix",

    gr_mu_P = "matrix",
    gr_var_P = "matrix",

    log_likelihood = "matrix",

    lagged_price_vector = "matrix"
  ),
  prototype(
    gamma = NA_real_,
    delta = NA_real_,

    log_likelihood = matrix(NA_real_),
    gradient = matrix(NA_real_),

    lagged_price_vector = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "system_deterministic_adjustment",
  function(
           .Object, quantity, price,
           demand_specification, supply_specification, data, correlated_shocks) {
    price_diff <- paste0(price, "_DIFF")

    demand_initializer <- function(...) {
      excess_supply_subset <- data[, price_diff] < 0
      new("equation_deterministic_adjustment", ..., excess_supply_subset)
    }
    supply_initializer <- function(...) {
      excess_demand_subset <- data[, price_diff] >= 0
      new("equation_deterministic_adjustment", ..., excess_demand_subset)
    }

    .Object <- callNextMethod(
      .Object, quantity, price,
      demand_specification, supply_specification, data, correlated_shocks,
      demand_initializer, supply_initializer
    )
    .Object@lagged_price_vector <- as.matrix(data[, lagged_price_variable(.Object)])

    .Object@sample_separation <- TRUE
    .Object
  }
)

setMethod(
  "likelihood_variables", signature(object = "system_deterministic_adjustment"),
  function(object) {
    likelihood_variables <- callNextMethod(object)

    len <- length(likelihood_variables)
    pos <- len - ifelse(object@correlated_shocks, 3, 2)
    likelihood_variables <- c(
      likelihood_variables[1:pos],
      price_differences_variable(object), likelihood_variables[(pos + 1):len]
    )

    likelihood_variables
  }
)

setMethod(
  "set_parameters", signature(object = "system_deterministic_adjustment"),
  function(object, parameters) {
    object <- callNextMethod(object, parameters)

    object@gamma <- parameters[price_differences_variable(object)]
    object@delta <- object@gamma + object@supply@alpha - object@demand@alpha

    object <- calculate_system_moments(object)

    object
  }
)
