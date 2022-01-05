#' @include equation_deterministic_adjustment.R
#' @include system_base.R

#' @describeIn system_classes Deterministic adjustment model's system class
#' @slot gamma Excess demand coefficient.
#' @slot delta \deqn{\delta = \gamma + \alpha_{d} - \alpha_{s}}
#' @slot mu_P \deqn{\mu_{P} = \mathrm{E}P}
#' @slot var_P \deqn{V_{P} = \mathrm{Var}P}
#' @slot sigma_P \deqn{\sigma_{P} = \sqrt{V_{P}}}
#' @slot h_P \deqn{h_{P} = \frac{P - \mu_{P}}{\sigma_{P}}}
#' @slot lagged_price_vector A vector with the system's observed prices lagged by
#' one date.
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
    lagged_price_vector = "matrix"
  ),
  prototype(
    gamma = NA_real_,
    delta = NA_real_,
    lagged_price_vector = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "system_deterministic_adjustment",
  function(.Object, specification, data, correlated_shocks) {
    price_diff <- paste0(
      all.vars(formula(specification, lhs = 2, rhs = 0)),
      "_DIFF"
    )

    demand_initializer <- function(...) {
      excess_supply_subset <- data[, price_diff] < 0
      new("equation_deterministic_adjustment", ..., excess_supply_subset)
    }
    supply_initializer <- function(...) {
      excess_demand_subset <- data[, price_diff] >= 0
      new("equation_deterministic_adjustment", ..., excess_demand_subset)
    }

    .Object <- callNextMethod(
      .Object, specification, data, correlated_shocks,
      demand_initializer, supply_initializer
    )
    .Object@lagged_price_vector <- as.matrix(data[, lagged_price_variable(.Object)])

    .Object@sample_separation <- TRUE
    .Object
  }
)

setMethod(
  "show_implementation", signature(object = "system_deterministic_adjustment"),
  function(object) {
    callNextMethod(object)
    cat(sprintf(
      "  %-18s: %s\n  %-18s: %s\n", "Short Side Rule", paste0(
        quantity_variable(object@demand), " = min(",
        prefixed_quantity_variable(object@demand), ", ",
        prefixed_quantity_variable(object@supply), ")"
      ),
      "Separation Rule", paste0(
        price_differences_variable(object), " analogous to (",
        prefixed_quantity_variable(object@demand), " - ",
        prefixed_quantity_variable(object@supply), ")"
      )
    ))
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
