#' @include equation_base.R
setClass(
  "system_base",
  representation(
    demand = "equation_base",
    supply = "equation_base",
    correlated_shocks = "logical",

    quantity_variable = "character",
    price_variable = "character",

    quantity_vector = "matrix",
    price_vector = "matrix",

    rho = "numeric",
    rho1 = "numeric",
    rho2 = "numeric"
  ),
  prototype(
    demand = NULL,
    supply = NULL,

    quantity_variable = NULL,
    price_variable = NULL,

    quantity_vector = matrix(NA_real_),
    price_vector = matrix(NA_real_),

    rho = 0,
    rho1 = 1,
    rho2 = 0
  )
)

setMethod(
  "initialize", "system_base",
  function(
           .Object, quantity, price,
           demand_specification, supply_specification, data, correlated_shocks,
           demand_initializer, supply_initializer) {
    .Object@demand <- demand_initializer(
      quantity, price,
      demand_specification, data, "Demand Equation", "D_"
    )
    .Object@supply <- supply_initializer(
      quantity, price,
      supply_specification, data, "Supply Equation", "S_"
    )
    .Object@correlated_shocks <- correlated_shocks

    .Object@quantity_variable <- quantity
    .Object@price_variable <- price

    .Object@quantity_vector <- as.matrix(data[, quantity])
    .Object@price_vector <- as.matrix(data[, price])

    .Object
  }
)

setGeneric("get_lagged_price_variable", function(object) {
  standardGeneric("get_lagged_price_variable")
})

setGeneric("get_price_differences_variable", function(object) {
  standardGeneric("get_price_differences_variable")
})

setGeneric("get_correlation_variable", function(object) {
  standardGeneric("get_correlation_variable")
})

setGeneric("get_likelihood_variables", function(object) {
  standardGeneric("get_likelihood_variables")
})

setGeneric("calculate_system_moments", function(object) {
  standardGeneric("calculate_system_moments")
})

setGeneric("calculate_system_loglikelihood", function(object) {
  standardGeneric("calculate_system_loglikelihood")
})

setGeneric("calculate_system_gradient", function(object) {
  standardGeneric("calculate_system_gradient")
})

setGeneric("calculate_system_scores", function(object) {
  standardGeneric("calculate_system_scores")
})

setMethod("get_lagged_price_variable", signature(object = "system_base"), function(object) {
  paste0("LAGGED_", object@demand@price_variable)
})

setMethod("get_price_differences_variable", signature(object = "system_base"), function(object) {
  paste0(object@price_variable, "_DIFF")
})

setMethod("get_correlation_variable", signature(object = "system_base"), function(object) {
  "RHO"
})

setGeneric("calculate_llh", function(object) {
  standardGeneric("calculate_llh")
})

setMethod("get_likelihood_variables", signature(object = "system_base"), function(object) {
  likelihood_variables <- c(
    get_prefixed_price_variable(object@demand),
    get_prefixed_control_variables(object@demand),
    get_prefixed_price_variable(object@supply),
    get_prefixed_control_variables(object@supply),
    get_prefixed_variance_variable(object@demand),
    get_prefixed_variance_variable(object@supply)
  )

  if (object@correlated_shocks) {
    likelihood_variables <- c(likelihood_variables, get_correlation_variable(object))
  }

  likelihood_variables
})


setMethod("set_parameters", signature(object = "system_base"), function(object, parameters) {
  object@demand <- set_parameters(object@demand, parameters)
  object@supply <- set_parameters(object@supply, parameters)
  if (object@correlated_shocks) {
    object@rho <- parameters[get_correlation_variable(object)]
    object@rho <- ifelse(abs(object@rho) > 1, NA_real_, object@rho)
    object@rho1 <- 1 / sqrt(1 - object@rho**2)
    object@rho2 <- object@rho * object@rho1
  }
  object
})
